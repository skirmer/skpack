# Gradient Boosting Model for SSL
# Stephanie Kirmer
#' GBM Modeling Function
#'
#' This function takes in the SSL source dataset, runs the XGBoost algorithm on every outcome variable, produces several evaluation metrics
#' for each model, and outputs that data in a list.
#'
#' If you want different results (the trees are currently maximizing RMSE but that can change), adjust what you
#' pass in. For example, removing unnecessary covariates before putting the dataset into the model can save a lot of time and perhaps improve results.
#' Other things you can do: set a seed inside the function (commented out currently), change all the xgb params, change the training split, etc.
#' Function also produces a plot of prediction x ground truth with loess line overlay, and saves this to the ssl/models/output folder.
#' @param No actual inputs, just make sure your dataset containing all desired variables is located in the gloval environment titled final_data_dt. And make sure it's a data.table.
#' @export
#' @examples
#' testing <- looping_outcomes_shell()
#' testing1 <- data.frame(matrix(unlist(testing), ncol = 9, byrow=T), stringsAsFactors = FALSE)
#' testing1 <- rbind(testing1, data.frame(matrix(unlist(looping_outcomes_shell()), ncol = 9, byrow=T), stringsAsFactors = FALSE))
#' colnames(testing1) <- c("AUC", "RMSE", "Brier","LogLoss","Avg Precision at 100","Avg Precision at 500",
#' "Avg Precision at 1000","Avg Precision at 5000","Outcome")

looping_gbm_shell <- function(eta = 0.05, nfold = 5, nrounds = 100, colsamplebt = 1, subsample = 1, max_depth = 3,
                                   eval_metric = "rmse", nthread = 2, splitpct = .8){

  #if we add more outcomes to the dataset, those should be represented in the following two tables.
  outcome_list <- as.data.table(c("shooting_involved2_OV","violence_involved_OV",
                                  "shooting_victim_OV","shooting_offender_OV",
                                  "shooting_victim2_OV","shooting_offender2_OV",
                                  "violence_victim_OV","violence_offender_OV", "shooting_involved_OV"))

  #Kludgy but adding an item to a table column is obnoxious.
  omit_list <- as.data.table(c("cluster","shooting_involved2_OV","violence_involved_OV",
                               "shooting_victim_OV","shooting_offender_OV",
                               "shooting_victim2_OV","shooting_offender2_OV",
                               "violence_victim_OV","violence_offender_OV", "shooting_involved_OV"))


  results <- list()

  for(n in 1:nrow(outcome_list)){
    outcome <- outcome_list[n,V1]
    varomit <- omit_list[V1 != outcome]

      #the outcome and variables to omit get reused repeatedly from here down.
      out_finaldt <- eval(parse(text=paste0("final_data_dt$",outcome))) #creating an input.

      ##Partition first ####
      #set.seed(1234) #Uncomment if you want replicability. However then don't run the function more than once, obvs.
      trainIndex <- createDataPartition(out_finaldt,p= splitpct ,list=FALSE)
      trainData <- final_data_dt[trainIndex,]
      testData <- final_data_dt[-trainIndex,]

      out_train1 <- eval(parse(text=paste0("trainData$", outcome))) #creating an input.
      modelform1 <- formula(paste0(outcome,"~.")) #creating an input formula.

      # =============================================================
      # XGBoost time
      # =============================================================
      # Create a sparse matrix from a data frame ####
      #All values must be numeric- inside a matrix everything has to be the same type.

      #Create matrices from the data frames
      trainData2<- as.matrix(trainData, rownames.force=NA)
      testData2<- as.matrix(testData, rownames.force=NA)

      #Turn the matrices into sparse matrices
      train <- as(trainData2, "sparseMatrix")
      test <- as(testData2, "sparseMatrix")

      #Basic set - for this, you need to be sure that neither the outcome nor the omitted variables are in here.
      trainData_test <- trainData
      for(var in varomit) trainData_test[, (var):= NULL]
      for(var in outcome) trainData_test[, (var):= NULL]
      varlist <- names(trainData_test)

      trainD <<- xgb.DMatrix(data = train[,varlist], label = train[,outcome]) #Convert to xgb.DMatrix format

      # ================================================================== #
      # Train the model
      # ================================================================== #

      #Choose the parameters for the model
      param <- list(colsample_bytree = colsamplebt,
                    subsample = subsample,
                    booster = "gbtree",
                    max_depth = max_depth,
                    eta = eta,
                    eval_metric = eval_metric,
                    objective="binary:logistic")


      #Train the model using those parameters
      bstSparse <-
        xgb.train(params = param,
                  data = trainD,
                  nrounds = nrounds,
                  #watchlist = list(train = trainD),
                  verbose = 0,
                  nfold = nfold,
                  nthread = nthread)


      testD <- xgb.DMatrix(data = test[,varlist])

      #Column names must match the inputs EXACTLY
      prediction <- predict(bstSparse, testD)

      #Put together the prediction value in a tidy way.
      test3 <- as.data.frame(as.matrix(test))
      prediction <- as.data.frame(as.matrix(prediction))
      colnames(prediction) <- "prediction"

      #Put the prediction and the original test data together in a tidy way.
      model_output <- cbind(test3, prediction)

      print("Predicted Yes:")
      print(length(model_output$prediction[model_output$prediction >= 0.3]))

      out_model2 <- eval(parse(text=paste0("model_output$", outcome)))#creating an input.

      #Draw the ROC curve, get the AUC
      auc2 <- AUC::auc(AUC::roc(model_output$prediction, factor(out_model2)))
      print("ROC AUC:")
      print(auc2)

      #Run the RMSE
      final_rmse <- rmse(out_model2,model_output$prediction)
      print("RMSE:")
      print(final_rmse)

      #Produce Brier Score
      brier <- mean((out_model2 - model_output$prediction)^2)
      print("Brier Score:")
      print(brier)
      #http://stats.stackexchange.com/questions/172945/rmse-root-mean-squared-error-for-logistic-models

      #Produce Average Precision at K
      model_output_sort <<- model_output[order(-model_output$prediction),]
      model_output_sort$binary_predict <- ifelse(model_output_sort$prediction >= 0.3, 1, 0)
      print(head(model_output_sort[,c("prediction", "binary_predict",outcome)]))

      ppv_100 <- ppv_fun(model_output_sort, eval(outcome), "prediction", 100)
      ppv_500 <- ppv_fun(model_output_sort, eval(outcome), "prediction", 500)
      ppv_1000 <- ppv_fun(model_output_sort, eval(outcome), "prediction", 1000)
      ppv_5000 <- ppv_fun(model_output_sort, eval(outcome), "prediction", 5000)

      print("Mean Avg Precision at 500:")
      print(ppv_500)

      #produce logloss
      ll_value <- Metrics::logLoss(out_model2,model_output$prediction)
      print("Log Loss:")
      print(ll_value)

      #Loess Plot
      predict_and_truth <- cbind(out_model2, prediction)
      pandt_2 <- predict_and_truth[order(-predict_and_truth$prediction),]
      pandt_3 <- pandt_2[1:1000,]
      plot_loess <- ggplot(data=pandt_3, aes(x=prediction, y=out_model2))+
        theme_bw()+
        geom_jitter()+
        stat_smooth(formula = y~x, method="loess")+
        labs(title=paste("Prediction x Ground Truth, Top 1000, Outcome=", outcome))
      print(plot_loess)

      plotname <- paste0("/export/projects/machine_learning/ssl/models/output/loessplot_jit", outcome, n, ".jpg")
      jpeg(plotname, width=600, height = 500)
      print(plot_loess)
      dev.off()

      #Uncomment below to save each model to disk when ready - can add other details to the model title if desired
      #xgb.save(model=bstSparse, fname=paste0("experimental_model2_", outcome))

      results[[n]] <- c(auc2, final_rmse, brier, ll_value, ppv_100, ppv_500,ppv_1000, ppv_5000, outcome)
      #Put everything together for the output.
      print(results)
      results
      }
  results
  }



