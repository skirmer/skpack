#' GBM Cross Validation Function
#'
#' Notes: the "looping_cv_shell" function runs the entire cross validation procedure. Steps before you run:
#' 1. Check that the params are what you want- if you don't need to test all the variations, then by all means set single values.
#' 2. This runs all the outcomes currently available - if more or different have been built, change outcome_list and omit_list to reflect.
#' 3. The metrics produced are: AUC, RMSE, Brier Score, Log Loss, Avg Precision at 100, Avg Precision at 500, and Avg Precision at 1000. If you
#' want others, add them at the end and ensure that the results output c(), label set, and the unlist statements are adjusted accordingly.
#'
#' @param No actual inputs, just make sure your dataset containing all desired variables is located in the gloval environment titled final_data_dt. And make sure it's a data.table.
#' @export
#' @examples
#' cv_test <- looping_cv_shell()
#' cv_test1 <- data.frame(matrix(unlist(cv_test), ncol = 12, byrow=T), stringsAsFactors = FALSE)
#' cv_test1 <- rbind(cv_test1, data.frame(matrix(unlist(looping_cv_shell()), ncol = 12, byrow=T), stringsAsFactors = FALSE))
#' colnames(cv_test1) <- c("AUC", "RMSE", "Brier","Log Loss","Avg Precision at 100","Avg Precision at 500",
#' "Avg Precision at 1000","Outcome", "Trees", "Max Depth", "Eta", "Subsample")


looping_cv_shell <- function(nfold = 5, colsamplebt = 1, eval_metric = "rmse", nthread = 2, splitpct = .8){
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

  #Set the parameters you are trying to test
  params <- expand.grid(nrounds=c(100),
                        max_depth=c(3),
                        eta=c(.05),
                        subsample=c(1))


  results <- list()

  for(n in 1:nrow(outcome_list)){
    outcome <- outcome_list[n,V1]
    varomit <- omit_list[V1 != outcome]

    #the outcome and variables to omit get reused repeatedly from here down.
    out_finaldt <- eval(parse(text=paste0("final_data_dt$",outcome))) #creating an input.


    ##Partition first ####
    #set.seed(1234) #Uncomment if you want replicability. However then don't run the function more than once, obvs.
    trainIndex <- createDataPartition(out_finaldt,p= splitpct ,list=FALSE) #Currently at 80/20 but change if you want.
    trainData <<- final_data_dt[trainIndex,]
    testData <<- final_data_dt[-trainIndex,]

    out_train1 <- eval(parse(text=paste0("trainData$", outcome))) #creating an input.
    modelform1 <- formula(paste0(outcome,"~.")) #creating an input formula.

    # =============================================================
    # XGBoost time
    # =============================================================
    # Create a sparse matrix from a data frame ####
    # All values must be numeric- inside a matrix everything has to be the same type.

    #Create matrices from the data frames
    trainData2<- as.matrix(trainData, rownames.force=NA)
    testData2<- as.matrix(testData, rownames.force=NA)

    #Turn the matrices into sparse matrices
    train <- as(trainData2, "sparseMatrix")
    test <- as(testData2, "sparseMatrix")

    #Basic set - for this, you need to be sure that neither the outcome nor the omitted variables are in here.
    trainData_test <- trainData
    for(var in varomit) trainData_test[, (var):= NULL]#, with=FALSE]
    for(var in outcome) trainData_test[, (var):= NULL]#, with=FALSE]
    varlist <- names(trainData_test)

    trainD <- xgb.DMatrix(data = train[,varlist], label = train[,outcome]) #Convert to xgb.DMatrix format

    # ================================================================== #
    # Crossvalidate the model
    # ================================================================== #

    cv.model <- sapply(1:nrow(params),function(n){

      trees <- params[n,1]
      maxdepth <- params[n,2]
      eta2 <- params[n,3]
      subsample <- params[n,4]

#       for(m in 1:params[n,1]){ #Use this if you want a tree-by-tree dataset for plotting- remember to add end curly brace
#         print(m)
      predictions <-  xgb.cv(data = trainD,
             nrounds = trees,
             min_child_weight = .5,
             max_depth = maxdepth,
             eta = eta2,
             subsample = subsample,
             colsample_bytree = colsamplebt,
             booster = "gbtree",
             eval_metric = eval_metric,
             verbose = 0,
             nfold = nfold,
             nthread = nthread,
             prediction = TRUE,
             objective="binary:logistic")

      #Column names must match the inputs EXACTLY
      prediction <- predictions$pred

      #Put together the prediction value in a tidy way.
      test3 <- as.data.frame(as.matrix(train))
      prediction <- as.data.frame(as.matrix(prediction))
      colnames(prediction) <- "prediction"

      #Put the prediction and the original test data together in a tidy way.
      model_output <- cbind(test3, prediction)

      print("Predicted Yes:")
      print(length(model_output$prediction[model_output$prediction >= 0.5]))

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

      #Produce Average Precision at K
      model_output_sort <- model_output[order(-model_output$prediction),]
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

      singleround <- c(auc2, final_rmse, brier,ll_value, ppv_100, ppv_500,ppv_1000, outcome, trees,
                       maxdepth, eta2, subsample) #Put everything together for the output.
      print(singleround)
      singleround
    })
    results[[n]] <- cv.model
    print(results)
  }
  results
}



