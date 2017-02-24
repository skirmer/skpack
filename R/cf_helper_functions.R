#' Causal Forest Plotting
#'
#' This function takes in a dataframe with features and an outcome. It produces predictions for a test set, and plots them against the features.
#' @param df is your dataframe
#' @param y is the outcome being predicted.
#' @param x_var is the specific feature to plot against that outcome
#' @param exclude takes any variables you want to omit from the sample.
#' @param split_r is the training/test proportion split you want. Defaults to .9.
#' @param seed_1 and those that follow allow you to see the random partition for reproducibility.
#' @export
#' @examples
#' causal_forest_plots()


causal_forest_plots <- function(df, y, x_var, exclude = NULL, split_r = 0.9, seed_1 = 24010, seed_2 = 39028, seed_3 = 45829) {

  set.seed(seed_1)
  train_ind <- sample.split(df$dmatch, SplitRatio = split_r) #divide to create training and prediction samples
  df_train  <- df[train_ind,]
  df_pred   <- df[!train_ind,]

  # Make these parameters of get_plots
  sample.size <- floor(nrow(df_train)/10)
  cv.option <- "matching"
  num.trees <- 500

  set.seed(seed_2)
  forest <- causalForest(X = df_train[, setdiff(names(df_train), exclude)], Y = df_train[, y], W = df_train$dmatch,
                         num.trees, sample.size, cv.option, verbose = FALSE, nodesize = 5) #Create the forest

  set.seed(seed_3)
  predictions_ci <- randomForestInfJack(forest, df_pred, calibrate = TRUE) #Predict using the forest

  df_pred <- cbind(df_pred, predictions_ci) #join predictions with covariates

  current_plot <- function(cov) {
    ggplot(df_pred, aes(x = cov, y = y.hat)) +
      theme_tufte()+
      theme(panel.background=element_rect(fill="white", color="black"), # Set up the background
            text=element_text(family="Times"), # Default font is serif
            plot.title=element_text(size=rel(1.5), family="Times"), # Make the title a bit bigger than other text
            axis.text.y=element_text(size=14), # Y axis text size
            axis.text.x=element_text(size=14), # X axis text size
            axis.title.y=element_text(size=16), # Y axis title text size
            axis.title.x=element_text(size=16), # X axis title text size
            legend.title=element_text(size=14), # Legend title text size
            legend.text=element_text(size=13), # Legend text size
            legend.justification=c(1,0), #Legend location pt 1
            legend.position=c(1,0), # Legend location pt 2
            legend.background=element_rect(color="black"))+ # Outline the legend
      geom_errorbar(aes(x = cov, ymin = y.hat - 1.96*sqrt(var.hat), ymax = y.hat + 1.96*sqrt(var.hat)), color="black") +
      geom_point(size=3) +
      geom_smooth(method = "auto", se = FALSE, color = '#969696') +
      labs(x = x_var, y = 'Predicted Treatment Effect',
           title= paste0('Predicted treatment effects on ', y, ' vs ', x_var, ' with regression line \nand estimated standard errors for each point'))
  }

  x_tau <- current_plot(cov = df_pred[,x_var])

  train_reg <- lm(df_train[, y] ~ . , data = df_train[, setdiff(names(df), y)])
  pred <- predict(train_reg, df_pred)
  df_pred <- cbind(df_pred, pred)

  current_plot2 <- function(outcome) {
    ggplot(df_pred, aes(x = pred, y = y.hat)) +
      theme_tufte()+
      theme(panel.background=element_rect(fill="white", color="black"), # Set up the background
            text=element_text(family="Times"), # Default font is serif
            plot.title=element_text(size=rel(1.5), family="Times"), # Make the title a bit bigger than other text
            axis.text.y=element_text(size=14), # Y axis text size
            axis.text.x=element_text(size=14), # X axis text size
            axis.title.y=element_text(size=16), # Y axis title text size
            axis.title.x=element_text(size=16), # X axis title text size
            legend.title=element_text(size=14), # Legend title text size
            legend.text=element_text(size=13), # Legend text size
            legend.justification=c(1,0), #Legend location pt 1
            legend.position=c(1,0), # Legend location pt 2
            legend.background=element_rect(color="black"))+ # Outline the legend
    geom_point(stroke=.5, color="black", pch=16, size=3)+
      geom_smooth(method = "auto", se = FALSE, color = '#969696') +
      labs(x = outcome, y = 'Predicted Treatment Effect', title= paste0('Predicted Treatment Effects on ', y, ' vs \nRegression Model Prediction of ', outcome, '\n'))
  }
  #y_tau <- ggMarginal(current_plot2(outcome=y), type = "boxplot", size=18)
  y_tau <- current_plot2(outcome=y)

  list(x_tau, y_tau, df_pred)
}

# Run a regression using df_train. Then plot tau-hat on y vs. y-hat on x.


