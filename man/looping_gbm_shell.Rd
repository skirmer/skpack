% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gbm_model_function.R
\name{looping_gbm_shell}
\alias{looping_gbm_shell}
\title{GBM Modeling Function}
\usage{
looping_gbm_shell(eta = 0.05, nfold = 5, nrounds = 100, colsamplebt = 1,
  subsample = 1, max_depth = 3, eval_metric = "rmse", nthread = 2,
  splitpct = 0.8)
}
\arguments{
\item{No}{actual inputs, just make sure your dataset containing all desired variables is located in the gloval environment titled final_data_dt. And make sure it's a data.table.}
}
\description{
This function takes in the SSL source dataset, runs the XGBoost algorithm on every outcome variable, produces several evaluation metrics
for each model, and outputs that data in a list.
}
\details{
If you want different results (the trees are currently maximizing RMSE but that can change), adjust what you
pass in. For example, removing unnecessary covariates before putting the dataset into the model can save a lot of time and perhaps improve results.
Other things you can do: set a seed inside the function (commented out currently), change all the xgb params, change the training split, etc.
Function also produces a plot of prediction x ground truth with loess line overlay, and saves this to the ssl/models/output folder.
}
\examples{
testing <- looping_outcomes_shell()
testing1 <- data.frame(matrix(unlist(testing), ncol = 9, byrow=T), stringsAsFactors = FALSE)
testing1 <- rbind(testing1, data.frame(matrix(unlist(looping_outcomes_shell()), ncol = 9, byrow=T), stringsAsFactors = FALSE))
colnames(testing1) <- c("AUC", "RMSE", "Brier","LogLoss","Avg Precision at 100","Avg Precision at 500",
"Avg Precision at 1000","Avg Precision at 5000","Outcome")
}
