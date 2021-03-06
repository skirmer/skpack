% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crossvalid_function.R
\name{looping_cv_shell}
\alias{looping_cv_shell}
\title{GBM Cross Validation Function}
\usage{
looping_cv_shell(nfold = 5, colsamplebt = 1, eval_metric = "rmse",
  nthread = 2, splitpct = 0.8)
}
\arguments{
\item{No}{actual inputs, just make sure your dataset containing all desired variables is located in the gloval environment titled final_data_dt. And make sure it's a data.table.}
}
\description{
Notes: the "looping_cv_shell" function runs the entire cross validation procedure. Steps before you run:
1. Check that the params are what you want- if you don't need to test all the variations, then by all means set single values.
2. This runs all the outcomes currently available - if more or different have been built, change outcome_list and omit_list to reflect.
3. The metrics produced are: AUC, RMSE, Brier Score, Log Loss, Avg Precision at 100, Avg Precision at 500, and Avg Precision at 1000. If you
want others, add them at the end and ensure that the results output c(), label set, and the unlist statements are adjusted accordingly.
}
\examples{
cv_test <- looping_cv_shell()
cv_test1 <- data.frame(matrix(unlist(cv_test), ncol = 12, byrow=T), stringsAsFactors = FALSE)
cv_test1 <- rbind(cv_test1, data.frame(matrix(unlist(looping_cv_shell()), ncol = 12, byrow=T), stringsAsFactors = FALSE))
colnames(cv_test1) <- c("AUC", "RMSE", "Brier","Log Loss","Avg Precision at 100","Avg Precision at 500",
"Avg Precision at 1000","Outcome", "Trees", "Max Depth", "Eta", "Subsample")
}
