#' ITT and TOT Model Function
#'
#' This function computes multiple ITT and TOT models at once. Produces robust standard errors.
#' @param sourcefile is your dataframe, should include a variable for treatment assignment and whatever demographics.
#' @param I is the outcome being predicted.
#' @param varlist is a vector of your covariates (things you are controlling for)- all should be the names of columns in your dataset.
#' @param assignvar is the specific column that specifies the treatment/control split in your sample. Also, your blocking variable should be "blocknum".
#' @param treatvar is the column specifying actual treatment dosage regardless of assignment.
#' @export
#' @examples
#' itt_tot_model()

itt_tot_model <- function(sourcefile, I, varlist, assignvar, treatvar, weight=NULL) {
  require("sandwich")
  require("lmtest")
  require("arm")

  vars_split <- paste(varlist, collapse="+")
  itt_form <- as.formula(paste0(I, "~", assignvar, "+", vars_split, "+ factor(blocknum)"))

  #First do the ITT
  model <- lm(itt_form, weight=weight, data=sourcefile)

  J <- as.matrix(I)
  summaryofols_nr<- summary(model)

#Robust Standard Errors
  model$newse<-vcovHC(model, type="HC1")
  summaryofols <- coeftest(model,model$newse)

  itt_est <- coef(model)

d <- summaryofols[2,2]
itt_pval <-  summary(model)$coefficients[,4]

itt_N <- stats::nobs(model)

clusterset <- sourcefile[complete.cases(J)==1,]
itt_clust_se <- cluster.robust.se(model, clusterset$sid)

# Then run the TOT
tot_form <- as.formula(paste0(J, "~", treatvar, "+", vars_split, "+ factor(blocknum)|", assignvar, "+", vars_split, "+ factor(blocknum)"))


tslsmodel <<- ivreg(tot_form, weight=weight, data=sourcefile)

totmod <- summary(tslsmodel)

tot_robustmod <<- robust.se(tslsmodel)

tot_est <- coef(tslsmodel)
tot_se <- robust.se(tslsmodel)
tot_pval <- summary(tslsmodel)$coefficients[,4]
tot_n <- stats::nobs(tslsmodel)

clusterset <- sourcefile[complete.cases(J)==1,]

tot_clustse <- cluster.robust.se(tslsmodel, clusterset$sid)
experiment <- c(itt_N, itt_est[2], itt_clust_se[2,2], itt_pval[2], tot_n, tot_est[2], tot_clustse[2,2], tot_pval[2])
experiment
}

