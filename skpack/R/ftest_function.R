#' F Test Function
#'
#' This function computes the f-test p-value for baseline characteristics between treatment and control.
#' @param sourcefile is your dataframe, should include a variable for treatment assignment and whatever demographics.
#' @param varlist is a vector of your covariates (things you are controlling for)- all should be the names of columns in your dataset.
#' @param assignvar is the specific column that specifies the treatment/control split in your sample. Also, your blocking variable should be "blocknum".
#' @export
#' @examples
#' ftest_fn(dataset, c("var1", "var2", "var3"), "dbam")


ftest_fn <- function(sourcefile, varlist, assignvar){

  vars_split <- paste(varlist, collapse="+")
  ftest_form <- as.formula(paste0(assignvar, "~", vars_split, "+ factor(blocknum)"))

  Ftest <- lm(ftest_form, data=sourcefile)

  ftest_form2 <- paste0(vars_split, "=0")

  ftest <- linearHypothesis(Ftest,
                            c(ftest_form2), test="F")

  ftest_pval<- ftest[2,6]
  ftest_pval
}

