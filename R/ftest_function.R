#' F Test Function used in BAM and Match
#'
#' This function computes the f-test p-value for baseline characteristics between treatment and control.
#' @param sourcefile is your dataframe, should include a variable for treatment assignment and whatever demographics.
#' @export
#' @examples
#' ftest_fn()


ftest_fn <- function(sourcefile){

  Ftest <- lm(dbam~dblack+dhispanic+ell_ever+
                age_pre+
                dfreelunch+dlearningdisabled+
                dgrade9+dgrade10+
                any_arrests_pre+
                violent_pre+property_pre+drug_pre+other_pre+
                gpa_pre+
                factor(blocknum), data=sourcefile)

  ftest <- linearHypothesis(Ftest,
                            c("dblack+dhispanic+ell_ever+age_pre+dfreelunch+dgrade9+dgrade10+dlearningdisabled+any_arrests_pre+
                              violent_pre+property_pre+drug_pre+other_pre+gpa_pre = 0"), test="F")

  ftest_pval_allbam <- ftest[2,6]
  ftest_pval_allbam
}