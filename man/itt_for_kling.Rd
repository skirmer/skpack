% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kling_as_function.R
\name{itt_for_kling}
\alias{itt_for_kling}
\title{Basic Kling Model Function}
\usage{
itt_for_kling(outcome, participation1, participation2, grouping, sizefile,
  weight, number_to_crop, sourcefile, weight_final, omit_type)
}
\arguments{
\item{number_to_crop}{should equal the number of grouping categories you have. It's for selecting the interactions only out of the model.}
}
\description{
This function computes the kling regressions for dividing the effect size of two different years of treatment. Your block variable must be factor,
and your dataset must omit records with a missing outcome or a missing treatment assignment.
}
\examples{
School Level MTO
itt_for_kling(outcome = "mathxil_z_post2", participation1 = "treat_post1_single", participation2 = "treat_post2",
grouping = "schlid", sizefile = schoolsizes, weight = "mtoweight_calculation", number_to_crop = 12,
sourcefile = master_dataset_1_math15_no2531, weight_final = "stderr_scores_weight", "omits")

# Block level MTO
itt_for_kling(outcome = "mathxil_z_post2", participation1 = "treat_post1_single", participation2 = "treat_post2",
grouping = "blocknum", sizefile = blocksizes, weight = "mtoweight_calculation", number_to_crop = 42,
sourcefile = master_dataset_1_math15_no2531, weight_final = "stderr_scores_weight", "omits")
}
