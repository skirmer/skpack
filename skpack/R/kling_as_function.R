#' Basic Kling Model Function
#'
#' This function computes the kling regressions for dividing the effect size of two different years of treatment. Your block variable must be factor,
#' and your dataset must omit records with a missing outcome or a missing treatment assignment.
#' @param number_to_crop should equal the number of grouping categories you have. It's for selecting the interactions only out of the model.
#' @export
#' @examples
#' School Level MTO
#' itt_for_kling(outcome = "mathxil_z_post2", participation1 = "treat_post1_single", participation2 = "treat_post2",
#' grouping = "schlid", sizefile = schoolsizes, weight = "mtoweight_calculation", number_to_crop = 12,
#' sourcefile = master_dataset_1_math15_no2531, weight_final = "stderr_scores_weight", "omits")
#'
#' # Block level MTO
#' itt_for_kling(outcome = "mathxil_z_post2", participation1 = "treat_post1_single", participation2 = "treat_post2",
#' grouping = "blocknum", sizefile = blocksizes, weight = "mtoweight_calculation", number_to_crop = 42,
#' sourcefile = master_dataset_1_math15_no2531, weight_final = "stderr_scores_weight", "omits")


itt_for_kling <- function(outcome, participation1, participation2, grouping, sizefile, weight, number_to_crop, sourcefile, weight_final, omit_type){

  library(dplyr)
  library(data.table)
  library(reshape2)
  library(sem)
  library(doBy)
  library(ggplot2)
  require(sem)
  require(AER)
  require(ivpack)
  require(lubridate)
  library(mondate)
  require("ggrepel")

  #Assemble formulas for the various models
  itt_step1_form <- formula(paste(outcome, "~", grouping, "+", grouping,":dmatch"))
  itt_step2_form <- formula(paste(participation1, "~", grouping, "+", grouping,":dmatch"))
  itt_step3optional_form <- formula(paste(participation2, "~", grouping, "+", grouping,":dmatch"))
  final_itt_form_1partic <- formula(paste("coeflist_scores~coeflist_partic-1")) #coeflist_partic2
  final_itt_form <- formula(paste("coeflist_scores~coeflist_partic+coeflist_partic2-1")) #coeflist_partic2

  #Get the counts together for weighting later
  blocksizes <- table(sourcefile[,"blocknum"])
  blocksizes <- as.data.frame(blocksizes)
  schoolsizes <- table(sourcefile[,"schlid"])
  schoolsizes <- as.data.frame(schoolsizes)

  #adding block or school size for later weighting - block level excludes some blocks but school keeps all
  master_dataset_1_forblocks <- merge(sourcefile, blocksizes, by.x="blocknum", by.y="Var1", all.x=T)
  master_dataset_1_forschools <- merge(sourcefile, schoolsizes, by.x="schlid", by.y="Var1", all.x=T)
  master_dataset_1_forschools$schlid <- as.factor(master_dataset_1_forschools$schlid)
  master_dataset_1_forblocks$schlid <- as.factor(master_dataset_1_forblocks$schlid)

  #Collect the Ns by school for using in the MTO weights
  assign_block <- summarize(group_by(master_dataset_1_forschools, dmatch, blocknum),assign_block_count = n())
  assign_school <- summarize(group_by(master_dataset_1_forschools, dmatch, schlid),assign_school_count = n())
  school_block <- summarize(group_by(master_dataset_1_forschools, schlid, blocknum),school_block_count = n())
  assign_school_block <- summarize(group_by(master_dataset_1_forschools, dmatch, schlid, blocknum),assign_school_block_count = n())
  assign <- summarize(group_by(master_dataset_1_forschools, dmatch),assign_count = n())
  allcount <- summarize(group_by(master_dataset_1_forschools),all_count = n())

  #Add the various Ns to the existing dataset
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools, assign_block, by=c("dmatch", "blocknum"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, assign_school, by=c("dmatch", "schlid"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, school_block, by=c("schlid", "blocknum"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, assign_school_block, by=c("dmatch", "schlid", "blocknum"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, assign, by=c("dmatch"), all.x=T)
  master_dataset_1_forschools_withcounts$all_count <- allcount$all_count

  #Calculate the MTO weight for each student as appropriate
  master_dataset_1_forschools_withcounts<-as.data.table(master_dataset_1_forschools_withcounts, keep.rownames=TRUE)
  master_dataset_1_forschools_withcounts[,mtoweight_calculation:=((assign_count/all_count)/(assign_school_block_count/school_block_count))]
  master_dataset_1_forschools_withcounts[,weight_calculation1:=(assign_count/all_count)]
  master_dataset_1_forschools_withcounts[,weight_calculation2:=(assign_school_block_count/school_block_count)]
  master_dataset_1_forschools_withcounts <<- as.data.frame(master_dataset_1_forschools_withcounts)

  #Collect the Ns by block for using in the MTO weights
  assign_block_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch, blocknum),assign_block_count = n())
  assign_school_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch, schlid),assign_school_count = n())
  school_block_bk <- summarize(group_by(master_dataset_1_forblocks, schlid, blocknum),school_block_count = n())
  assign_school_block_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch, schlid, blocknum),assign_school_block_count = n())
  assign_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch),assign_count = n())
  allcount_bk <- summarize(group_by(master_dataset_1_forblocks),all_count = n())

  #Add the various Ns to the existing dataset
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forschools, assign_block_bk, by=c("dmatch", "blocknum"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, assign_school_bk, by=c("dmatch", "schlid"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, school_block_bk, by=c("schlid", "blocknum"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, assign_school_block_bk, by=c("dmatch", "schlid", "blocknum"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, assign_bk, by=c("dmatch"), all.x=T)
  master_dataset_1_forblocks_withcounts$all_count <- allcount_bk$all_count

  #Calculate the MTO weight for each student as appropriate
  master_dataset_1_forblocks_withcounts<-as.data.table(master_dataset_1_forblocks_withcounts, keep.rownames=TRUE)
  master_dataset_1_forblocks_withcounts[,mtoweight_calculation:=((assign_count/all_count)/(assign_school_block_count/school_block_count))]
  master_dataset_1_forblocks_withcounts[,weight_calculation1:=(assign_count/all_count)]
  master_dataset_1_forblocks_withcounts[,weight_calculation2:=(assign_school_block_count/school_block_count)]
  master_dataset_1_forblocks_withcounts <<- as.data.frame(master_dataset_1_forblocks_withcounts)


  #Conditional- if you're working based on schools, then your filename is this.
  if(grouping=="schlid"){
    working_data_file <<- master_dataset_1_forschools_withcounts
    file_string <<- "master_dataset_1_forschools_withcounts"
  }

  #Conditional- if you're working based on blocks, then your filename is this.
    if(grouping=="blocknum"){
    working_data_file <<- master_dataset_1_forblocks
    file_string <<- "master_dataset_1_forblocks"
  }

  #Creates school level summary variables
  sample <- summarize(group_by(master_dataset_1_forschools_withcounts, blocknum, dmatch, schlid, assign_block_count, assign_school_count, school_block_count, assign_school_block_count, assign_count, all_count, mtoweight_calculation),
                      count = n(),
                      tx_yr1 = sum(treat_post1, na.rm=T),
                      tx_yr2 = sum(treat_post2, na.rm=T),
                      pct_tx_yr1 = tx_yr1/count,
                      pct_tx_yr2 = tx_yr2/count,
                      math_2015_avg = mean(mathxil_z_post2)
  )


  # Layered ITT Work ####

  #Build the weight variable out of the function arguments
  weight2 <<- eval(parse(text=paste0(file_string, "$", weight)))

  # First ITT - Math Score #
  scoremod <- lm(itt_step1_form, weights=weight2, data=working_data_file)
  coeflist_scores <- coef(scoremod)
  stderr_scores <- coef(summary(scoremod))[,2]
  resid_scores <- resid(scoremod)
  a <<- nobs(scoremod)
  # Second ITT - Year 1 Treatment Participation #
  partic_mod <- lm(itt_step2_form, weights=weight2, data=working_data_file)
  coeflist_partic <- coef(partic_mod)
  stderr_partic <- coef(summary(partic_mod))[,2]
  resid_partic <- resid(partic_mod)
  b <<- nobs(partic_mod)
  # Third ITT - Year 2 Treatment Participation #
  partic2_mod <- lm(itt_step3optional_form,weights=weight2, data=working_data_file)
  coeflist_partic2 <- coef(partic2_mod)
  stderr_partic2 <- coef(summary(partic2_mod))[,2]
  resid_partic2 <- resid(partic2_mod)
  ccc <<- nobs(partic2_mod)

  # Combine ITT output into a new data frame ####
  scorepartic <- cbind(coeflist_scores, coeflist_partic, coeflist_partic2, stderr_scores, stderr_partic, stderr_partic2)
  scorepartic_resids <- cbind(resid_scores, resid_partic, resid_partic2)

  #Drop the estimates for the individual blocks- we just want the interactions here#
  working_scorepartic <- scorepartic[-c(1:number_to_crop),]
  working_scorepartic <- cbind(sizefile, working_scorepartic)
  working_scorepartic<-as.data.frame(working_scorepartic)

  #Calculate the precision weights- 1/standard error squared
  working_scorepartic<-as.data.table(working_scorepartic, keep.rownames=TRUE)
  working_scorepartic[,stderr_scores_weight:=(1/(working_scorepartic$stderr_scores^2))]
  working_scorepartic[,stderr_partic_weight:=(1/(working_scorepartic$stderr_partic^2))]
  working_scorepartic[,stderr_partic2_weight:=(1/(working_scorepartic$stderr_partic2^2))]
  working_scorepartic <- as.data.frame(working_scorepartic)

  #Create the weight variable for the final model
  weight_final2 <- eval(parse(text=paste0("working_scorepartic", "$", weight_final)))

  #Run the final ITT model
  summary(lm(final_itt_form,weights=weight_final2,data=working_scorepartic))
  ittmodel <- lm(final_itt_form,weights=weight_final2,data=working_scorepartic)
  ittmodel_robust <- robust.se(ittmodel)
  print(ittmodel_robust)
  d <<- nobs(ittmodel)
  #Create the output with dynamic naming
  assign(paste0("ittmodel_", grouping,"_", weight, "_", omit_type), envir=.GlobalEnv, ittmodel)
  assign(paste0("ittmodel_", grouping,"_", weight, "_", omit_type, "_robustse"), envir=.GlobalEnv, ittmodel_robust)

  #ALTERNATIVE APPROACH - uses a different strategy with just one formula.
  #Build formula, uses inputs from the function arguments again
  klingform <- formula(paste("J~",  grouping, "+", participation1, "+", participation2, "|", grouping, "+", grouping, ":dmatch"))

  suppressMessages(library(formattable))
  suppressMessages(library(knitr))
  suppressMessages(library(data.table))
  suppressMessages(library(dplyr))
  suppressMessages(library(xtable))
  suppressMessages(library(sem))
  suppressMessages(library(AER))
  suppressMessages(library(ivpack))

  #Run an IV Reg with the formula designed above.
  fulloutcome <- eval(parse(text=paste0(file_string, "$", outcome)))
  tsls_fn_small <- function(I) {
    J <<- as.matrix(I)
    require(sem)
    model <<- ivreg(klingform,
                    data=working_data_file,
                    weight=weight2)
    totmod <<- summary(model)
    totmod
    robustmod <<- robust.se(model)
    robustmod
  }

  print(tsls_fn_small(fulloutcome))

  #Create the output with dynamic naming
  assign(paste0("klingmodel_", grouping,"_", weight, "_", omit_type), envir=.GlobalEnv, model)
  assign(paste0("klingmodel_", grouping,"_", weight, "_", omit_type, "_robustse"), envir=.GlobalEnv, robustmod)


  #ALTERNATIVE APPROACH - one formula, but the regressor is block, and the grouping variable is school.
  #Build formula, uses inputs from the function arguments again
  klingform2 <<- formula(paste("J~",  "blocknum", "+", participation1, "+", participation2, "|", "blocknum", "+", "schlid", ":dmatch"))

  #Run an IV Reg with the formula designed above.
  fulloutcome <- eval(parse(text=paste0(file_string, "$", outcome)))
  tsls_fn_small2 <- function(I) {
    J <<- as.matrix(I)
    require(sem)
    model3 <<- ivreg(klingform2,
                    data=working_data_file,
                    weight=weight2)
    totmod <<- summary(model3)
    totmod
    robustmod <<- robust.se(model3)
    robustmod
  }

  print(tsls_fn_small2(fulloutcome))

  #Create the output with dynamic naming
  assign(paste0("klingmodel_mixed_", weight, "_", omit_type), envir=.GlobalEnv, model)
  assign(paste0("klingmodel_mixed_", weight, "_", omit_type, "_robustse"), envir=.GlobalEnv, robustmod)

}


#' Kling Model with Three Participation Variables
#'
#' This function is generally the same as itt_for_kling but it allows for three years and three participation vars.
#' @param number_to_crop should equal the number of grouping categories you have. It's for selecting the interactions only out of the model.
#' @export
#' @examples
#' School Level MTO
#' itt_for_kling_p3(outcome = "mathxil_z_post2", participation1 = "treat_post1_single", participation2 = "treat_post2",
#' participation3 = "treat_post3", grouping = "schlid", sizefile = schoolsizes, weight = "mtoweight_calculation", number_to_crop = 12,
#' sourcefile = master_dataset_1_math15_no2531, weight_final = "stderr_scores_weight", "omits")


itt_for_kling_p3 <- function(outcome, participation1, participation2, participation3, grouping, sizefile, weight, number_to_crop, sourcefile, weight_final, omit_type){

  #Assemble formulas for the various models
  itt_step1_form <- formula(paste(outcome, "~", grouping, "+", grouping,":dmatch"))
  itt_step2_form <- formula(paste(participation1, "~", grouping, "+", grouping,":dmatch"))
  itt_step3optional_form <- formula(paste(participation2, "~", grouping, "+", grouping,":dmatch"))
  itt_step4optional_form <- formula(paste(participation3, "~", grouping, "+", grouping,":dmatch"))
  final_itt_form_1partic <- formula(paste("coeflist_scores~coeflist_partic-1")) #coeflist_partic2
  final_itt_form <- formula(paste("coeflist_scores~coeflist_partic+coeflist_partic2+coeflist_partic3-1")) #coeflist_partic2

  #Get the counts together for weighting later
  blocksizes <<- table(sourcefile[,"blocknum"])
  blocksizes <- as.data.frame(blocksizes)
  schoolsizes <<- table(sourcefile[,"schlid"])
  schoolsizes <- as.data.frame(schoolsizes)

  #adding block or school size for later weighting - block level excludes some blocks but school keeps all
  master_dataset_1_forblocks <- merge(sourcefile, blocksizes, by.x="blocknum", by.y="Var1", all.x=T)
  master_dataset_1_forschools <- merge(sourcefile, schoolsizes, by.x="schlid", by.y="Var1", all.x=T)
  master_dataset_1_forschools$schlid <- as.factor(master_dataset_1_forschools$schlid)
  master_dataset_1_forblocks$schlid <- as.factor(master_dataset_1_forblocks$schlid)

  #Collect the Ns by school for using in the MTO weights
  assign_block <- summarize(group_by(master_dataset_1_forschools, dmatch, blocknum),assign_block_count = n())
  assign_school <- summarize(group_by(master_dataset_1_forschools, dmatch, schlid),assign_school_count = n())
  school_block <- summarize(group_by(master_dataset_1_forschools, schlid, blocknum),school_block_count = n())
  assign_school_block <- summarize(group_by(master_dataset_1_forschools, dmatch, schlid, blocknum),assign_school_block_count = n())
  assign <- summarize(group_by(master_dataset_1_forschools, dmatch),assign_count = n())
  allcount <- summarize(group_by(master_dataset_1_forschools),all_count = n())

  #Add the various Ns to the existing dataset
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools, assign_block, by=c("dmatch", "blocknum"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, assign_school, by=c("dmatch", "schlid"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, school_block, by=c("schlid", "blocknum"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, assign_school_block, by=c("dmatch", "schlid", "blocknum"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, assign, by=c("dmatch"), all.x=T)
  master_dataset_1_forschools_withcounts$all_count <- allcount$all_count

  #Calculate the MTO weight for each student as appropriate
  master_dataset_1_forschools_withcounts<-as.data.table(master_dataset_1_forschools_withcounts, keep.rownames=TRUE)
  master_dataset_1_forschools_withcounts[,mtoweight_calculation:=((assign_count/all_count)/(assign_school_block_count/school_block_count))]
  master_dataset_1_forschools_withcounts[,weight_calculation1:=(assign_count/all_count)]
  master_dataset_1_forschools_withcounts[,weight_calculation2:=(assign_school_block_count/school_block_count)]
  master_dataset_1_forschools_withcounts <- as.data.frame(master_dataset_1_forschools_withcounts)

  #Collect the Ns by block for using in the MTO weights
  assign_block_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch, blocknum),assign_block_count = n())
  assign_school_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch, schlid),assign_school_count = n())
  school_block_bk <- summarize(group_by(master_dataset_1_forblocks, schlid, blocknum),school_block_count = n())
  assign_school_block_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch, schlid, blocknum),assign_school_block_count = n())
  assign_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch),assign_count = n())
  allcount_bk <- summarize(group_by(master_dataset_1_forblocks),all_count = n())

  #Add the various Ns to the existing dataset
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forschools, assign_block_bk, by=c("dmatch", "blocknum"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, assign_school_bk, by=c("dmatch", "schlid"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, school_block_bk, by=c("schlid", "blocknum"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, assign_school_block_bk, by=c("dmatch", "schlid", "blocknum"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, assign_bk, by=c("dmatch"), all.x=T)
  master_dataset_1_forblocks_withcounts$all_count <- allcount_bk$all_count

  #Calculate the MTO weight for each student as appropriate
  master_dataset_1_forblocks_withcounts<-as.data.table(master_dataset_1_forblocks_withcounts, keep.rownames=TRUE)
  master_dataset_1_forblocks_withcounts[,mtoweight_calculation:=((assign_count/all_count)/(assign_school_block_count/school_block_count))]
  master_dataset_1_forblocks_withcounts[,weight_calculation1:=(assign_count/all_count)]
  master_dataset_1_forblocks_withcounts[,weight_calculation2:=(assign_school_block_count/school_block_count)]
  master_dataset_1_forblocks_withcounts <<- as.data.frame(master_dataset_1_forblocks_withcounts)


  #Conditional- if you're working based on schools, then your filename is this.
  if(grouping=="schlid"){
    working_data_file <<- master_dataset_1_forschools_withcounts
    file_string <<- "master_dataset_1_forschools_withcounts"
  }

  #Conditional- if you're working based on blocks, then your filename is this.
  if(grouping=="blocknum"){
    working_data_file <<- master_dataset_1_forblocks
    file_string <<- "master_dataset_1_forblocks"
  }

  #Creates school level summary variables
  sample <- summarize(group_by(master_dataset_1_forschools_withcounts, blocknum, dmatch, schlid, assign_block_count, assign_school_count, school_block_count, assign_school_block_count, assign_count, all_count, mtoweight_calculation),
                      count = n(),
                      tx_yr1 = sum(treat_post1, na.rm=T),
                      tx_yr2 = sum(treat_post2, na.rm=T),
                      pct_tx_yr1 = tx_yr1/count,
                      pct_tx_yr2 = tx_yr2/count,
                      math_2015_avg = mean(mathxil_z_post2)
  )


  # Layered ITT Work ####

  #Build the weight variable out of the function arguments
  weight2 <<- eval(parse(text=paste0(file_string, "$", weight)))

  # First ITT - Math Score #
  scoremod <- lm(itt_step1_form, weights=weight2, data=working_data_file)
  coeflist_scores <- coef(scoremod)
  stderr_scores <- coef(summary(scoremod))[,2]
  resid_scores <- resid(scoremod)

  # Second ITT - Year 1 Treatment Participation #
  partic_mod <- lm(itt_step2_form, weights=weight2, data=working_data_file)
  coeflist_partic <- coef(partic_mod)
  stderr_partic <- coef(summary(partic_mod))[,2]
  resid_partic <- resid(partic_mod)

  # Third ITT - Year 2 Treatment Participation #
  partic2_mod <- lm(itt_step3optional_form,weights=weight2, data=working_data_file)
  coeflist_partic2 <- coef(partic2_mod)
  stderr_partic2 <- coef(summary(partic2_mod))[,2]
  resid_partic2 <- resid(partic2_mod)

  # Fourth ITT - Third Treatment Participation #
  partic3_mod <- lm(itt_step4optional_form,weights=weight2, data=working_data_file)
  coeflist_partic3 <- coef(partic3_mod)
  stderr_partic3 <- coef(summary(partic3_mod))[,2]
  resid_partic3 <- resid(partic3_mod)

  # Combine ITT output into a new data frame ####
  scorepartic <- cbind(coeflist_scores, coeflist_partic, coeflist_partic2, coeflist_partic3, stderr_scores, stderr_partic, stderr_partic2, stderr_partic3)
  scorepartic_resids <- cbind(resid_scores, resid_partic, resid_partic2, resid_partic3)

  #Drop the estimates for the individual blocks- we just want the interactions here#
  working_scorepartic <- scorepartic[-c(1:number_to_crop),]
  working_scorepartic <- cbind(sizefile, working_scorepartic)
  working_scorepartic<-as.data.frame(working_scorepartic)

  #Calculate the precision weights- 1/standard error squared
  working_scorepartic<-as.data.table(working_scorepartic, keep.rownames=TRUE)
  working_scorepartic[,stderr_scores_weight:=(1/(working_scorepartic$stderr_scores^2))]
  working_scorepartic[,stderr_partic_weight:=(1/(working_scorepartic$stderr_partic^2))]
  working_scorepartic[,stderr_partic2_weight:=(1/(working_scorepartic$stderr_partic2^2))]
  working_scorepartic[,stderr_partic3_weight:=(1/(working_scorepartic$stderr_partic3^2))]
  working_scorepartic <- as.data.frame(working_scorepartic)

  #Create the weight variable for the final model
  weight_final2 <- eval(parse(text=paste0("working_scorepartic", "$", weight_final)))

  #Run the final ITT model
  summary(lm(final_itt_form,weights=weight_final2,data=working_scorepartic))
  ittmodel <- lm(final_itt_form,weights=weight_final2,data=working_scorepartic)
  ittmodel_robust <- robust.se(ittmodel)
  print(ittmodel_robust)


  #Create the output with dynamic naming
  assign(paste0("ittmodel_", grouping,"_", weight, "_", omit_type), envir=.GlobalEnv, ittmodel)
  assign(paste0("ittmodel_", grouping,"_", weight, "_", omit_type, "_robustse"), envir=.GlobalEnv, ittmodel_robust)


  #ALTERNATIVE APPROACH - uses a different strategy with just one formula.
  #Build formula, uses inputs from the function arguments again
  klingform <- formula(paste("J~",  grouping, "+", participation1, "+", participation2, "+", participation3, "|", grouping, "+", grouping, ":dmatch"))

  suppressMessages(library(formattable))
  suppressMessages(library(knitr))
  suppressMessages(library(data.table))
  suppressMessages(library(dplyr))
  suppressMessages(library(xtable))
  suppressMessages(library(sem))
  suppressMessages(library(AER))
  suppressMessages(library(ivpack))

  #Run an IV Reg with the formula designed above.
  fulloutcome <- eval(parse(text=paste0(file_string, "$", outcome)))
  tsls_fn_small <- function(I) {
    J <<- as.matrix(I)
    require(sem)
    model <<- ivreg(klingform,
                    data=working_data_file,
                    weight=weight2)
    totmod <<- summary(model)
    totmod
    robustmod <<- robust.se(model)
    robustmod
  }

  print(tsls_fn_small(fulloutcome))

  #Create the output with dynamic naming
  assign(paste0("klingmodel_", grouping,"_", weight, "_", omit_type), envir=.GlobalEnv, model)
  assign(paste0("klingmodel_", grouping,"_", weight, "_", omit_type, "_robustse"), envir=.GlobalEnv, robustmod)

}



#' Kling Model with Two Grouping Variables
#'
#' This function is generally the same as itt_for_kling but it allows for mixing block and school (or whatever) grouping variables.
#' @param number_to_crop should equal the number of grouping categories you have. It's for selecting the interactions only out of the model.
#' @export
#' @examples
#' School Level
#' itt_for_kling_p4(outcome = "mathxil_z_post2", participation1 = "treat_post1", participation2 = "treat_post2",
#' grouping1 = "schlid", grouping2 = "blocknum", sizefile = schoolsizes, weight = "precision", number_to_crop = 42,
#' sourcefile = master_dataset_1_math15_allblocks, weight_final = "stderr_scores_weight", "noomits_nome")

itt_for_kling_p4 <- function(outcome, participation1, participation2, grouping1, grouping2, sizefile, weight, number_to_crop, sourcefile, weight_final, omit_type){

  #Assemble formulas for the various models #
  itt_step1_form <- formula(paste(outcome, "~", grouping2, "+", grouping1,":dmatch"))
  itt_step2_form <- formula(paste(participation1, "~", grouping2, "+", grouping1,":dmatch"))
  itt_step3optional_form <- formula(paste(participation2, "~", grouping2, "+", grouping1,":dmatch"))
  final_itt_form_1partic <- formula(paste("coeflist_scores~coeflist_partic-1")) #coeflist_partic2
  final_itt_form <- formula(paste("coeflist_scores~coeflist_partic+coeflist_partic2-1")) #coeflist_partic2

  #Get the counts together for weighting later
  blocksizes <<- table(sourcefile[,"blocknum"])
  blocksizes <- as.data.frame(blocksizes)
  schoolsizes <<- table(sourcefile[,"schlid"])
  schoolsizes <- as.data.frame(schoolsizes)

  #adding block or school size for later weighting - block level excludes some blocks but school keeps all
  master_dataset_1_forblocks <- merge(sourcefile, blocksizes, by.x="blocknum", by.y="Var1", all.x=T)
  master_dataset_1_forschools <- merge(sourcefile, schoolsizes, by.x="schlid", by.y="Var1", all.x=T)
  master_dataset_1_forschools$schlid <- as.factor(master_dataset_1_forschools$schlid)
  master_dataset_1_forblocks$schlid <- as.factor(master_dataset_1_forblocks$schlid)

  #Collect the Ns by school for using in the MTO weights
  assign_block <- summarize(group_by(master_dataset_1_forschools, dmatch, blocknum),assign_block_count = n())
  assign_school <- summarize(group_by(master_dataset_1_forschools, dmatch, schlid),assign_school_count = n())
  school_block <- summarize(group_by(master_dataset_1_forschools, schlid, blocknum),school_block_count = n())
  assign_school_block <- summarize(group_by(master_dataset_1_forschools, dmatch, schlid, blocknum),assign_school_block_count = n())
  assign <- summarize(group_by(master_dataset_1_forschools, dmatch),assign_count = n())
  allcount <- summarize(group_by(master_dataset_1_forschools),all_count = n())

  #Add the various Ns to the existing dataset
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools, assign_block, by=c("dmatch", "blocknum"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, assign_school, by=c("dmatch", "schlid"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, school_block, by=c("schlid", "blocknum"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, assign_school_block, by=c("dmatch", "schlid", "blocknum"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, assign, by=c("dmatch"), all.x=T)
  master_dataset_1_forschools_withcounts$all_count <- allcount$all_count

  #Calculate the MTO weight for each student as appropriate
  master_dataset_1_forschools_withcounts<-as.data.table(master_dataset_1_forschools_withcounts, keep.rownames=TRUE)
  master_dataset_1_forschools_withcounts[,mtoweight_calculation:=((assign_count/all_count)/(assign_school_block_count/school_block_count))]
  master_dataset_1_forschools_withcounts[,weight_calculation1:=(assign_count/all_count)]
  master_dataset_1_forschools_withcounts[,weight_calculation2:=(assign_school_block_count/school_block_count)]
  master_dataset_1_forschools_withcounts <- as.data.frame(master_dataset_1_forschools_withcounts)

  #Collect the Ns by block for using in the MTO weights
  assign_block_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch, blocknum),assign_block_count = n())
  assign_school_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch, schlid),assign_school_count = n())
  school_block_bk <- summarize(group_by(master_dataset_1_forblocks, schlid, blocknum),school_block_count = n())
  assign_school_block_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch, schlid, blocknum),assign_school_block_count = n())
  assign_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch),assign_count = n())
  allcount_bk <- summarize(group_by(master_dataset_1_forblocks),all_count = n())

  #Add the various Ns to the existing dataset
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forschools, assign_block_bk, by=c("dmatch", "blocknum"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, assign_school_bk, by=c("dmatch", "schlid"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, school_block_bk, by=c("schlid", "blocknum"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, assign_school_block_bk, by=c("dmatch", "schlid", "blocknum"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, assign_bk, by=c("dmatch"), all.x=T)
  master_dataset_1_forblocks_withcounts$all_count <- allcount_bk$all_count

  #Calculate the MTO weight for each student as appropriate
  master_dataset_1_forblocks_withcounts<-as.data.table(master_dataset_1_forblocks_withcounts, keep.rownames=TRUE)
  master_dataset_1_forblocks_withcounts[,mtoweight_calculation:=((assign_count/all_count)/(assign_school_block_count/school_block_count))]
  master_dataset_1_forblocks_withcounts[,weight_calculation1:=(assign_count/all_count)]
  master_dataset_1_forblocks_withcounts[,weight_calculation2:=(assign_school_block_count/school_block_count)]
  master_dataset_1_forblocks_withcounts <<- as.data.frame(master_dataset_1_forblocks_withcounts)


  #Conditional- if you're working based on schools, then your filename is this.
  if(grouping1=="schlid"){
    working_data_file <<- master_dataset_1_forschools_withcounts
    file_string <<- "master_dataset_1_forschools_withcounts"
  }

  #Conditional- if you're working based on blocks, then your filename is this.
  if(grouping1=="blocknum"){
    working_data_file <<- master_dataset_1_forblocks
    file_string <<- "master_dataset_1_forblocks"
  }

  #Creates school level summary variables
  sample <- summarize(group_by(master_dataset_1_forschools_withcounts, blocknum, dmatch, schlid, assign_block_count, assign_school_count, school_block_count, assign_school_block_count, assign_count, all_count, mtoweight_calculation),
                      count = n(),
                      tx_yr1 = sum(treat_post1, na.rm=T),
                      tx_yr2 = sum(treat_post2, na.rm=T),
                      pct_tx_yr1 = tx_yr1/count,
                      pct_tx_yr2 = tx_yr2/count,
                      math_2015_avg = mean(mathxil_z_post2)
  )


  # Layered ITT Work ####

  #Build the weight variable out of the function arguments
  weight2 <<- eval(parse(text=paste0(file_string, "$", weight)))

  # First ITT - Math Score #
  scoremod <- lm(itt_step1_form, weights=weight2, data=working_data_file)
  coeflist_scores <- coef(scoremod)
  stderr_scores <- coef(summary(scoremod))[,2]
  resid_scores <- resid(scoremod)

  # Second ITT - Year 1 Treatment Participation #
  partic_mod <- lm(itt_step2_form, weights=weight2, data=working_data_file)
  coeflist_partic <- coef(partic_mod)
  stderr_partic <- coef(summary(partic_mod))[,2]
  resid_partic <- resid(partic_mod)

  # Third ITT - Year 2 Treatment Participation #
  partic2_mod <- lm(itt_step3optional_form,weights=weight2, data=working_data_file)
  coeflist_partic2 <- coef(partic2_mod)
  stderr_partic2 <- coef(summary(partic2_mod))[,2]
  resid_partic2 <- resid(partic2_mod)

  # Combine ITT output into a new data frame ####
  scorepartic <- cbind(coeflist_scores, coeflist_partic, coeflist_partic2, stderr_scores, stderr_partic, stderr_partic2)
  scorepartic_resids <- cbind(resid_scores, resid_partic, resid_partic2)

  #Drop the estimates for the individual blocks- we just want the interactions here#
  working_scorepartic <- scorepartic[-c(1:number_to_crop),]
  working_scorepartic <- cbind(sizefile, working_scorepartic)
  working_scorepartic<-as.data.frame(working_scorepartic)

  #Calculate the precision weights- 1/standard error squared
  working_scorepartic<-as.data.table(working_scorepartic, keep.rownames=TRUE)
  working_scorepartic[,stderr_scores_weight:=(1/(working_scorepartic$stderr_scores^2))]
  working_scorepartic[,stderr_partic_weight:=(1/(working_scorepartic$stderr_partic^2))]
  working_scorepartic[,stderr_partic2_weight:=(1/(working_scorepartic$stderr_partic2^2))]
  working_scorepartic <- as.data.frame(working_scorepartic)

  #Create the weight variable for the final model
  weight_final2 <- eval(parse(text=paste0("working_scorepartic", "$", weight_final)))

  #Run the final ITT model
  summary(lm(final_itt_form,weights=weight_final2,data=working_scorepartic))
  ittmodel <- lm(final_itt_form,weights=weight_final2,data=working_scorepartic)
  ittmodel_robust <- robust.se(ittmodel)
  print(ittmodel_robust)

  #Create the output with dynamic naming
  assign(paste0("ittmodel_", "mixed2","_", weight, "_", omit_type), envir=.GlobalEnv, ittmodel)
  assign(paste0("ittmodel_", "mixed2","_", weight, "_", omit_type, "_robustse"), envir=.GlobalEnv, ittmodel_robust)

  #ALTERNATIVE APPROACH - uses a different strategy with just one formula.
  #Build formula, uses inputs from the function arguments again
  klingform <- formula(paste("J~", participation1, "+", participation2, "+", grouping2,
                               "|", grouping1, ":dmatch", "+", grouping2))#, "+", grouping2, ":dmatch"))

  #grouping1, ":dmatch", "+",
  #grouping1, ":dmatch", "+" ,
  print(klingform)


  suppressMessages(library(formattable))
  suppressMessages(library(knitr))
  suppressMessages(library(data.table))
  suppressMessages(library(dplyr))
  suppressMessages(library(xtable))
  suppressMessages(library(sem))
  suppressMessages(library(AER))
  suppressMessages(library(ivpack))

  #Run an IV Reg with the formula designed above.
  fulloutcome <- eval(parse(text=paste0(file_string, "$", outcome)))
  tsls_fn_small <- function(I) {
    J <<- as.matrix(I)
    require(sem)
    model <<- ivreg(klingform,
                    data=working_data_file,
                    weight=weight2)
    totmod <<- summary(model)
    totmod
    robustmod <<- robust.se(model)
    robustmod
  }

  print(tsls_fn_small(fulloutcome))

  #Create the output with dynamic naming
  assign(paste0("klingmodel_", "mixed2","_", weight, "_", omit_type), envir=.GlobalEnv, model)
  assign(paste0("klingmodel_", "mixed2","_", weight, "_", omit_type, "_robustse"), envir=.GlobalEnv, robustmod)

}



#' Kling Model with Two Grouping Variables, Instrumenting Var 2
#'
#' This function is generally the same as itt_for_kling_p4 but it puts grouping variable 2 in the ITT models and instruments for it in the ivreg.
#' This variation seems to have produced good results for the Match/BAM project.
#' @param number_to_crop should equal the number of grouping categories you have. It's for selecting the interactions only out of the model.
#' @export
#' @examples
#' Block Level
#' itt_for_kling_p6(outcome = "mathxil_z_post2", participation1 = "treat_post1", participation2 = "treat_post2",
#' grouping1 = "schlid", grouping2 = "blocknum", sizefile = schoolsizes, weight = "precision", number_to_crop = 42,
#' sourcefile = master_dataset_1_math15_allblocks, weight_final = "stderr_scores_weight", "noomits_nome")


itt_for_kling_p6 <- function(outcome, participation1, participation2, grouping1, grouping2, sizefile, weight, number_to_crop, sourcefile, weight_final, omit_type){

  #Assemble formulas for the various models #
  itt_step1_form <- formula(paste(outcome, "~", grouping2,":dmatch", "+", grouping2))#,":dmatch"))
  itt_step2_form <- formula(paste(participation1, "~", grouping2,":dmatch", "+", grouping2))#,":dmatch"))
  itt_step3optional_form <- formula(paste(participation2, "~", grouping2,":dmatch", "+", grouping2))#,":dmatch"))
  final_itt_form_1partic <- formula(paste("coeflist_scores~coeflist_partic-1")) #coeflist_partic2
  final_itt_form <- formula(paste("coeflist_scores~coeflist_partic+coeflist_partic2-1")) #coeflist_partic2

  #Get the counts together for weighting later
  blocksizes <<- table(sourcefile[,"blocknum"])
  blocksizes <- as.data.frame(blocksizes)
  schoolsizes <<- table(sourcefile[,"schlid"])
  schoolsizes <- as.data.frame(schoolsizes)

  #adding block or school size for later weighting - block level excludes some blocks but school keeps all
  master_dataset_1_forblocks <- merge(sourcefile, blocksizes, by.x="blocknum", by.y="Var1", all.x=T)
  master_dataset_1_forschools <- merge(sourcefile, schoolsizes, by.x="schlid", by.y="Var1", all.x=T)
  master_dataset_1_forschools$schlid <- as.factor(master_dataset_1_forschools$schlid)
  master_dataset_1_forblocks$schlid <- as.factor(master_dataset_1_forblocks$schlid)

  #Collect the Ns by school for using in the MTO weights
  assign_block <- summarize(group_by(master_dataset_1_forschools, dmatch, blocknum),assign_block_count = n())
  assign_school <- summarize(group_by(master_dataset_1_forschools, dmatch, schlid),assign_school_count = n())
  school_block <- summarize(group_by(master_dataset_1_forschools, schlid, blocknum),school_block_count = n())
  assign_school_block <- summarize(group_by(master_dataset_1_forschools, dmatch, schlid, blocknum),assign_school_block_count = n())
  assign <- summarize(group_by(master_dataset_1_forschools, dmatch),assign_count = n())
  allcount <- summarize(group_by(master_dataset_1_forschools),all_count = n())

  #Add the various Ns to the existing dataset
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools, assign_block, by=c("dmatch", "blocknum"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, assign_school, by=c("dmatch", "schlid"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, school_block, by=c("schlid", "blocknum"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, assign_school_block, by=c("dmatch", "schlid", "blocknum"), all.x=T)
  master_dataset_1_forschools_withcounts <- merge(master_dataset_1_forschools_withcounts, assign, by=c("dmatch"), all.x=T)
  master_dataset_1_forschools_withcounts$all_count <- allcount$all_count

  #Calculate the MTO weight for each student as appropriate
  master_dataset_1_forschools_withcounts<-as.data.table(master_dataset_1_forschools_withcounts, keep.rownames=TRUE)
  master_dataset_1_forschools_withcounts[,mtoweight_calculation:=((assign_count/all_count)/(assign_school_block_count/school_block_count))]
  master_dataset_1_forschools_withcounts[,weight_calculation1:=(assign_count/all_count)]
  master_dataset_1_forschools_withcounts[,weight_calculation2:=(assign_school_block_count/school_block_count)]
  master_dataset_1_forschools_withcounts <- as.data.frame(master_dataset_1_forschools_withcounts)

  #Collect the Ns by block for using in the MTO weights
  assign_block_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch, blocknum),assign_block_count = n())
  assign_school_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch, schlid),assign_school_count = n())
  school_block_bk <- summarize(group_by(master_dataset_1_forblocks, schlid, blocknum),school_block_count = n())
  assign_school_block_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch, schlid, blocknum),assign_school_block_count = n())
  assign_bk <- summarize(group_by(master_dataset_1_forblocks, dmatch),assign_count = n())
  allcount_bk <- summarize(group_by(master_dataset_1_forblocks),all_count = n())

  #Add the various Ns to the existing dataset
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forschools, assign_block_bk, by=c("dmatch", "blocknum"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, assign_school_bk, by=c("dmatch", "schlid"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, school_block_bk, by=c("schlid", "blocknum"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, assign_school_block_bk, by=c("dmatch", "schlid", "blocknum"), all.x=T)
  master_dataset_1_forblocks_withcounts <- merge(master_dataset_1_forblocks_withcounts, assign_bk, by=c("dmatch"), all.x=T)
  master_dataset_1_forblocks_withcounts$all_count <- allcount_bk$all_count

  #Calculate the MTO weight for each student as appropriate
  master_dataset_1_forblocks_withcounts<-as.data.table(master_dataset_1_forblocks_withcounts, keep.rownames=TRUE)
  master_dataset_1_forblocks_withcounts[,mtoweight_calculation:=((assign_count/all_count)/(assign_school_block_count/school_block_count))]
  master_dataset_1_forblocks_withcounts[,weight_calculation1:=(assign_count/all_count)]
  master_dataset_1_forblocks_withcounts[,weight_calculation2:=(assign_school_block_count/school_block_count)]
  master_dataset_1_forblocks_withcounts <<- as.data.frame(master_dataset_1_forblocks_withcounts)


  #Conditional- if you're working based on schools, then your filename is this.
  if(grouping1=="schlid"){
    working_data_file <<- master_dataset_1_forschools_withcounts
    file_string <<- "master_dataset_1_forschools_withcounts"
  }

  #Conditional- if you're working based on blocks, then your filename is this.
  if(grouping1=="blocknum"){
    working_data_file <<- master_dataset_1_forblocks
    file_string <<- "master_dataset_1_forblocks"
  }

  #Creates school level summary variables
  sample <- summarize(group_by(master_dataset_1_forschools_withcounts, blocknum, dmatch, schlid, assign_block_count, assign_school_count, school_block_count, assign_school_block_count, assign_count, all_count, mtoweight_calculation),
                      count = n(),
                      tx_yr1 = sum(treat_post1, na.rm=T),
                      tx_yr2 = sum(treat_post2, na.rm=T),
                      pct_tx_yr1 = tx_yr1/count,
                      pct_tx_yr2 = tx_yr2/count,
                      math_2015_avg = mean(mathxil_z_post2)
  )


  # Layered ITT Work ####

  #Build the weight variable out of the function arguments
  weight2 <<- eval(parse(text=paste0(file_string, "$", weight)))

  # First ITT - Math Score #
  scoremod <- lm(itt_step1_form, weights=weight2, data=working_data_file)
  coeflist_scores <- coef(scoremod)
  stderr_scores <- coef(summary(scoremod))[,2]
  resid_scores <- resid(scoremod)

  print(scoremod)

  # Second ITT - Year 1 Treatment Participation #
  partic_mod <- lm(itt_step2_form, weights=weight2, data=working_data_file)
  coeflist_partic <- coef(partic_mod)
  stderr_partic <- coef(summary(partic_mod))[,2]
  resid_partic <- resid(partic_mod)

  print(partic_mod)

  # Third ITT - Year 2 Treatment Participation #
  partic2_mod <- lm(itt_step3optional_form,weights=weight2, data=working_data_file)
  coeflist_partic2 <- coef(partic2_mod)
  stderr_partic2 <- coef(summary(partic2_mod))[,2]
  resid_partic2 <- resid(partic2_mod)

  print(partic2_mod)

  # Combine ITT output into a new data frame ####
  scorepartic <- cbind(coeflist_scores, coeflist_partic, coeflist_partic2, stderr_scores, stderr_partic, stderr_partic2)
  scorepartic_resids <- cbind(resid_scores, resid_partic, resid_partic2)

  #Drop the estimates for the individual blocks- we just want the interactions here#
  working_scorepartic <- scorepartic[-c(1:number_to_crop),]
  working_scorepartic <- cbind(sizefile, working_scorepartic)
  working_scorepartic<-as.data.frame(working_scorepartic)

  #Calculate the precision weights- 1/standard error squared
  working_scorepartic<-as.data.table(working_scorepartic, keep.rownames=TRUE)
  working_scorepartic[,stderr_scores_weight:=(1/(working_scorepartic$stderr_scores^2))]
  working_scorepartic[,stderr_partic_weight:=(1/(working_scorepartic$stderr_partic^2))]
  working_scorepartic[,stderr_partic2_weight:=(1/(working_scorepartic$stderr_partic2^2))]
  working_scorepartic <- as.data.frame(working_scorepartic)

  #Create the weight variable for the final model
  weight_final2 <- eval(parse(text=paste0("working_scorepartic", "$", weight_final)))

  #Run the final ITT model
  summary(lm(final_itt_form,weights=weight_final2,data=working_scorepartic))
  ittmodel <- lm(final_itt_form,weights=weight_final2,data=working_scorepartic)
  ittmodel_robust <- robust.se(ittmodel)
  print(ittmodel_robust)

  #Create the output with dynamic naming
  assign(paste0("ittmodel_", "mixed4","_", weight, "_", omit_type), envir=.GlobalEnv, ittmodel)
  assign(paste0("ittmodel_", "mixed4","_", weight, "_", omit_type, "_robustse"), envir=.GlobalEnv, ittmodel_robust)

  #ALTERNATIVE APPROACH - uses a different strategy with just one formula.
  #Build formula, uses inputs from the function arguments again
  klingform <- formula(paste("J~", participation1, "+", participation2, "+", grouping2,
                             "|", grouping2, "+", grouping2,":dmatch"))

  #grouping1, ":dmatch", "+",
  #grouping1, ":dmatch", "+" ,
  print(klingform)


  suppressMessages(library(formattable))
  suppressMessages(library(knitr))
  suppressMessages(library(data.table))
  suppressMessages(library(dplyr))
  suppressMessages(library(xtable))
  suppressMessages(library(sem))
  suppressMessages(library(AER))
  suppressMessages(library(ivpack))

  #Run an IV Reg with the formula designed above.
  fulloutcome <- eval(parse(text=paste0(file_string, "$", outcome)))
  tsls_fn_small <- function(I) {
    J <<- as.matrix(I)
    require(sem)
    model <<- ivreg(klingform,
                    data=working_data_file,
                    weight=weight2)
    totmod <<- summary(model)
    totmod
    robustmod <<- robust.se(model)
    robustmod
  }

  print(tsls_fn_small(fulloutcome))

  #Create the output with dynamic naming
  assign(paste0("klingmodel_", "mixed4","_", weight, "_", omit_type), envir=.GlobalEnv, model)
  assign(paste0("klingmodel_", "mixed4","_", weight, "_", omit_type, "_robustse"), envir=.GlobalEnv, robustmod)

}


