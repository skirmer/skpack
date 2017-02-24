#' Permutation Test Function
#'
#' This function computes permutation tests, using sample with replacement, keeping the blocks balanced. You need to edit the covariates inside the function for use in other projects.
#' @param data is your dataframe.
#' @param M is the outcome being tested.
#' @export
#' @examples
#' results_rescaled_math_post1  <- boot(data=master_dataset_1, statistic=bs_post, strata=master_dataset_1$blocknum,
#' R=100000, M="d$rescaled_math_post1", sim="permutation")
#' Result: results_rescaled_math_post1$t


# function to obtain regression output for each resample ####
permtest_multiple <- function(data, indices, M) {
  require(resample)
  require(boot)

  d <- data[indices,] # allows boot to select sample
  # Create variables to identify size of block and size of block/assignment
  d <- as.data.table(d, keep.rownames=FALSE)
  blocktxsizes<- summarize(group_by(d, blocknum),
                           size = sum(dmatch==1, na.rm=T))
  d <- merge(d, blocktxsizes, by.x="blocknum", by.y="blocknum", all.x=T)
  #   table(d$size, d$blocknum) #Make sure block/tx sizes are right

  #Sort students within schools by this number and select first x (number of treated in school)

  #Identify the "pseudoT" group
  set.seed(145560) #For ensuring replicability
  d$randomnum <- sample(1:nrow(d))
  #print(summary(d$randomnum[d$blocknum==20])) #Check to make sure the random number assignment is replicable
  d[, blkrank:= rank(-randomnum), by=blocknum]
  d[blkrank <= size, pseudoT:=1]
  d[blkrank > size, pseudoT:=0]

  # Standardize the test scores (baseline and post 1) so that the model is right
  # Going with the simplistic tedious approach
  mean_math_plan_pre <- mean(d$mathxil_pre[d$pseudoT==0 & d$plan_pre==1], na.rm=T)
  sd_math_plan_pre <- sd(d$mathxil_pre[d$pseudoT==0 & d$plan_pre==1], na.rm=T)
  mean_math_exp9_pre <- mean(d$mathxil_pre[d$pseudoT==0 & d$explore_gr9_pre==1], na.rm=T)
  sd_math_exp9_pre <- sd(d$mathxil_pre[d$pseudoT==0 & d$explore_gr9_pre==1], na.rm=T)
  mean_math_exp8_pre <- mean(d$mathxil_pre[d$pseudoT==0 & d$explore_gr8_pre==1], na.rm=T)
  sd_math_exp8_pre <- sd(d$mathxil_pre[d$pseudoT==0 & d$explore_gr8_pre==1], na.rm=T)

  mean_read_plan_pre <- mean(d$readxil_pre[d$pseudoT==0 & d$plan_pre==1], na.rm=T)
  sd_read_plan_pre <- sd(d$readxil_pre[d$pseudoT==0 & d$plan_pre==1], na.rm=T)
  mean_read_exp9_pre <- mean(d$readxil_pre[d$pseudoT==0 & d$explore_gr9_pre==1], na.rm=T)
  sd_read_exp9_pre <- sd(d$readxil_pre[d$pseudoT==0 & d$explore_gr9_pre==1], na.rm=T)
  mean_read_exp8_pre <- mean(d$readxil_pre[d$pseudoT==0 & d$explore_gr8_pre==1], na.rm=T)
  sd_read_exp8_pre <- sd(d$readxil_pre[d$pseudoT==0 & d$explore_gr8_pre==1], na.rm=T)

  mean_math_plan_post1 <- mean(d$mathxil_post1[d$pseudoT==0 & d$plan_post1==1], na.rm=T)
  sd_math_plan_post1 <- sd(d$mathxil_post1[d$pseudoT==0 & d$plan_post1==1], na.rm=T)
  mean_math_exp_post1 <- mean(d$mathxil_post1[d$pseudoT==0 & d$explore_post1==1], na.rm=T)
  sd_math_exp_post1 <- sd(d$mathxil_post1[d$pseudoT==0 & d$explore_post1==1], na.rm=T)
  mean_math_act_post1 <- mean(d$mathxil_post1[d$pseudoT==0 & d$act_post1==1], na.rm=T)
  sd_math_act_post1 <- sd(d$mathxil_post1[d$pseudoT==0 & d$act_post1==1], na.rm=T)

  mean_read_plan_post1 <- mean(d$readxil_post1[d$pseudoT==0 & d$plan_post1==1], na.rm=T)
  sd_read_plan_post1 <- sd(d$readxil_post1[d$pseudoT==0 & d$plan_post1==1], na.rm=T)
  mean_read_exp_post1 <- mean(d$readxil_post1[d$pseudoT==0 & d$explore_post1==1], na.rm=T)
  sd_read_exp_post1 <- sd(d$readxil_post1[d$pseudoT==0 & d$explore_post1==1], na.rm=T)
  mean_read_act_post1 <- mean(d$readxil_post1[d$pseudoT==0 & d$act_post1==1], na.rm=T)
  sd_read_act_post1 <- sd(d$readxil_post1[d$pseudoT==0 & d$act_post1==1], na.rm=T)

  #if anything is NA then make it zero -  act has been a problem on this, might also run into it on plan pre
  sd_math_plan_pre[is.na(sd_math_plan_pre)] <- 0
  sd_math_act_post1[is.na(sd_math_act_post1)] <- 0

  # Calculate new standardization #
  d[d$plan_pre==1, rescaled_math_plan_pre := (d$mathxil_pre-mean_math_plan_pre)/sd_math_plan_pre]
  d[d$explore_gr9_pre==1, rescaled_math_exp9_pre := (d$mathxil_pre-mean_math_exp9_pre)/sd_math_exp9_pre]
  d[d$explore_gr8_pre==1, rescaled_math_exp8_pre := (d$mathxil_pre-mean_math_exp8_pre)/sd_math_exp8_pre]

  d[d$plan_pre==1, rescaled_read_plan_pre := (d$readxil_pre-mean_read_plan_pre)/sd_read_plan_pre]
  d[d$explore_gr9_pre==1, rescaled_read_exp9_pre := (d$readxil_pre-mean_read_exp9_pre)/sd_read_exp9_pre]
  d[d$explore_gr8_pre==1, rescaled_read_exp8_pre := (d$readxil_pre-mean_read_exp8_pre)/sd_read_exp8_pre]

  d[d$plan_post1==1, rescaled_math_plan_post1 := (d$mathxil_post1-mean_math_plan_post1)/sd_math_plan_post1]
  d[d$explore_post1==1, rescaled_math_exp_post1 := (d$mathxil_post1-mean_math_exp_post1)/sd_math_exp_post1]
  d[d$act_post1==1, rescaled_math_act_post1 := (d$mathxil_post1-mean_math_act_post1)/sd_math_act_post1]

  d[d$plan_post1==1, rescaled_read_plan_post1 := (d$readxil_post1-mean_read_plan_post1)/sd_read_plan_post1]
  d[d$explore_post1==1, rescaled_read_exp_post1 := (d$readxil_post1-mean_read_exp_post1)/sd_read_exp_post1]
  d[d$act_post1==1, rescaled_read_act_post1 := (d$readxil_post1-mean_read_act_post1)/sd_read_act_post1]

  d[,rescaled_math_pre := rescaled_math_plan_pre]
  d[is.na(rescaled_math_pre),rescaled_math_pre := rescaled_math_exp9_pre]
  d[is.na(rescaled_math_pre),rescaled_math_pre := rescaled_math_exp8_pre]
  d[is.na(rescaled_math_pre),rescaled_math_pre := 0]

  d[,rescaled_read_pre := rescaled_read_plan_pre]
  d[is.na(rescaled_read_pre),rescaled_read_pre := rescaled_read_exp9_pre]
  d[is.na(rescaled_read_pre),rescaled_read_pre := rescaled_read_exp8_pre]
  d[is.na(rescaled_read_pre),rescaled_read_pre := 0]

  d[,rescaled_math_post1 := rescaled_math_plan_post1]
  d[is.na(rescaled_math_post1),rescaled_math_post1 := rescaled_math_exp_post1]
  d[is.na(rescaled_math_post1) & sd_math_act_post1 >0,rescaled_math_post1 := rescaled_math_act_post1]
  d[is.na(rescaled_math_post1), rescaled_math_post1 := 0]

  d[,rescaled_read_post1 := rescaled_read_plan_post1]
  d[is.na(rescaled_read_post1),rescaled_read_post1 := rescaled_read_exp_post1]
  d[is.na(rescaled_read_post1) & sd_read_act_post1 >0,rescaled_read_post1 := rescaled_read_act_post1]
  d[is.na(rescaled_read_post1), rescaled_read_post1 := 0]

  d <- as.data.frame(d)

  # Run the actual LM and return the T-statistic for pseudoT(dmatch proxy)

  I <- eval(parse(text=M))

  runmodel <- function(I) {
    fit <- lm(I~pseudoT+ blocknum+ d13andunder+d14+d15+d16+d17andover+ dlearningdisabled+dfreelunch+ dblack+ dhispanic+dother+
                dgrade9+ dgrade10+ gpa_pre_zeros+ numAs_pre+  numBs_pre+ numCs_pre+ numDs_pre+ numFs_pre+ missing_gpa_pre+ days_absent_pre_zeros+
                missing_attend_pre+ rescaled_math_pre+  rescaled_read_pre +mathxil_z_pre_missing+readxil_z_pre_missing+ oss_dis_pre_zeros+
                incidents_pre_zeros+any_arrests_pre+violent_pre+property_pre+drug_pre, data=d)
    summary(fit)
    #t-test
    output <- summary(fit)$coefficients["pseudoT",3]
    #pval
    #output <- summary(fit)$coefficients["pseudoT",4]
  }
  runmodel(I)
  return(output)
}
