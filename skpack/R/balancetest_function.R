#' Balance Test Table Production
#'
#' This function takes in a dataset and treatment assignment variable, and produces balance tests and control/treatment means in table form.
#' @param dataset is your dataframe
#' @param assignment is the name of the variable used to identify treatment and control.
#' @export
#' @examples
#' balance_fn()



balance_fn <- function(dataset, assignment){

  suppressMessages(library(formattable))
  suppressMessages(library(knitr))
  suppressMessages(library(data.table))
  suppressMessages(library(dplyr))
  suppressMessages(library(xtable))
  suppressMessages(library(car))
  suppressMessages(library(stargazer))


  y1matchct <- dplyr::filter(dataset, assignment==0)
  y1matchtx <- dplyr::filter(dataset, assignment==1)

  # Y1 #
  means_y1_ct<- data.frame(
    mean(as.numeric(y1matchct$dblack), na.rm=T),
    mean(as.numeric(y1matchct$dhispanic), na.rm=T),
    mean(as.numeric(y1matchct$ell_ever), na.rm=T),
    mean(as.numeric(y1matchct$age), na.rm=T),
    mean(as.numeric(y1matchct$dfreelunch), na.rm=T),
    mean(as.numeric(y1matchct$dlearningdisabled), na.rm=T),

    mean(as.numeric(y1matchct$dgrade9), na.rm=T),
    mean(as.numeric(y1matchct$dgrade10), na.rm=T),
    mean(as.numeric(y1matchct$gpa_pre), na.rm=T),

    mean(as.numeric(y1matchct$any_arrests_pre), na.rm=T),
    mean(as.numeric(y1matchct$all_arrests_pre), na.rm=T),
    mean(as.numeric(y1matchct$violent_pre), na.rm=T),
    mean(as.numeric(y1matchct$property_pre), na.rm=T),
    mean(as.numeric(y1matchct$drug_pre), na.rm=T),
    mean(as.numeric(y1matchct$other_pre), na.rm=T))
  mean_1 <- t(means_y1_ct)


  means_y1_tx<- data.frame(
    mean(as.numeric(y1matchtx$dblack), na.rm=T),
    mean(as.numeric(y1matchtx$dhispanic), na.rm=T),
    mean(as.numeric(y1matchtx$ell_ever), na.rm=T),
    mean(as.numeric(y1matchtx$age), na.rm=T),
    mean(as.numeric(y1matchtx$dfreelunch), na.rm=T),
    mean(as.numeric(y1matchtx$dlearningdisabled), na.rm=T),

    mean(as.numeric(y1matchtx$dgrade9), na.rm=T),
    mean(as.numeric(y1matchtx$dgrade10), na.rm=T),
    mean(as.numeric(y1matchtx$gpa_pre), na.rm=T),

    mean(as.numeric(y1matchtx$any_arrests_pre), na.rm=T),
    mean(as.numeric(y1matchtx$all_arrests_pre), na.rm=T),
    mean(as.numeric(y1matchtx$violent_pre), na.rm=T),
    mean(as.numeric(y1matchtx$property_pre), na.rm=T),
    mean(as.numeric(y1matchtx$drug_pre), na.rm=T),
    mean(as.numeric(y1matchtx$other_pre), na.rm=T))

  mean_2 <- t(means_y1_tx)

  means_allbam <- cbind(mean_1, mean_2)

  colnames(means_allbam) <- c("Control Group Mean", "Treatment Group Mean")
  rownames(means_allbam) <- c("Black", "Hispanic", "ELL","Age", "Free Lunch Recipient", "Learning Disability",
                              "In Grade 9 at Study Start", "In Grade 10 at Study Start","GPA",
                              "Ever Arrested Baseline", "Baseline Total Arrests","Violent offenses",
                              "Property offenses", "Drug offenses", "Other offenses"
  )


  runmodel <- function(I,J) {

    model <<- lm(I~assignment+
                   factor(blocknum), data=J)

    summaryofols_nr<<- summary(model)

    #Robust Standard Errors
    require("sandwich")
    require("lmtest")
    model$newse<<-vcovHC(model, type="HC1")
    summaryofols <<-coeftest(model,model$newse)

    c <- coef(model)
    c[2]
    require("arm")
    d <- summaryofols[2,2]
    #se.coef(model)
    e <-  summaryofols[,4]
    e[2]

    experiment <<- c(c[2], d, e[2])
  }

  rm(results)
  results <- data.frame(dblack=runmodel(dataset$dblack,dataset))
  results <- data.frame(results, dhispanic=runmodel(dataset$dhispanic,dataset))
  results <- data.frame(results, ell_ever=runmodel(dataset$ell_ever,dataset))

  results <- data.frame(results, age=runmodel(dataset$age,dataset))
  results <- data.frame(results, dfreelunch=runmodel(dataset$dfreelunch,dataset))
  results <- data.frame(results, dlearningdisabled=runmodel(dataset$dlearningdisabled,dataset))
  results <- data.frame(results, dgrade9=runmodel(dataset$dgrade9,dataset))
  results <- data.frame(results, dgrade10=runmodel(dataset$dgrade10,dataset))
  results <- data.frame(results, gpa_pre=runmodel(dataset$gpa_pre,dataset))

  results <- data.frame(results, any_arrests_pre=runmodel(dataset$any_arrests_pre,dataset))
  results <- data.frame(results, all_arrests_pre=runmodel(dataset$all_arrests_pre,dataset))
  results <- data.frame(results, violent_pre=runmodel(dataset$violent_pre,dataset))
  results <- data.frame(results, property_pre=runmodel(dataset$property_pre,dataset))
  results <- data.frame(results, drug_pre=runmodel(dataset$drug_pre,dataset))
  results <- data.frame(results, other_pre=runmodel(dataset$other_pre,dataset))

  balance_allbam <- t(results)

  colnames(balance_allbam) <- c("OLS Estimate", "Robust Standard Error", "P Value")
  rownames(balance_allbam) <- c("Black", "Hispanic", "ELL","Age", "Free Lunch Recipient", "Learning Disability","In Grade 9 at Study Start",
                                "In Grade 10 at Study Start", "GPA",
                                "Baseline Ever Arrested", "Baseline Total Arrests", "Violent offenses",
                                "Property offenses", "Drug offenses", "Other offenses")

  study1_balance <- cbind(means_allbam, balance_allbam)

  study1_balance <- as.data.frame(study1_balance)
  study1_balance$sig[study1_balance[,5] < .10] <- "*"
  study1_balance$sig[study1_balance[,5] < .05] <- "**"
  study1_balance$sig[study1_balance[,5] < .01] <- "***"
  study1_balance$sig[study1_balance[,5] >= .10] <- ""

  # Rounding #
  study1_balance$est_complete <- paste0(round(study1_balance[,3], 3),study1_balance[,6]," (", round(study1_balance[,4], 3), ")")
  allbam_balance_display <- study1_balance[,c(1,2,3,4,6)]

  colnames(allbam_balance_display)[5] <- "P Value"

  n1 <- assignment[1]
  n2 <- assignment[2]

  colnames(allbam_balance_display)[1] <- paste0("Control Group Mean, N=",n1)
  colnames(allbam_balance_display)[2] <- paste0("Treatment Group Mean, N=",n2)

  allbam_balance_display
}