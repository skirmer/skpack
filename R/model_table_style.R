#Function for Building Sweave Tables in Crime Lab Style

#' Produce ITT and TOT tables in Crime Lab style.
#'
#'
#' @export
model_table_style <- function(tablename, colpositions, titletext="Table", footnotetext=""){

  tablename <- as.data.frame(tablename[,colpositions])
  tablename$outcomename <- rownames(tablename)

  #Label for significance #
  tablename$itt_sig[tablename[,5] < .10] <- "*"
  tablename$itt_sig[tablename[,5] < .05] <- "**"
  tablename$itt_sig[tablename[,5] < .01] <- "***"
  tablename$itt_sig[tablename[,5] >= .10] <- ""

  tablename$tot_sig[tablename[,8] < .10] <- "*"
  tablename$tot_sig[tablename[,8] < .05] <- "**"
  tablename$tot_sig[tablename[,8] < .01] <- "***"
  tablename$tot_sig[tablename[,8] >= .10] <- ""

  #Produce the estimate fields that will go into the table
  # data.table is not working in the function so other arrangements have been made - need to solve this.
  # Rounding #
#   roundme <- colnames(tablename[,c(3, 4, 5, 6, 7, 8, 9, 10)])
#   tablename2 <- data.table::as.data.table(tablename, keep.rownames = FALSE)
#   for(var in roundme) tablename2[, (var) := round(get(var), 3)] #Round the values to three decimals
#   tablename <- as.data.frame(tablename2)
#

  # ITT Style #
  tablename$itt_est <- paste0(round(tablename[,3],3), tablename[,"itt_sig"])
  tablename$tot_est <- paste0(round(tablename[,6],3), tablename[,"tot_sig"])
  tablename$se_line <- paste0("\\\\\n & & & (", round(tablename[,4],3),") & (", round(tablename[,7],3), ")")

  display_table <- tablename[,c(11, 1, 2, 14, 15, 9, 10, 16)]

  colnames(display_table) <- c("title","N", "Control Mean", "Intention to Treat", "Effect of Participation (IV)", "Control Complier Mean", "FDR Q-Value", " ")
  rownames(display_table) <- display_table[,1]
  display_table <- display_table[,-1]

footnotewords <- str_split(footnotetext, " ")

  #Calculate how many rows of footnote you'll need, roughly
  footnotelength <- ceiling(length(footnotewords[[1]])/21)

  #A line in the footnote should be no more than about 130 characters, so this is meant to get the length about right and do the splits neatly.
  build_footnotes <- function(footnotewords, footnotelength){
    if(footnotelength > 0){
    first_footnote_line <<- ifelse(sum(nchar(footnotewords[[1]][1:20])) < 100, paste(footnotewords[[1]][1:21], collapse = " "),
                                  paste(footnotewords[[1]][1:20], collapse = " "))
    }

    if(footnotelength == 1){
      first_footnote_line <<- paste(footnotewords[[1]], collapse = " ")
    }

    if(footnotelength > 1){
      remaining_footnote <- sub(first_footnote_line, "", footnotetext, fixed=TRUE)
      rfootnotewords <- str_split(remaining_footnote, " ")
      second_footnote_line <<- ifelse(sum(nchar(rfootnotewords[[1]][1:20])) < 105, paste(rfootnotewords[[1]][1:21], collapse = " "),
                                     paste(rfootnotewords[[1]][1:20], collapse = " "))
    }

    if(footnotelength == 2){
      remaining_footnote <- sub(first_footnote_line, "", footnotetext, fixed=TRUE)
      rfootnotewords <- str_split(remaining_footnote, " ")
      second_footnote_line <<- paste(rfootnotewords[[1]], collapse = " ")
    }

    if(footnotelength > 2){
      thirdset_footnote <- sub(second_footnote_line, "", remaining_footnote, fixed=TRUE)
      thirdfootnotewords <- str_split(thirdset_footnote, " ")
      third_footnote_line <<- ifelse(sum(nchar(thirdfootnotewords[[1]][1:20])) < 105, paste(thirdfootnotewords[[1]][1:21], collapse = " "),
                                      paste(thirdfootnotewords[[1]][1:20], collapse = " "))
    }

    if(footnotelength == 3){
      thirdset_footnote <- sub(second_footnote_line, "", remaining_footnote, fixed=TRUE)
      thirdfootnotewords <- str_split(thirdset_footnote, " ")
      third_footnote_line <<- paste(thirdfootnotewords[[1]], collapse = " ")
    }

    if(footnotelength > 3){
      fourthset_footnote <- sub(third_footnote_line, "", thirdset_footnote, fixed=TRUE)
      fourthfootnotewords <- str_split(fourthset_footnote, " ")
      fourth_footnote_line <<- ifelse(sum(nchar(fourthfootnotewords[[1]][1:20])) < 105, paste(fourthfootnotewords[[1]][1:21], collapse = " "),
                                     paste(fourthfootnotewords[[1]][1:20], collapse = " "))
    }

    if(footnotelength == 4){
      fourthset_footnote <- sub(third_footnote_line, "", thirdset_footnote, fixed=TRUE)
      fourthfootnotewords <- str_split(fourthset_footnote, " ")
      fourth_footnote_line <<- paste(fourthfootnotewords[[1]], collapse = " ")
    }

    if(footnotelength > 4){
      fifthset_footnote <- sub(fourth_footnote_line, "", fourthset_footnote, fixed=TRUE)
      fifthfootnotewords <- str_split(fifthset_footnote, " ")
      fifth_footnote_line <<- paste(fifthfootnotewords[[1]], collapse = " ")
    }
  }
  build_footnotes(footnotewords, footnotelength)

  first_footnote_line <- ifelse(first_footnote_line != "", paste0("Note: ", first_footnote_line), first_footnote_line)
  second_footnote_line <- ifelse(footnotelength > 1, second_footnote_line, " ")
  third_footnote_line <- ifelse(footnotelength > 2, third_footnote_line, " ")
  fourth_footnote_line <- ifelse(footnotelength > 3, fourth_footnote_line, " ")
  fifth_footnote_line <- ifelse(footnotelength > 4, fifth_footnote_line, " ")

  print(xtable(display_table,
               align=c("L{4.6cm}","C{.7cm}","C{1.2cm}","C{1.9cm}","C{1.9cm}","C{2.5cm}","C{1.5cm}","C{0.01cm}"),
               caption=titletext,
               digits=c(0,0,3,0,3,3,3,3), booktabs=TRUE, include.rownames=TRUE, format.args=list(big.mark=",")),
        floating=TRUE,add.to.row=list(pos=list(0, nrow(display_table)),
                                      command=c("\n \\hline \n \\multicolumn{3}{l}{Outcomes} \\\\\n",
                                                paste0('\n \\hline', '\n ','\\multicolumn{7}{l}{',first_footnote_line,' } \\\\\n',
                                                       '\\multicolumn{7}{l}{',second_footnote_line,' } \\\\\n ',
                                                       '\\multicolumn{7}{l}{',third_footnote_line,' } \\\\\n ',
                                                       '\\multicolumn{7}{l}{',fourth_footnote_line,' } \\\\\n ',
                                                       '\\multicolumn{7}{l}{',fifth_footnote_line,'} \\\\\n'))),
        size="\\scriptsize", sanitize.text.function=identity, caption.placement="top",table.placement="!h",# floating.environment="sidewaystable",
        hline.after=c(-1,0))

}

