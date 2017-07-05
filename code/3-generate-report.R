#### Generate Rmd files for each chapter


## Load the form

mainDir <- getwd()
## Load all required packages
source(paste0(mainDir,"/code/0-packages.R"))
library(koboloadeR)
source("/home/edouard/R-project/koboloadeR/R/kobo_dico.R")

##############################################
## Load form


cat("\n\n Build dictionnary from the xlsform \n")

rm(form)
form <- "form.xls"
## Generate & Load dictionnary
kobo_dico(form)
dico <- read.csv(paste("data/dico_",form,".csv",sep=""), encoding="UTF-8", na.strings="")
rm(form)


### Get the dico with list of chapter

chapters <- as.data.frame(unique(dico$chapter))
names(chapters)[1] <- "Chapter"
chapters <- as.data.frame(chapters[!is.na(chapters$Chapter), ])
#for ()
## for each chapter: create a Rmd file
for(i in 1:nrow(chapters))
{
  # i <-1
  chaptersname <- as.character(chapters[ i , 1])
  cat(paste(i, " - Render chapter for ",as.character(chapters[ i , 1]),"\n" ))
  chapter.name <- paste("code/report/",i,"-", chaptersname, "-chapter.Rmd", sep="")

  ## Get the

  cat("---", file=chapter.name , sep="\n", append=TRUE)
  cat(paste("title: \"Preliminary exploration of results for Chapter: ",chaptersname , "- Draft not for distribution. \"", sep=""), file=chapter.name ,sep="\n", append=TRUE)
  cat("author: \"Prepared by UNHCR DOiA\"", file=chapter.name ,sep="\n", append=TRUE)
  cat("date: \"Amman, prepared on the `r format(Sys.Date(),  '%d %B %Y')`\"", file=chapter.name ,sep="\n", append=TRUE)
  cat("output:",file=chapter.name ,sep="\n", append=TRUE)
  cat("  word_document:", file=chapter.name , sep="\n", append=TRUE)
  cat("    fig_caption: yes", file=chapter.name , sep="\n", append=TRUE)
  cat("    fig_height: 5", file=chapter.name , sep="\n", append=TRUE)
  cat("    fig_width: 8", file=chapter.name , sep="\n", append=TRUE)
  cat("    toc: yes", file=chapter.name , sep="\n", append=TRUE)
  cat("    toc_depth: 2", file=chapter.name , sep="\n", append=TRUE)
  cat("    reference_docx: style-unhcr-portrait.docx", file=chapter.name , sep="\n", append=TRUE)
  cat("---", file=chapter.name , sep="\n", append=TRUE)

  cat(paste("# Compilation of questions Results"),file=chapter.name ,sep="\n", append=TRUE)

  ## First chunk to get the data in the report

  cat("```{r setup, include=FALSE, echo=FALSE, warning=FALSE, message=FALSE}", file=chapter.name , sep="\n", append=TRUE)
  cat("mainDir <- getwd()", file=chapter.name , sep="\n", append=TRUE)
  cat("mainDirroot <- substring(mainDir, 0 , nchar(mainDir)- 12)", file=chapter.name , sep="\n", append=TRUE)
  cat("## Load all required packages", file=chapter.name , sep="\n", append=TRUE)
  cat("source(paste0(mainDirroot,\"/code/0-packages.R\"))", file=chapter.name , sep="\n", append=TRUE)
  cat("library(koboloadeR)", file=chapter.name , sep="\n", append=TRUE)
  cat("## Provide below the name of the form in xsl form - format should be xls not xlsx", file=chapter.name , sep="\n", append=TRUE)
  cat("form <- \"form.xls\"", file=chapter.name , sep="\n", append=TRUE)
  cat("dico <- read.csv(paste0(mainDirroot,\"/data/dico_\",form,\".csv\"), encoding=\"UTF-8\", na.strings=\"\")", file=chapter.name , sep="\n", append=TRUE)

  cat("household <- read.csv(paste0(mainDirroot,\"/data/household.csv\"), encoding=\"UTF-8\", na.strings=\"\")", file=chapter.name , sep="\n", append=TRUE)
  cat("case_number_details <- read.csv(paste0(mainDirroot,\"/data/case_number_details.csv\"), encoding=\"UTF-8\", na.strings=\"\")", file=chapter.name , sep="\n", append=TRUE)
  cat("individual_biodata <- read.csv(paste0(mainDirroot,\"/data/individual_biodata.csv\"), encoding=\"UTF-8\", na.strings=\"\")", file=chapter.name , sep="\n", append=TRUE)

  cat("## label Variables", file=chapter.name , sep="\n", append=TRUE)
  cat("household <- kobo_label(household , dico)", file=chapter.name , sep="\n", append=TRUE)
  cat("case_number_details <- kobo_label(case_number_details , dico)", file=chapter.name , sep="\n", append=TRUE)
  cat("individual_biodata <- kobo_label(individual_biodata , dico)", file=chapter.name , sep="\n", append=TRUE)

  cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)


  chapterquestions <- dico[which(dico$chapter== chaptersname & dico$formpart=="questions"),
                           c("chapter", "name", "label", "type", "qrepeatlabel", "fullname") ]

  #levels(as.factor(as.character(dico[which(!(is.na(dico$chapter)) & dico$formpart=="questions"), c("type") ])))

  for(j in 1:nrow(chapterquestions))
  {
   #j <-3
  ## Now getting level for each questions
  questions.name <- as.character(chapterquestions[ j , c("fullname")])
  questions.shortname <- as.character(chapterquestions[ j , c("name")])
  questions.type <- as.character(chapterquestions[ j , c("type")])
  questions.frame <- as.character(chapterquestions[ j , c("qrepeatlabel")])
  questions.label <- as.character(chapterquestions[ j , c("label")])
  questions.variable <- paste0(questions.frame,"$",questions.name)
  ## write question name
  cat("\n ",file=chapter.name , sep="\n",append=TRUE)
  cat(paste("## ", questions.label ,sep=""),file=chapter.name , sep="\n", append=TRUE)

  ## Now create para based on question type

######################################################################################################
######################################################################################################
  if (questions.type =="select_one" ) {
    cat(paste("Single choice question \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ##############################################################################

    cat(paste("### Tabulation" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n"), file=chapter.name, append=TRUE)

    cat(paste("### Tabulation" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("##Compute contengency table"),file=chapter.name ,sep="\n",append=TRUE)


    cat(paste0("frequ <- table(",questions.variable,")"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0(questions.variable," <- factor(",questions.variable,", levels=names(frequ[order(frequ, decreasing = TRUE)]))"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("totalanswer <- nrow(",questions.frame,")"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("## subsetting to those who replied"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0(questions.frame,"1 <- ",questions.frame,"[ !(is.na(",questions.variable,")), ]"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("percentreponse <- paste0(round((nrow(",questions.frame,"1)/totalanswer)*100,digits=1),\"%\")"),file=chapter.name ,sep="\n",append=TRUE)

    questions.variable2 <- paste0(questions.frame,"1$",questions.name)

    cat(paste0("## and now the graph"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("ggplot(",questions.frame,"1, aes(",questions.variable2,")) +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))),"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("fill=\"#2a87c8\",colour=\"#2a87c8\") +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("guides(fill=FALSE) +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("ylab(\"Frequency\") +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("scale_y_continuous(labels=percent)+"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("xlab(\"\") +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("coord_flip() +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("ggtitle(\"",questions.label,"\","),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("subtitle = paste0(\"Select_one question: Response rate to this question is \",percentreponse,\" of the total.\")) +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("theme(plot.title=element_text(face=\"bold\", size=9),"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("plot.background = element_rect(fill = \"transparent\",colour = NA))"),file=chapter.name ,sep="\n",append=TRUE)






    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################

    cat(paste("### Chart" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".chart , echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################

    cat(paste("### Map" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".map , echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################

    cat(paste("### Crosstabulation" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".cross , echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################

    cat(paste("### Analysis of relationship" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)


######################################################################################################
######################################################################################################
  } else if (questions.type =="decimal" | questions.type =="integer" ) {
    cat(paste("Numeric question \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ##############################################################################
    cat(paste("### Tabulation" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)

    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################

    cat(paste("### Chart" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".chart , echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################

    cat(paste("### Map" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".map , echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################

    cat(paste("### Crosstabulation" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".cross , echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################

    cat(paste("### Analysis of relationship" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)


######################################################################################################
######################################################################################################
  } else if ( questions.type =="select_multiple_d" ) {
    cat(paste("Multiple choice question \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ##############################################################################

    cat(paste("### Tabulation" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################

    cat(paste("### Chart" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".chart , echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)

    ##############################################################################

    cat(paste("### Map" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".map , echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################

    cat(paste("### Crosstabulation" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".cross , echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################

    cat(paste("### Analysis of relationship" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, fig.height=4}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)



######################################################################################################
######################################################################################################
  } else if (questions.type =="date") {
    cat(paste("Date question \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

######################################################################################################
######################################################################################################
  } else if ( questions.type =="text" ) {
    cat(paste("Open ended question \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

  # End test on question on type
  }

  ## End loop on questions
  }


# Write the reference to the chapter in the main report file
#cat(paste0("\n```{r child = '",i,"-", as.character(chapters[ i , 1]), "-chapter.Rmd", "'}\n```\n"), sep = '\n',file="code/report/report-tabulation.Rmd",append=TRUE)
# End chapter
}

  #rmd <- list.files(pattern = '*-chapter.Rmd', recursive = T, include.dirs = T)
  #chunks <- paste0("\n```{r child = '", rmd, "'}\n```\n")
  #cat(chunks, sep = '\n')
## Inser chapter child Rmd in the report-tabulation.Rmd


#```{r child = 'chapter1.Rmd'}
#```




### Render now all reports

cat(" Render now reports... \n")


#rmarkdown::render('report-tabulation.Rmd')

cat(" Done!! Reports are in the folder CODE > REPORT - You are now ready to start the qualitative analysis and the analysis workshops...")
