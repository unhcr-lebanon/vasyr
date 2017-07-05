#### Generate Rmd files for each chapter

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
  cat("```", file=chapter.name , sep="\n", append=TRUE)




  chapterquestions <- dico[which(dico$chapter== chaptersname & dico$formpart=="questions"),
                           c("chapter", "name", "label", "type", "qrepeatlabel", "fullname") ]

  levels(as.factor(as.character(dico[which(!(is.na(dico$chapter)) & dico$formpart=="questions"), c("type") ])))

  for(j in 1:nrow(chapterquestions))
  {
  # j <-1
  ## Now getting level for each questions
  questions.name <- as.character(chapterquestions[ j , c("fullname")])
  questions.shortname <- as.character(chapterquestions[ j , c("name")])
  questions.type <- as.character(chapterquestions[ j , c("type")])
  questions.frame <- as.character(chapterquestions[ j , c("qrepeatlabel")])
  questions.label <- as.character(chapterquestions[ j , c("label")])

  ## write question name
  cat("\n ",file=chapter.name , sep="\n",append=TRUE)
  cat(paste("## ", questions.label ,sep=""),file=chapter.name , sep="\n", append=TRUE)

  ## Now create para based on question type

######################################################################################################
######################################################################################################
  if (questions.type =="date") {
    cat(paste("Date question \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

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
  } else if (questions.type =="select_one" ) {
    cat(paste("Single choice question \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

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




### Render the report now
#rmarkdown::render('report-tabulation.Rmd')
