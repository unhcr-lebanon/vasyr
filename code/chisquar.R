#################################################################
### Run a series of chi squarred test on select_one and splitted select_multiple questions

## subset all select questions
#levels(dico$type)
# names(dico)

selectvariable <- dico[dico$type %in% c("select_multiple_d","select_one") & !(is.na(dico$correlate)), c("chapter", "name", "label", "type", "qrepeatlabel", "fullname","disaggregation")]
selectvariable.household <- selectvariable[selectvariable$qrepeatlabel %in% c("household"), ]
selectvariable.household1 <- as.character(selectvariable.household$fullname)

check <- as.data.frame(names(household))
names(check)[1] <- "fullname"
check$id <- row.names(check)
names(selectvariable.household)
selectdf <- join(x=selectvariable.household, y=check, by="fullname",  type="left")
selectdf <- selectdf[!is.na(selectdf$id), ]
selectvariable.household1 <- as.character(selectdf$fullname)

data <- household[ , selectvariable.household1 ]
data <- kobo_label(data)
#names(data)

# Empty frame to get results
rm(chiquare.resultall)
chiquare.resultall <- data.frame(c(1))
names(chiquare.resultall)[1] <- "id"
chiquare.resultall$target <- "target"
chiquare.resultall$tested <- "result"
chiquare.resultall$frame <- "frame"
chiquare.resultall$target.n <- 1
chiquare.resultall$tested.n <- 2
chiquare.resultall$target.label <- "target.label"
chiquare.resultall$tested.label <- "tested.label"
chiquare.resultall$p.value <- 0.999
chiquare.resultall$Chi.Square <- 0.999
chiquare.resultall$df <- 0.999
#chiquare.resultall$fisher <- 0.999

# l <- 3
for (l in 1:ncol(data) ) {
  chiquare.result <- data.frame(c(1))
  names(chiquare.result)[1] <- "id"
  chiquare.result$id <- l
  chiquare.result[1, c("target")] <- questions.name
  chiquare.result[1, c("tested")] <-  as.character(names(data)[l])
  chiquare.result[1, c("frame")]  <- questions.frame

  ## getting labels
  chiquare.result[1, c("target.n")] <-   which(colnames(get(paste0(chiquare.result$frame)))== chiquare.result[1, c("target")])
  chiquare.result[1, c("tested.n")] <-   which(colnames(get(paste0(chiquare.result$frame)))== chiquare.result[1, c("tested")])

  chiquare.result[1, c("target.label")]  <- attributes(get(paste0(chiquare.result$frame)))$variable.labels[chiquare.result[1, c("target.n")]]
  chiquare.result[1, c("tested.label")]  <- attributes(get(paste0(chiquare.result$frame)))$variable.labels[chiquare.result[1, c("tested.n")]]

  ## attributes(get(household)$variable.labels[chiquare.result[1, c("tested.n")]]

  cat(paste0(l," correlation between --",chiquare.result[1, c("target.label")],"-- and --",chiquare.result[1, c("tested.label")],"--.\n"))
  formula <- cbind(as.data.frame(get(paste0(chiquare.result$frame))[[chiquare.result$target]]), as.data.frame(get(paste0(chiquare.result$frame))[[chiquare.result$tested]]))
  names(formula)[1] <- "target"
  names(formula)[2] <- "tested"
  formula<-formula[!(is.na(formula$target)),]
  formula<-formula[!(is.na(formula$tested)),]

  ### Testing number of levels for the 2 variables as 'x' and 'y' must have at least 2 levels
  if ( (chiquare.result[1, c("target")]!=chiquare.result[1, c("tested")] ) &
       (nlevels(as.factor(as.character(formula$target))) >1 ) &
       (nlevels(as.factor(as.character(formula$tested))) >1 ) &
       (nlevels(as.factor(as.character(formula$target))) < 8 ) &
       (nlevels(as.factor(as.character(formula$tested))) < 8 ) )
  { chiquare.result[1, c("p.value")]  <- round(chisq.test(formula$target,formula$tested)$p.value,4)
    chiquare.result[1, c("Chi.Square")]  <- round(chisq.test(formula$target,formula$tested)$statistic,4)
    chiquare.result[1, c("df")]  <- chisq.test(formula$target,formula$tested)$parameter
  # chiquare.result[1, c("fisher")]  <- round(fisher.test(formula$target,formula$tested)$parameter,3)
  } else {
    chiquare.result[1, c("p.value")] <- 1
    chiquare.result[1, c("Chi.Square")] <- 1
    chiquare.result[1, c("df")] <- 1
  # chiquare.result[1, c("fisher")]  <- 1
  }

  chiquare.resultall <- rbind(chiquare.resultall, chiquare.result)
  rm(chiquare.result)
}

## Subsetting results on test where pvvalue is below 0.05
chiquare.true <- chiquare.resultall[ chiquare.resultall$p.value <= 0.05, ]

## now generating correlation plot for each of the dependent.
## https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

library(corrplot)

for (i in 1:nrow(chiquare.true)) {
  # i<-2
  frame<- chiquare.true[i,4 ]
  target <- chiquare.true[i,2 ]
  tested <- chiquare.true[i,3 ]
  target.label <- chiquare.true[i,7]
  tested.label <- chiquare.true[i,8 ]
  formula.target    <-get(paste0(frame))[[target]]
  formula.tested    <-get(paste0(frame))[[tested]]
 # png(height=1200, width=1200, pointsize=25, file=paste0("out/corrplot-",i,".png") )
  png(file=paste0("out/corrplot-",i,".png") )
  corrplot(chisq.test(formula.target, formula.tested)$residuals, is.cor = FALSE,
           cl.pos="n", ## Do not display the color legend
           tl.cex = 0.7, ## Size of axis label
           mar=c(1,1,4,1), ## margin of plots
         #  order ="hclust", ## How records are ordered hierarchicla clustering
          # addrect=2, ##draw rectangles around the chart of corrrlation matrix based on the results of hierarchical clustering
           title=paste0("Correlation between \n \"",target.label ,"\" \n  & \"",tested.label, "\""  ), ## Title
           diag= FALSE)
  dev.off()
}

