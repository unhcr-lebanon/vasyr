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

#
rm(chiquare.resultall)
chiquare.resultall <- data.frame(c(1))
names(chiquare.resultall)[1] <- "id"
chiquare.resultall$target <- "target"
chiquare.resultall$tested <- "result"
chiquare.resultall$p.value <- 0.999
chiquare.resultall$frame <- "frame"

## 'x' and 'y' must have at least 2 levels
question.name <- "section3_household.housing.rent_period"
question.frame <- "section3_household.housing.rent_period"

nlevels(data[ ,question.target])
#ncol(data[ ,question.target])
#names(data)
for (l in 2:nrow(data) ) {
  #l<- 14
  if (nlevels(data[ ,l])>=2){
  cat(paste0(l,"\n"))
  chiquare.result <- data.frame(c(1))
  names(chiquare.result)[1] <- "id"
  chiquare.result$id <- l
  chiquare.result$target <- question.name
  chiquare.result$tested <-  as.character(names(data)[l])
  if (chiquare.result$target==chiquare.result$tested){ chiquare.result$p.value <- 1 }
  else{ chiquare.result$p.value <- chisq.test(data[ ,15], data[ ,l])$p.value}

  chiquare.result$frame <- "household"
  chiquare.resultall <- rbind(chiquare.resultall, chiquare.result)
  } else { cat("Not enough levels \n")}
}


## Subsetting results on test where pvvalue is below 0.05
chiquare.true <- chiquare.resultall[ chiquare.resultall$p.value <= 0.05, ]

## now generating correlation plot for each of the dependent.
## https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

library(corr)

for (i in 2:nrow(chiquare.true)) {
  # i<-2
  frame<- chiquare.true[i,5 ]
  target <- chiquare.true[i,2 ]
  tested <- chiquare.true[i,3 ]
  formula.target    <-get(paste0(frame))[[target]]
  formula.tested    <-get(paste0(frame))[[tested]]
  png(height=1200, width=1200, pointsize=25, file=paste0("out/corrplot-",i,".png") )
 # png(file=paste0("out/corrplot-",i,".png") )
  corrplot(chisq.test(formula.target, formula.tested)$residuals, is.cor = FALSE,
           cl.pos="n", ## Do not display the color legend
           tl.cex = 0.7, ## Size of axis label
           mar=c(1,1,1,1), ## margin of plots
         #  order ="hclust", ## How records are ordered hierarchicla clustering
          # addrect=2, ##draw rectangles around the chart of corrrlation matrix based on the results of hierarchical clustering
           title="Correlation Plot", ## Title
           diag= FALSE)
  dev.off()
}

