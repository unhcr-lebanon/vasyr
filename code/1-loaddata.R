################################################################
## Load all required packages
#source('/code/0-packages.R')
## delete packages
#remove.packages("koboloadeR")
### Double Check that you have the last version
#source("https://raw.githubusercontent.com/Edouard-Legoupil/koboloadeR/master/inst/script/install_github.R")
#install.packages("devtools")
#library("devtools")
#install_github("Edouard-Legoupil/koboloadeR")


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





#################################################################################################################################
## Load all frames
#################################################################################################################################
library(readr)
household <- read_csv("data/Vasyr2017_Live_7.csv")
case_number_details <- read_csv("data/Vasyr2017_Live_7_section2_case_number_details.csv")
individual_biodata <- read_csv("data/Vasyr2017_Live_7_case_number_individuals_individual_biodata.csv")
difficulties_encountered <- read_csv("data/Vasyr2017_Live_7_section7_critical_info_hh_difficulties_encountered.csv")
illegal_residence <- read_csv("data/Vasyr2017_Live_7_section7_critical_info_hh_illegal_residence.csv")
legal_residence <- read_csv("data/Vasyr2017_Live_7_section7_critical_info_hh_legal_residence.csv")
moved_returnee <- read_csv("data/Vasyr2017_Live_7_section7_critical_info_hh_moved_returnee.csv")

##################################################################
###### Restore links between frame
## household - case_number_details
## household - case_number_details - individual_biodata
## household -illegal_residence
## household -difficulties_encountered
## household -legal_residence
## household -moved_returnee



#################################################################################################################################
## Household
## fEW CHECK
#names(household)
#table(household$case_reachable-reachable)
# nrow(as.data.frame(unique(household$meta-instanceID)))
# nrow(as.data.frame(unique(household$KEY)))

cat("\n\nCheck Household\n")
household <- household
datalabel <- as.data.frame( names(household))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)
datalabel$namenew <- str_replace_all(datalabel$nameor, "-", ".")
names(household) <- datalabel[, 2]

## Remove rows for "not reachable"
#table(household[ ,8])
#str(household)
#names(household)
#str(household$case_reachable.reachable)
#levels(as.factor(household$case_reachable.reachable))
household <- household[which(household$case_reachable.reachable =="yes"), ]
# names(household)



#################################################################################################################################
## Case

cat("\n\nCheck cases\n")
case_number_details <- case_number_details
datalabel <- as.data.frame( names(case_number_details))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)
datalabel$namenew <- str_replace_all(datalabel$nameor, "-", ".")
datalabel$namenew<- paste("section2.case_number_details.", datalabel$namenew, sep="")
names(case_number_details) <- datalabel[, 2]

## merge
#names(case_number_details)
#levels(as.factor(household$SET.OF.section2.case_number_details))
case_number_details$SET.OF.section2.case_number_details <- case_number_details$section2.case_number_details.SET.OF.case_number_details
case_number_details <- join(y= household, x= case_number_details, by="SET.OF.section2.case_number_details", type="left")

#################################################################################################################################
## Bio Data
# names(individual_biodata)
cat("\n\nCheck individuals\n")
individual_biodata <- individual_biodata
datalabel <- as.data.frame( names(individual_biodata))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)
datalabel$namenew <- str_replace_all(datalabel$nameor, "-", ".")
datalabel$namenew<- paste("section2.case_number_details.case_number_individuals.individual_biodata.", datalabel$namenew, sep="")
names(individual_biodata) <- datalabel[, 2]

#names(case_number_details)
#names(individual_biodata)
individual_biodata$section2.case_number_details.SET.OF.case_number_individuals.individual_biodata <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.SET.OF.individual_biodata
individual_biodata <- join(y= case_number_details, x= individual_biodata, by="section2.case_number_details.SET.OF.case_number_individuals.individual_biodata", type="left")
#names(individual_biodata)
#levels(as.factor(individual_biodata$section7.communication.social_media))

#################################################################################################################################
##
cat("\n\nCheck difficulties\n")

difficulties_encountered <- difficulties_encountered
datalabel <- as.data.frame( names(difficulties_encountered))
names(datalabel)[1] <- "nameor"
#str(datalabel$nameor)
#levels(datalabel$nameor)
datalabel$nameor <- as.character(datalabel$nameor)
datalabel$namenew <- str_replace_all(datalabel$nameor, "-", ".")
datalabel$namenew<- paste("section7.critical_info_hh.difficulties_encountered.", datalabel$namenew, sep="")
names(difficulties_encountered) <- datalabel[, 2]
#levels(as.factor(household$SET.OF.section7.critical_info_hh.difficulties_encountered))
#names(difficulties_encountered)
difficulties_encountered$SET.OF.section7.critical_info_hh.difficulties_encountered <- difficulties_encountered$section7.critical_info_hh.difficulties_encountered.SET.OF.difficulties_encountered
difficulties_encountered <- join(y= household, x= difficulties_encountered, by="SET.OF.section7.critical_info_hh.difficulties_encountered", type="left")



#################################################################################################################################
##
cat("\n\nCheck illegal_residence\n")
illegal_residence <- illegal_residence
datalabel <- as.data.frame( names(illegal_residence))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)
datalabel$namenew <- str_replace_all(datalabel$nameor, "-", ".")
datalabel$namenew<- paste("section7.critical_info_hh.illegal_residence.", datalabel$namenew, sep="")
names(illegal_residence) <- datalabel[, 2]
## next table
#levels(as.factor(household$SET.OF.section7.critical_info_hh.illegal_residence))
illegal_residence$SET.OF.section7.critical_info_hh.illegal_residence <- illegal_residence$section7.critical_info_hh.illegal_residence.SET.OF.illegal_residence
illegal_residence <- join(y= household, x= illegal_residence, by="SET.OF.section7.critical_info_hh.illegal_residence", type="left")

#################################################################################################################################
##
cat("\n\nCheck legal_residence\n")

legal_residence <- legal_residence
datalabel <- as.data.frame( names(legal_residence))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)
datalabel$namenew <- str_replace_all(datalabel$nameor, "-", ".")
datalabel$namenew<- paste("section7.critical_info_hh.legal_residence.", datalabel$namenew, sep="")
names(legal_residence) <- datalabel[, 2]
## next table
#levels(as.factor(household$SET.OF.section7.critical_info_hh.legal_residence))
legal_residence$SET.OF.section7.critical_info_hh.legal_residence <- legal_residence$section7.critical_info_hh.legal_residence.SET.OF.legal_residence
legal_residence <- join(y= household, x= legal_residence , by="SET.OF.section7.critical_info_hh.legal_residence", type="left")

#################################################################################################################################
##
cat("\n\nCheck moved_returnee\n")

moved_returnee <- moved_returnee
datalabel <- as.data.frame( names(moved_returnee))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)
datalabel$namenew <- str_replace_all(datalabel$nameor, "-", ".")
datalabel$namenew<- paste("section7.critical_info_hh.moved_returnee.", datalabel$namenew, sep="")
names(moved_returnee) <- datalabel[, 2]
## next table
#levels(as.factor(household$SET.OF.section7.critical_info_hh.moved_returnee))
moved_returnee$SET.OF.section7.critical_info_hh.moved_returnee <- moved_returnee$section7.critical_info_hh.moved_returnee.SET.OF.moved_returnee
moved_returnee <- join(y= household, x= moved_returnee, by="SET.OF.section7.critical_info_hh.moved_returnee", type="left")




##################################################################

#################################################################################

cat("\n\nCheck matching of variable names with generated dictionnary\n\n")


datalabel.household <- as.data.frame( names(household))
names(datalabel.household)[1] <- "fullname"
datalabel.household$frame <- "household"

datalabel.case_number_details <- as.data.frame( names(case_number_details))
names(datalabel.case_number_details)[1] <- "fullname"
datalabel.case_number_details$frame <- "case_number_details"

datalabel.individual_biodata <- as.data.frame( names(individual_biodata))
names(datalabel.individual_biodata)[1] <- "fullname"
datalabel.individual_biodata$frame <- "individual_biodata"

datalabel.difficulties_encountered <- as.data.frame( names(difficulties_encountered))
names(datalabel.difficulties_encountered)[1] <- "fullname"
datalabel.difficulties_encountered$frame <- "difficulties_encountered"

datalabel.illegal_residence <- as.data.frame( names(illegal_residence))
names(datalabel.illegal_residence)[1] <- "fullname"
datalabel.illegal_residence$frame <- "illegal_residence"

datalabel.legal_residence <- as.data.frame( names(legal_residence))
names(datalabel.legal_residence)[1] <- "fullname"
datalabel.legal_residence$frame <- "legal_residence"

datalabel.moved_returnee <- as.data.frame( names(moved_returnee))
names(datalabel.moved_returnee)[1] <- "fullname"
datalabel.moved_returnee$frame <- "moved_returnee"

datalabel <- rbind(datalabel.household,
                   datalabel.case_number_details,
                   datalabel.individual_biodata ,
                   datalabel.difficulties_encountered,
                   datalabel.illegal_residence,
                   datalabel.legal_residence ,
                   datalabel.moved_returnee)

datalabel.cast <- dcast(datalabel, fullname ~ frame)
datalabel.cast1 <- join(x=datalabel.cast , y=dico, by="fullname", type="left")
write.csv(datalabel.cast1, "data/datalabelcast1.csv")


rm(datalabel, datalabel.cast, datalabel.cast1,
      datalabel.household,
      datalabel.case_number_details,
      datalabel.individual_biodata ,
      datalabel.difficulties_encountered,
      datalabel.illegal_residence,
      datalabel.legal_residence ,
      datalabel.moved_returnee)


###################################################################################
##### Re-encode correctly the dataset
cat("\n\n\nNow re-encode data and label variables \n\n\n\n")

household <- kobo_encode(household, dico)
household <- kobo_label(household , dico)

##### Re-encode correctly the dataset
case_number_details <- kobo_encode(case_number_details, dico)
case_number_details <- kobo_label(case_number_details , dico)

##### Re-encode correctly the dataset
individual_biodata <- kobo_encode(individual_biodata, dico)
individual_biodata <- kobo_label(individual_biodata , dico)


cat("\n\nWrite backup\n")

write.csv(household, "data/household.csv")
write.csv(case_number_details, "data/case_number_details.csv")
write.csv(individual_biodata , "data/individual_biodata.csv")
write.csv(difficulties_encountered, "data/difficulties_encountered.csv")
write.csv(illegal_residence, "data/illegal_residence.csv")
write.csv(legal_residence , "data/legal_residence.csv")
write.csv(moved_returnee, "data/moved_returnee.csv")
