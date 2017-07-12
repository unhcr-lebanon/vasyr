#######################################
### Clean and weight data         ####
#######################################

source("code/1-a.load-data.R")

################################################################
#### Check location

location <- read.csv("data/location.csv")
names(location)
names(household2)

location.vasyr <- as.data.frame(unique(household2$section1.location.pcode))
names(location.vasyr)[1] <- "P.code"

location.vasyr <- merge(x=location.vasyr, y=location, by="P.code", all.x=TRUE)
location.vasyr2 <- location.vasyr[is.na(location.vasyr$UNHCR.Location ), c("P.code", "UNHCR.Location") ]
location.vasyr2 <- location.vasyr2[!(is.na(location.vasyr2$P.code )), ]
names(location.vasyr2) <- "section1.location.pcode"
locationunsure <- as.character(location.vasyr2$section1.location.pcode)

location2.vasyr <- household2[ ,c("section1.location.pcode", "section1.location.is_pcode", "section1.location.geodata.Latitude",
                                  "section1.location.geodata.Longitude" , "section1.location.geodata.Altitude", "section1.location.geodata.Accuracy",
                                  "section1.location.soft_field1")]

location.vasyr3 <- merge(x=location.vasyr2, y=location2.vasyr, by="section1.location.pcode", all.x=TRUE )
location.vasyr.unsure <- location2.vasyr[location2.vasyr$section1.location.pcode %in%  locationunsure, ]
location.vasyr.sure <- location2.vasyr[ !(location2.vasyr$section1.location.pcode %in%  locationunsure), ]

location.vasyr333 <- unique(location.vasyr33[ ,c("section1.location.pcode", "section1.location.is_pcode", "section1.location.geodata.Latitude",
                                                "section1.location.geodata.Longitude","section1.location.soft_field1")] )


write.csv(location.vasyr333, "data/location-to-be-checked.csv")


################################################################
#### Tables with clean log
library(readxl)
Correct.CaseNo <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "Vasyr2017_CaseNo_Correction")
Correct.District <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "Vasyr2017_District_Correction")
Correct.Org <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "Vasyr2017_Org_Correction")
Correct.form <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "formid_and_correct_district")

names(Correct.CaseNo)
#  "formid"           "incorrect_caseno" "correct_caseno"
names(Correct.District)
# "caseno"           "district"         "actual_cadaster"  "assigned_cluster" "Order of Cases"   "ODK District"     "Formid"           "Same District"
names(Correct.Org)
# "formid"       "organization"
names(Correct.form)
# "formid"   "district"



################################################################
#### Weighting data

weight <- read_excel("data/weight22_06-2017-2.xlsx",  sheet = "weight2206")

## Check the merge on location
#location.vasyr.district <- as.data.frame(unique(household$section1.location.district))

#names(location.vasyr.district)[1] <- "Districts"
#location.vasyr.district$rowid <- row.names(location.vasyr.district)
#location.vasyr.district <- as.data.frame(location.vasyr.district[!(is.na(location.vasyr.district$Districts )), ])
#names(weight)
#location.vasyr.district$Districts <- as.character(location.vasyr.district$Districts)
#write.csv(location.vasyr.district, "data/locationvasyrdistrict.csv")
#str(location.vasyr.district$Districts)
#str(weight$Districts)
#weight <- merge(x=weight, y=location.vasyr.district, by="Districts", all.x=TRUE)


## Cf https://rpubs.com/trjohns/survey-cluster
## caluclate fpc i.e the number of clusters
fpc <- nrow(weight)
weight$fpc <- fpc
## Good -- let's trim the weight frame

weight2 <- weight[ c("Districts",   "Normalized.Weight","fpc")]
names(weight2)[1] <- "section1.location.district"
household <- join(x=household, y=weight2, by="section1.location.district")
#household$rowid <- row.names(household)

#table(household$Normalized.Weight)
prop.table(table(household$section1.location.district, useNA="ifany"))
## We have some NA for the district -- needs to exclude them

household2 <- household[ !(is.na(household$section1.location.district)), ]

## Now testing weighting using the survey library
library(survey)

household.survey <- svydesign(ids = ~ section1.location.district ,  data = household2 ,  weights = ~Normalized.Weight ,  fpc = ~fpc )
summary(household.survey)
svymean(~ section3_household.housing.type_of_housing, design = household.survey)

#frequ <- as.data.frame(prop.table(table(household$section3_household.housing.type_of_housing, household$section1.location.district )))
frequ <- as.data.frame(prop.table(table(household$section3_household.housing.type_of_housing )))

#frequ.weight <- as.data.frame( svyby(  ~ section3_household.housing.type_of_housing, by = ~ section1.location.district, design = household.survey, FUN = svymean  ))
frequ.weight <- as.data.frame(svymean(~ section3_household.housing.type_of_housing, design = household.survey))
frequ.weight$Var0 <- row.names(frequ.weight)
frequ.weight$Var1 <- substr(frequ.weight$Var0, regexpr("section3_household.housing.type_of_housing",frequ.weight$Var0), 10)

frequ.weight$Var1 <- substr(as.character(frequ.weight$Var0), 42, nchar(frequ.weight$Var0)-42)
nchar(frequ.weight$Var0)
