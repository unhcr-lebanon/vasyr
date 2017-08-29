#######################################
### Clean and weight data         ####
#######################################

source("code/1-a.load-data.R")


################################################################
#### Change age from day to years - rounded to the year..

#names(individual_biodata)
individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age <- round(individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age/365, digits = 0)
#table(individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age)

################################################################
#### Check location - fill with GPS when not available based on P-code

location <- read.csv("data/location.csv")
names(location)[1] <- "section1.location.pcode"
location.add <- location[ , c("section1.location.pcode", "Latitude", "Longitude")]

#names(location)
#names(household)

#location.vasyr <- as.data.frame(unique(household2$section1.location.pcode))
household.back <- household
household <- merge(x=household, y=location.add, by="section1.location.pcode", all.x=TRUE)

household$lat <- household$section1.location.geodata.Latitude
household$long <- household$section1.location.geodata.Longitude
#View(household[ ,c("section1.location.geodata.Longitude","section1.location.geodata.Latitude","Longitude","Latitude","long","lat")])

for (i in 1:nrow(household)) {
  # i <-137
  if (is.na(household[i, c("lat")]))
  {household[i, c("lat")]  <- as.numeric(as.character((household[i, c("Latitude")]))) }
  else { }
  # else {household[i, c("lat")]  <- household[i, c("lat")]}

  if (is.na(household[i, c("long")]))
  {household[i, c("long")] <- as.numeric(as.character(household[i, c("Longitude")]))}
  else { }
  # else {household[i, c("long")] <- household[i, c("long")]}
}
#View(household[ ,c("section1.location.geodata.Longitude","section1.location.geodata.Latitude","Longitude","Latitude","long","lat")])

household.nolocation <- household[ is.na(household$lat), c("section1.location.district", "section1.location.cluster_number", "section1.location.pcode", "section1.location.is_pcode", "section1.location.geodata.Latitude",
                                                            "section1.location.geodata.Longitude" , "section1.location.geodata.Altitude", "section1.location.geodata.Accuracy",
                                                            "section1.location.soft_field1")]
write.csv(household.nolocation, "data/location-to-be-checked.csv")

### Dataset with
#household <- household[ !(is.na(household$lat)), ]

#source("code/Chek-with-maps.R")

################################################################
#### Tables with clean log
library(readxl)
Correct.CaseNo <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "Vasyr2017_CaseNo_Correction")
Correct.District <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "Vasyr2017_District_Correction")
Correct.Org <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "Vasyr2017_Org_Correction")
Correct.form <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "formid_and_correct_district")

#names(Correct.CaseNo)
#  "formid"           "incorrect_caseno" "correct_caseno"
#names(Correct.District)
# "caseno"           "district"         "actual_cadaster"  "assigned_cluster" "Order of Cases"   "ODK District"     "Formid"           "Same District"
#names(Correct.Org)
# "formid"       "organization"
#names(Correct.form)
# "formid"   "district"

### Remove FormIDs to be dropped 
# Duplicate visits to be dropped from cleaning file AND further FormIDs also not in WFP final dataset

drop.form <- read_excel("data/erorr_and_correction_tables.xlsx", sheet = "duplicate_visit_drop")
household <- merge(x=household, y=drop.form, by="KEY", all.x=TRUE)
household <- household[ (is.na(household$to_delete)), ]

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
## calculate fpc i.e the number of clusters that should be used to build the survey object
fpc <- nrow(weight)
weight$fpc <- fpc

## Good -- let's trim the weight frame
weight2 <- weight[ c("Districts",   "Normalized.Weight","fpc")]
names(weight2)[1] <- "section1.location.district"
household <- join(x=household, y=weight2, by="section1.location.district")
case_number_details <- join(x=case_number_details, y=weight2, by="section1.location.district")
individual_biodata <- join(x=individual_biodata, y=weight2, by="section1.location.district")
#household$rowid <- row.names(household)

#table(household$Normalized.Weight)
#prop.table(table(household$section1.location.district, useNA="ifany"))
## We have some NA for the district -- needs to exclude them

household <- household[ !(is.na(household$section1.location.district)), ]
case_number_details <- case_number_details[ !(is.na(case_number_details$section1.location.district)), ]
individual_biodata <- individual_biodata[ !(is.na(individual_biodata$section1.location.district)), ]

## Now testing weighting using the survey library
#library(survey)

## Survey design follows one-stage modality due to sampling with population proportional to size in the first stage
#household.survey <- svydesign(ids = ~ section1.location.district ,  data = household2 ,  weights = ~Normalized.Weight ,  fpc = ~fpc )
#summary(household.survey)
#svymean(~ section3_household.housing.type_of_housing, design = household.survey)

#frequ <- as.data.frame(prop.table(table(household$section3_household.housing.type_of_housing, household$section1.location.district )))
#frequ <- as.data.frame(prop.table(table(household$section3_household.housing.type_of_housing )))

#frequ.weight <- as.data.frame( svyby(  ~ section3_household.housing.type_of_housing, by = ~ section1.location.district, design = household.survey, FUN = svymean  ))
#frequ.weight <- as.data.frame(svymean(~ section3_household.housing.type_of_housing, design = household.survey))

#frequ.weight2 <- cbind(frequ.weight, frequ)
#names(frequ.weight2 )
#frequ.weight2 <- frequ.weight2[ ,c("Var1", "Freq", "mean")]
#write.csv(frequ.weight2, "data/type_of_housing.csv")
#frequ.weight$Var0 <- row.names(frequ.weight)
#frequ.weight$Var1 <- substr(frequ.weight$Var0, regexpr("section3_household.housing.type_of_housing",frequ.weight$Var0), 10)

#frequ.weight$Var1 <- substr(as.character(frequ.weight$Var0), 42, nchar(frequ.weight$Var0)-42)
#nchar(frequ.weight$Var0)
