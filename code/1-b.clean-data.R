#######################################
### Clean and weight data         ####
#######################################

source("code/1-a.loaddata.R")

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

weight <- read_excel("data/weight data 22_06-2017-2.xlsx",  sheet = " weight2206")
location.vasyr.district <- as.data.frame(unique(household2$section1.location.location))

names(location.vasyr.district)[1] <- "Districts"
#location.vasyr.district <- as.data.frame(location.vasyr.district[!(is.na(location.vasyr.district$Districts )), ])
names(weight)
weight <- merge(x=weight, y=location.vasyr.district, by="Districts", all.x=TRUE)
