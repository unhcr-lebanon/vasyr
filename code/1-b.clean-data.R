
### Clean and weight data         ####

source("code/1-a.load-data.R")


#### Change age from day to years - rounded to the year..####################

#names(individual_biodata)
individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age <- round(individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age/365, digits = 0)
#table(individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age)


## Pb with wrong age of household date --

#test <- as.data.frame(household$section1.identify_interviewee.dob_HHHead)
#test <- as.data.frame(household[ 1:15, c("section1.identify_interviewee.dob_HHHead")])
#test$formated <-  as.Date(test$section1.identify_interviewee.dob_HHHead, format("%d-%b-%y"))
#test$corrected <- Sys.Date()
#for (i in 1:nrow(test))
#{
  # i <-1
#  if(test[i, c("formated")] > Sys.Date() )
#  { test[i, c("corrected")] <- as.Date((test[i, c("formated")] - 36525), format("%d-%b-%y"))
#  } else { test[i, c("corrected")] <- as.Date(test[i, c("formated")], format("%d-%b-%y")) }
#}
household$section1.identify_interviewee.dob_HHHeadold1 <- household$section1.identify_interviewee.dob_HHHead
household$section1.identify_interviewee.dob_HHHeadold <- as.Date(household$section1.identify_interviewee.dob_HHHead, format("%d-%b-%y"))
household$section1.identify_interviewee.dob_HHHead <- Sys.Date()

#View(household[638:642, c("section1.identify_interviewee.dob_HHHead")])
#View(household[638:642, c("section1.identify_interviewee.dob_HHHeadold")])

## How many NA
how.many.na <- household[is.na(household$section1.identify_interviewee.dob_HHHeadold), ]
rm(how.many.na)
#str(household[i, c("section1.identify_interviewee.dob_HHHeadold")]- 36525)
# i <-639

for (i in 1:nrow(household))
  {
  #cat(paste0(i,"\n"))
  if( is.na (household[ i, c("section1.identify_interviewee.dob_HHHeadold") ] ) )
        { household[ i, c("section1.identify_interviewee.dob_HHHead") ] <- "1900-01-01"
  } else if( household[ i, c("section1.identify_interviewee.dob_HHHeadold") ] > Sys.Date() )
        { household[ i, c("section1.identify_interviewee.dob_HHHead") ] <- household[ i, c("section1.identify_interviewee.dob_HHHeadold")] - 36525
        } else { household[ i, c("section1.identify_interviewee.dob_HHHead") ] <- household[ i, c("section1.identify_interviewee.dob_HHHeadold")] }
  }


#### Check location - fill with GPS when not available based on P-code##########

location <- read.csv("data/location.csv")
names(location)[1] <- "section1.location.pcode"
location.add <- location[ , c("section1.location.pcode", "Latitude", "Longitude")]

#names(location)
#names(household)

#location.vasyr <- as.data.frame(unique(household2$section1.location.pcode))
#household.back <- household
#rm(household.back)
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


#### Tables with clean log #######################################

### First look at records that needs to be updated#########################
#names(household)
# names(case_number_details)
library(readxl)
# Correct the case number for HH table based on the cleaning logs 
Correct.CaseNo <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "Vasyr2017_CaseNo_Correction")
#names(Correct.CaseNo)
#  "formid",  "incorrect_caseno" "correct_caseno"
Correct.CaseNo <- Correct.CaseNo[ , c("formid", "correct_caseno") ]
names(Correct.CaseNo)[1] <- "KEY"
case_number_details <- merge(x = case_number_details, y = Correct.CaseNo, by = "KEY", all.x = TRUE)
for (i in 1:nrow(case_number_details))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (case_number_details[ i, c("correct_caseno") ]) ) )
  { case_number_details[ i, c("section2.case_number_details.casenumber.unhcr_case_number") ] <- case_number_details[ i, c("correct_caseno") ] } else {}
}

# Correct the organisation for HH table based on the cleaning logs 
Correct.Org <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "Vasyr2017_Org_Correction")
#names(Correct.Org)
# "formid"       "organization"
Correct.Org <- Correct.Org [ , c("formid", "organization") ]
names(Correct.Org)[1] <- "KEY"
household <- merge(x = household, y = Correct.Org , by = "KEY", all.x = TRUE)
for (i in 1:nrow(household))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (household[ i, c("organization") ]) ) )
  { household[ i, c("enumerator_details.organization") ] <- household[ i, c("organization") ] } else {}
}

# Correct the cluster number for HH table based on the cleaning logs 
Correct.cluster <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "Vasyr2017_District_Correction")
#names(Correct.cluster)
# "caseno", "district",  "actual_cadaster", "assigned_cluster", "Order of Cases", "ODK District", "Formid" ,  "Same District"
Correct.cluster <- Correct.cluster[ , c("Formid", "assigned_cluster") ]
names(Correct.cluster)[1] <- "KEY"
household <- merge(x = household, y = Correct.cluster, by = "KEY", all.x = TRUE)

for (i in 1:nrow(household))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (household[ i, c("assigned_cluster") ]) ) )
  { household[ i, c("section1.location.cluster_number") ] <- household[ i, c("assigned_cluster") ] } else {}
}

# Correct the district for HH table based on the cleaning logs 
Correct.district <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "formid_and_correct_district")
#names(Correct.district)
# "formid"   "district"

## check different district naming scheme & correct
table(household$section1.location.district)
table(Correct.district$district)
#distr.check <- household$section1.location.district
#table(distr.check)

Correct.district <- data.frame(lapply(Correct.district, function(x) {gsub("el_hermel", "El_Hermel", x)}))
Correct.district <- data.frame(lapply(Correct.district, function(x) {gsub("west_bekaa", "West_Bekaa", x)}))
Correct.district$district <- as.character(Correct.district$district)
Correct.district$formid <- as.character(Correct.district$formid)
Correct.district <- Correct.district[ , c("formid", "district") ]
names(Correct.district)[1] <- "KEY"
household <- merge(x = household, y = Correct.district, by = "KEY", all.x = TRUE)

for (i in 1:nrow(household))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (household[ i, c("district") ]) ) )
  { household[ i, c("section1.location.district") ] <- household[ i, c("district") ] } else {}
}

# Correct the district for HH table based on updated cleaning script
Correct.district <- read_excel("data/Cluster_reassignment_missing_script.xlsx", sheet = "table", skip = 1)
#names(Correct.district)
#"formid",    "district" ,   "cluster_number" , "org_district",  "org_cluster" ,   "unhcr_case_number"
# "location_name" ,  "origin_or_replacement", "Assigned_District",    "Assigned_Cluster"

## check different district naming scheme & correct
table(household$section1.location.district)
table(Correct.district$Assigned_District)
Correct.district$Assigned_District <- sub(" ", "_", Correct.district$Assigned_District)
Correct.district$Assigned_District <- sub("-", "_", Correct.district$Assigned_District)
Correct.district <- Correct.district[ , c("formid", "Assigned_District") ]
names(Correct.district)[1] <- "KEY"
household <- merge(x = household, y = Correct.district, by = "KEY", all.x = TRUE)

for (i in 1:nrow(household))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (household[ i, c("Assigned_District") ]) ) )
  { household[ i, c("section1.location.district") ] <- household[ i, c("Assigned_District") ]} else {}
}


# Correct the district for HH table based on final cleaning provided 

Correct.district.final <- read_excel("data/Vasyr_Cluster_Assignment.xlsx",   sheet = "Sheet1")
names(Correct.district.final)
# "formid"            "Assigned_District" "Assigned_Cluster"
names(Correct.district.final)[1] <- "KEY"
names(Correct.district.final)[2] <- "Assigned_District2"
Correct.district.final <- Correct.district.final[ , c("KEY", "Assigned_District2") ]

# Check naming scheme of district & correct
table(Correct.district.final$Assigned_District2)
table(household$section1.location.district)
Correct.district.final$Assigned_District2 <- sub(" ", "_", Correct.district.final$Assigned_District2)
Correct.district.final$Assigned_District2 <- sub("-", "_", Correct.district.final$Assigned_District2)
household <- merge(x = household, y = Correct.district.final, by = "KEY", all.x = TRUE)

for (i in 1:nrow(household))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (household[ i, c("Assigned_District2") ]) ) )
  { household[ i, c("section1.location.district") ] <- household[ i, c("Assigned_District2") ] } else {}
}

## Correct the cluster number for HH table based on updated cleaning script
Correct.clust.2 <- read_excel("data/Cluster_reassignment_missing_script.xlsx", sheet = "table", skip = 1)
names(Correct.clust.2)
#"formid",    "district" ,   "cluster_number" , "org_district",  "org_cluster" ,   "unhcr_case_number"
# "location_name" ,  "origin_or_replacement", "Assigned_District",    "Assigned_Cluster"

# check cluster numbers
table(household$section1.location.cluster_number)
table(Correct.clust.2$Assigned_Cluster)
Correct.clust.2 <- Correct.clust.2[ , c("formid", "Assigned_Cluster") ]
names(Correct.clust.2)[1] <- "KEY"
household <- merge(x = household, y = Correct.clust.2, by = "KEY", all.x = TRUE)

for (i in 1:nrow(household))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (household[ i, c("Assigned_Cluster") ]) ) )
  { household[ i, c("section1.location.cluster_number") ] <- household[ i, c("Assigned_Cluster") ]} else {}
}


##### Correct the cluster number for HH table based on final cleaning provided #########

Correct.clus.final <- read_excel("data/Vasyr_Cluster_Assignment.xlsx",   sheet = "Sheet1")
names(Correct.clus.final)
# "formid"            "Assigned_District" "Assigned_Cluster"
names(Correct.clus.final)[1] <- "KEY"
names(Correct.clus.final)[3] <- "Assigned_Cluster2"
Correct.clus.final <- Correct.clus.final[ , c("KEY", "Assigned_Cluster2")]

# Check naming scheme of district & correct
table(Correct.clus.final$Assigned_Cluster2)
table(household$section1.location.cluster_number)
household <- merge(x = household, y = Correct.clus.final, by = "KEY", all.x = TRUE)

for (i in 1:nrow(household))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (household[ i, c("Assigned_Cluster2") ]) ) )
  { household[ i, c("section1.location.cluster_number") ] <- household[ i, c("Assigned_Cluster2") ] } else {}
}

rm(Correct.district.final, Correct.CaseNo, Correct.cluster, Correct.district, Correct.Org, location, location.add, Correct.clus.final, Correct.clust.2)


### Cluster assignment for INDIVIDUAL dataframe from error logs

Correct.cluster <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "Vasyr2017_District_Correction")
#names(Correct.cluster)
# "caseno", "district",  "actual_cadaster", "assigned_cluster", "Order of Cases", "ODK District", "Formid" ,  "Same District"
Correct.cluster <- Correct.cluster[ , c("Formid", "assigned_cluster") ]
names(Correct.cluster)[1] <- "KEY"
individual_biodata <- merge(x = individual_biodata, y = Correct.cluster, by = "KEY", all.x = TRUE)

for (i in 1:nrow(individual_biodata))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (individual_biodata[ i, c("assigned_cluster") ]) ) )
  { individual_biodata[ i, c("section1.location.cluster_number") ] <- individual_biodata[ i, c("assigned_cluster") ] } else {}
}

### District adjustment for INDIVIDUAL dataframe from error logs
Correct.district <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "formid_and_correct_district")
#names(Correct.district)
# "formid"   "district"

## check different district naming scheme & correct
table(individual_biodata$section1.location.district)
table(Correct.district$district)
#distr.check <- individual_biodata$section1.location.district
#table(distr.check)

Correct.district <- data.frame(lapply(Correct.district, function(x) {gsub("el_hermel", "El_Hermel", x)}))
Correct.district <- data.frame(lapply(Correct.district, function(x) {gsub("west_bekaa", "West_Bekaa", x)}))
Correct.district$district <- as.character(Correct.district$district)
Correct.district$formid <- as.character(Correct.district$formid)

Correct.district <- Correct.district[ , c("formid", "district") ]

names(Correct.district)[1] <- "KEY"
individual_biodata <- merge(x = individual_biodata, y = Correct.district, by = "KEY", all.x = TRUE)
for (i in 1:nrow(individual_biodata))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (individual_biodata[ i, c("district") ]) ) )
  { individual_biodata[ i, c("section1.location.district") ] <- individual_biodata[ i, c("district") ] } else {}
}

# Correct the district for IDV table based on updated cleaning script
Correct.district <- read_excel("data/Cluster_reassignment_missing_script.xlsx", sheet = "table", skip = 1)
#names(Correct.district)
#"formid",    "district" ,   "cluster_number" , "org_district",  "org_cluster" ,   "unhcr_case_number"
# "location_name" ,  "origin_or_replacement", "Assigned_District",    "Assigned_Cluster"

## check different district naming scheme & correct
table(individual_biodata$section1.location.district)
table(Correct.district$Assigned_District)
Correct.district$Assigned_District <- sub(" ", "_", Correct.district$Assigned_District)
Correct.district$Assigned_District <- sub("-", "_", Correct.district$Assigned_District)
Correct.district <- Correct.district[ , c("formid", "Assigned_District") ]
names(Correct.district)[1] <- "KEY"
individual_biodata <- merge(x = individual_biodata, y = Correct.district, by = "KEY", all.x = TRUE)

for (i in 1:nrow(individual_biodata))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (individual_biodata[ i, c("Assigned_District") ]) ) )
  { individual_biodata[ i, c("section1.location.district") ] <- individual_biodata[ i, c("Assigned_District") ]} else {}
}


# Correct the district for IDV table based on final cleaning provided 

Correct.district.final <- read_excel("data/Vasyr_Cluster_Assignment.xlsx",   sheet = "Sheet1")
names(Correct.district.final)
# "formid"            "Assigned_District" "Assigned_Cluster"
names(Correct.district.final)[1] <- "KEY"
names(Correct.district.final)[2] <- "Assigned_District2"
Correct.district.final <- Correct.district.final[ , c("KEY", "Assigned_District2") ]

# Check naming scheme of district & correct
table(Correct.district.final$Assigned_District2)
table(individual_biodata$section1.location.district)
Correct.district.final$Assigned_District2 <- sub(" ", "_", Correct.district.final$Assigned_District2)
Correct.district.final$Assigned_District2 <- sub("-", "_", Correct.district.final$Assigned_District2)
individual_biodata <- merge(x = individual_biodata, y = Correct.district.final, by = "KEY", all.x = TRUE)

for (i in 1:nrow(individual_biodata))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (individual_biodata[ i, c("Assigned_District2") ]) ) )
  { individual_biodata[ i, c("section1.location.district") ] <- individual_biodata[ i, c("Assigned_District2") ] } else {}
}

# Check districts ## check later after drop of forms
write.csv(individual_biodata$section1.location.district, file = "out/district.check.csv")

## Correct the cluster number for IDV table based on updated cleaning script
Correct.clust.2 <- read_excel("data/Cluster_reassignment_missing_script.xlsx", sheet = "table", skip = 1)
names(Correct.clust.2)
#"formid",    "district" ,   "cluster_number" , "org_district",  "org_cluster" ,   "unhcr_case_number"
# "location_name" ,  "origin_or_replacement", "Assigned_District",    "Assigned_Cluster"

# check cluster numbers
table(individual_biodata$section1.location.cluster_number)
table(Correct.clust.2$Assigned_Cluster)
Correct.clust.2 <- Correct.clust.2[ , c("formid", "Assigned_Cluster") ]
names(Correct.clust.2)[1] <- "KEY"
individual_biodata <- merge(x = individual_biodata, y = Correct.clust.2, by = "KEY", all.x = TRUE)

for (i in 1:nrow(individual_biodata))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (individual_biodata[ i, c("Assigned_Cluster") ]) ) )
  { individual_biodata[ i, c("section1.location.cluster_number") ] <- individual_biodata[ i, c("Assigned_Cluster") ]} else {}
}


##### Correct the cluster number for IDV table based on final cleaning provided #########

Correct.clus.final <- read_excel("data/Vasyr_Cluster_Assignment.xlsx",   sheet = "Sheet1")
names(Correct.clus.final)
# "formid"            "Assigned_District" "Assigned_Cluster"
names(Correct.clus.final)[1] <- "KEY"
names(Correct.clus.final)[3] <- "Assigned_Cluster2"
Correct.clus.final <- Correct.clus.final[ , c("KEY", "Assigned_Cluster2")]

# Check naming scheme of district & correct
table(Correct.clus.final$Assigned_Cluster2)
table(individual_biodata$section1.location.cluster_number)
individual_biodata <- merge(x = individual_biodata, y = Correct.clus.final, by = "KEY", all.x = TRUE)

for (i in 1:nrow(individual_biodata))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (individual_biodata[ i, c("Assigned_Cluster2") ]) ) )
  { individual_biodata[ i, c("section1.location.cluster_number") ] <- individual_biodata[ i, c("Assigned_Cluster2") ] } else {}
}

rm(Correct.district.final, Correct.CaseNo, Correct.cluster, Correct.district, Correct.district, Correct.Org, location, location.add, Correct.clus.final, Correct.clust.2)
          
### cluster assignment for CASE dataframe from cleaning logs 

Correct.cluster <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "Vasyr2017_District_Correction")
#names(Correct.cluster)
# "caseno", "district",  "actual_cadaster", "assigned_cluster", "Order of Cases", "ODK District", "Formid" ,  "Same District"
Correct.cluster <- Correct.cluster[ , c("Formid", "assigned_cluster") ]
names(Correct.cluster)[1] <- "KEY"
case_number_details <- merge(x = case_number_details, y = Correct.cluster, by = "KEY", all.x = TRUE)

for (i in 1:nrow(case_number_details))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (case_number_details[ i, c("assigned_cluster") ]) ) )
  { case_number_details[ i, c("section1.location.cluster_number") ] <- case_number_details[ i, c("assigned_cluster") ] } else {}
}


Correct.district <- read_excel("data/erorr_and_correction_tables.xlsx",   sheet = "formid_and_correct_district")
#names(Correct.district)
# "formid"   "district"

## check different district naming scheme & correct
table(case_number_details$section1.location.district)
table(Correct.district$district)
#distr.check <- case_number_details$section1.location.district
#table(distr.check)

Correct.district <- data.frame(lapply(Correct.district, function(x) {gsub("el_hermel", "El_Hermel", x)}))
Correct.district <- data.frame(lapply(Correct.district, function(x) {gsub("west_bekaa", "West_Bekaa", x)}))
Correct.district$district <- as.character(Correct.district$district)
Correct.district$formid <- as.character(Correct.district$formid)

Correct.district <- Correct.district[ , c("formid", "district") ]

names(Correct.district)[1] <- "KEY"
case_number_details <- merge(x = case_number_details, y = Correct.district, by = "KEY", all.x = TRUE)
for (i in 1:nrow(case_number_details))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (case_number_details[ i, c("district") ]) ) )
  { case_number_details[ i, c("section1.location.district") ] <- case_number_details[ i, c("district") ] } else {}
}

# Correct the district for CASE table based on updated cleaning script
Correct.district <- read_excel("data/Cluster_reassignment_missing_script.xlsx", sheet = "table", skip = 1)
#names(Correct.district)
#"formid",    "district" ,   "cluster_number" , "org_district",  "org_cluster" ,   "unhcr_case_number"
# "location_name" ,  "origin_or_replacement", "Assigned_District",    "Assigned_Cluster"

## check different district naming scheme & correct
table(case_number_details$section1.location.district)
table(Correct.district$Assigned_District)
Correct.district$Assigned_District <- sub(" ", "_", Correct.district$Assigned_District)
Correct.district$Assigned_District <- sub("-", "_", Correct.district$Assigned_District)
Correct.district <- Correct.district[ , c("formid", "Assigned_District") ]
names(Correct.district)[1] <- "KEY"
case_number_details <- merge(x = case_number_details, y = Correct.district, by = "KEY", all.x = TRUE)

for (i in 1:nrow(case_number_details))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (case_number_details[ i, c("Assigned_District") ]) ) )
  { case_number_details[ i, c("section1.location.district") ] <- case_number_details[ i, c("Assigned_District") ]} else {}
}


# Correct the district for CASE table based on final cleaning provided 

Correct.district.final <- read_excel("data/Vasyr_Cluster_Assignment.xlsx",   sheet = "Sheet1")
names(Correct.district.final)
# "formid"            "Assigned_District" "Assigned_Cluster"
names(Correct.district.final)[1] <- "KEY"
names(Correct.district.final)[2] <- "Assigned_District2"
Correct.district.final <- Correct.district.final[ , c("KEY", "Assigned_District2") ]

# Check naming scheme of district & correct
table(Correct.district.final$Assigned_District2)
table(case_number_details$section1.location.district)
Correct.district.final$Assigned_District2 <- sub(" ", "_", Correct.district.final$Assigned_District2)
Correct.district.final$Assigned_District2 <- sub("-", "_", Correct.district.final$Assigned_District2)
case_number_details <- merge(x = case_number_details, y = Correct.district.final, by = "KEY", all.x = TRUE)

for (i in 1:nrow(case_number_details))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (case_number_details[ i, c("Assigned_District2") ]) ) )
  { case_number_details[ i, c("section1.location.district") ] <- case_number_details[ i, c("Assigned_District2") ] } else {}
}

# Check districts ## check later after drop of forms
#write.csv(case_number_details$section1.location.district, file = "out/district.check.csv")

## Correct the cluster number for CASE table based on updated cleaning script
Correct.clust.2 <- read_excel("data/Cluster_reassignment_missing_script.xlsx", sheet = "table", skip = 1)
names(Correct.clust.2)
#"formid",    "district" ,   "cluster_number" , "org_district",  "org_cluster" ,   "unhcr_case_number"
# "location_name" ,  "origin_or_replacement", "Assigned_District",    "Assigned_Cluster"

# check cluster numbers
table(case_number_details$section1.location.cluster_number)
table(Correct.clust.2$Assigned_Cluster)
Correct.clust.2 <- Correct.clust.2[ , c("formid", "Assigned_Cluster") ]
names(Correct.clust.2)[1] <- "KEY"
case_number_details <- merge(x = case_number_details, y = Correct.clust.2, by = "KEY", all.x = TRUE)

for (i in 1:nrow(case_number_details))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (case_number_details[ i, c("Assigned_Cluster") ]) ) )
  { case_number_details[ i, c("section1.location.cluster_number") ] <- case_number_details[ i, c("Assigned_Cluster") ]} else {}
}


##### Correct the cluster number for CASE table based on final cleaning provided #########

Correct.clus.final <- read_excel("data/Vasyr_Cluster_Assignment.xlsx",   sheet = "Sheet1")
names(Correct.clus.final)
# "formid"            "Assigned_District" "Assigned_Cluster"
names(Correct.clus.final)[1] <- "KEY"
names(Correct.clus.final)[3] <- "Assigned_Cluster2"
Correct.clus.final <- Correct.clus.final[ , c("KEY", "Assigned_Cluster2")]

# Check naming scheme of district & correct
table(Correct.clus.final$Assigned_Cluster2)
table(case_number_details$section1.location.cluster_number)
case_number_details <- merge(x = case_number_details, y = Correct.clus.final, by = "KEY", all.x = TRUE)

for (i in 1:nrow(case_number_details))
{
  #cat(paste0(i,"\n"))
  if( !(is.na (case_number_details[ i, c("Assigned_Cluster2") ]) ) )
  { case_number_details[ i, c("section1.location.cluster_number") ] <- case_number_details[ i, c("Assigned_Cluster2") ] } else {}
}

rm(Correct.district.final, Correct.CaseNo, Correct.cluster, Correct.district, Correct.district, Correct.Org, location, location.add, Correct.clus.final, Correct.clust.2)


### Backup Dataframes

household.backup <- household
individual_biodata.backup <- individual_biodata
case_number_details.backup <- case_number_details

### Restore 

#household.backup -> household
#individual_biodata.backup -> individual_biodata
#case_number_details.backup -> case_number_details

### Now Remove FormIDs to be dropped #######################
# Duplicate visits to be dropped from cleaning file AND further FormIDs also not in WFP final dataset

drop.form <- read_excel("data/erorr_and_correction_tables.xlsx", sheet = "duplicate_visit_drop")
household <- merge(x = household, y = drop.form, by = "KEY", all.x = TRUE)
household <- household[ (is.na(household$to_delete)), ]

individual_biodata <- merge(x=individual_biodata, y=drop.form, by="KEY", all.x=TRUE)
individual_biodata <- individual_biodata[ (is.na(individual_biodata$to_delete)), ]

case_number_details <- merge(x=case_number_details, y=drop.form, by="KEY", all.x=TRUE)
case_number_details <- case_number_details[ (is.na(case_number_details$to_delete)), ]

difficulties_encountered <- merge(x=difficulties_encountered, y=drop.form, by="KEY", all.x=TRUE)
difficulties_encountered <- difficulties_encountered[ (is.na(difficulties_encountered$to_delete)), ]

illegal_residence <- merge(x=illegal_residence, y=drop.form, by="KEY", all.x=TRUE)
illegal_residence <- illegal_residence[ (is.na(illegal_residence$to_delete)), ]

legal_residence <- merge(x=legal_residence, y=drop.form, by="KEY", all.x=TRUE)
legal_residence <- legal_residence[ (is.na(legal_residence$to_delete)), ]

moved_returnee <- merge(x=moved_returnee, y=drop.form, by="KEY", all.x=TRUE)
moved_returnee <- moved_returnee[ (is.na(moved_returnee$to_delete)), ]

### Now drop the duplicate individuals Forms
### need to find better solution to link drop directly rather than through row number
dup.idv <- which(individual_biodata$KEY == "uuid:919aea4a-043d-4cfb-bd32-31b3e4f58323" & individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.first_name == "Mashaan")
dup.idv2 <- which(individual_biodata$KEY == "uuid:ac40b266-3b14-49d7-bed9-efef682bf907" & individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.first_name == "Ahmad")
dup.idv[1]
dup.idv2[1]

individual_biodata <- individual_biodata[-dup.idv[1],]
individual_biodata <- individual_biodata[-dup.idv2[1],]

#cross.check <- read_excel("data/erorr_and_correction_tables.xlsx", sheet = "dublicate_visit_to_Delx")
#individual_biodata <- merge(x=individual_biodata, y=cross.check, by="KEY", all.x=TRUE)
#individual_biodata <- individual_biodata[ (is.na(individual_biodata$further_delete)), ]

#individual_biodata.drop <- read_excel("data/erorr_and_correction_tables.xlsx", sheet = "dublicate_visit_to_Del")
#individual_biodata.drop$unique <- paste(individual_biodata.drop$KEY, individual_biodata.drop$section2.case_number_details.casenumber.unhcr_number_bis)
#individual_biodata$unique <- paste(individual_biodata$KEY, individual_biodata$section2.case_number_details.casenumber.unhcr_number_bis)
#individual_biodata <- merge(x=individual_biodata, y=individual_biodata.drop, by="unique", all.x=TRUE)
#individual_biodata <- individual_biodata[ (is.na(individual_biodata$to_delete)), ]

#rm(individual_biodata.drop)
#rm(cross.check)
rm(drop.form, Correct.CaseNo, Correct.cluster, Correct.Cluster2, Correct.district, Correct.Org, location, location.add, dup.idv, dup.idv2)

#### Check district and cluster assignment #####
HH.check.clus.dist <- household[, c("section1.location.district", "section1.location.cluster_number", "KEY")]
IDV.check.clus.dist <- individual_biodata[, c("section1.location.district", "section1.location.cluster_number", "KEY")]
CASE.check.clus.dist <- case_number_details[, c("section1.location.district", "section1.location.cluster_number", "KEY")]
write.csv(HH.check.clus.dist, file = "out/hh.dist.clus.check.csv")
write.csv(IDV.check.clus.dist, file = "out/idv.dist.clus.check.csv")
write.csv(CASE.check.clus.dist, file = "out/case.dist.clus.check.csv")
rm(HH.check.clus.dist, IDV.check.clus.dist, CASE.check.clus.dist)
#### Weighting data#####################################################

weight <- read_excel("data/weight22_06-2017-2.xlsx",  sheet = "weight2206")
#names(weight)
#"Districts"              "Districtssp"       "Population.Individuals" "Population.HHsNS"      
#"prop.in.sample"         "Sample.size"        "prop.in.pop"            "Design.Weight"         
#"Normalized.Weight"
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
#fpc <- nrow(weight)
#weight$fpc <- fpc

## Good -- let's trim the weight frame
weight2 <- weight[ c("Districts",   "Normalized.Weight","Population.HHsNS", "Sample.size")]
names(weight2)[1] <- "section1.location.district"
names(weight2)[3] <- "fpc"

fpc <- weight2$fpc
  
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

rm(weight2, weight)

## Now testing weighting using the survey library
library(survey)

## Survey design follows one-stage modality due to sampling with population proportional to size in the first stage
#household.survey <- svydesign(ids = ~ section1.location.district ,  data = household ,  weights = ~Normalized.Weight ,  fpc = ~fpc )
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
