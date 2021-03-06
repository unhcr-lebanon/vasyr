#######################################
### Clean and weight data         ####
#######################################

source("code/1-b.clean-data.R")


###################################################################################
##### Re-encode correctly the dataset
cat("\n\n\nNow re-encode data and label variables \n\n\n\n")


cat("\n\n\n Household \n\n\n\n")
# household1 <- kobo_split_multiple(household, dico)
household <- kobo_split_multiple(household, dico)
household <- kobo_encode(household, dico)
household <- kobo_label(household , dico)


cat("\n\n\n Case \n\n\n\n")
case_number_details <- kobo_split_multiple(case_number_details, dico)
case_number_details <- kobo_encode(case_number_details, dico)
case_number_details <- kobo_label(case_number_details , dico)


cat("\n\n\n Individuals \n\n\n\n")
individual_biodata <- kobo_split_multiple(individual_biodata, dico)
individual_biodata <- kobo_encode(individual_biodata, dico)
individual_biodata <- kobo_label(individual_biodata , dico)


cat("\n\nWrite backup\n")

write.csv(household, "data/household1.csv")
write.csv(case_number_details, "data/case_number_details1.csv")
write.csv(individual_biodata , "data/individual_biodata1.csv")
write.csv(difficulties_encountered, "data/difficulties_encountered1.csv")
write.csv(illegal_residence, "data/illegal_residence1.csv")
write.csv(legal_residence , "data/legal_residence1.csv")
write.csv(moved_returnee, "data/moved_returnee1.csv")
