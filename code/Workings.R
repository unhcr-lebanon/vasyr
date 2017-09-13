## Testing survey objects
# survey with fpc (PSU number not right)
household.survey <- svydesign(ids = ~ section1.location.district ,  data = household ,  weights = ~Normalized.Weight ,  fpc = ~fpc )
summary(household.survey)
svymean(~household$section2.tot_above_15, design = household.survey)

# survey without fpc (no need for fpc with PPS but are weightings enough, stratification...) 
household.survey.nofpc <- svydesign(ids = ~ section1.location.district ,  data = household ,  weights = ~Normalized.Weight )
summary(household.survey.nofpc)
svymean(~household$section2.tot_above_15, design = household.survey.nofpc)

# survey object with cluster ids
household$clusterID <- paste(household$section1.location.district, household$section1.location.cluster_number)
#How many unique?
uniqueIDs <- unique(household$clusterID)
summary(uniqueIDs)
uniqueIDs <- as.data.frame(uniqueIDs)
#fpc <- nrow(uniqueIDs)
#weight$fpc <- fpc

household$test.w <- NA
for (i in unique(household$clusterID)) {
  household$test.w[household$clusterID == i] <- 280031/(892 * sum(household$clusterID == i))}

HH.clus.survey <- svydesign(id = ~clusterID, data = household, weights = ~test.w)
summary(HH.clus.survey)
svymean(~household$section2.tot_above_15, design = HH.clus.survey)

###################################

# testing other survey object 

weight <- read_excel("data/weight22_06-2017-2.xlsx",  sheet = "weight2206")
names(weight)
weight2 <- weight[ c("DistGov",   "Normalized.Weight","Population.HHsNS", "Sample.size")]

names(weight2)[1] <- "section1.location.district"
household <- join(x=household, y=weight2, by="section1.location.district")
case_number_details <- join(x=case_number_details, y=weight2, by="section1.location.district")
individual_biodata <- join(x=individual_biodata, y=weight2, by="section1.location.district")

HH.survey <- svydesign(id = ~clusterID, data = household, weights = ~Normalized.Weight)
household$Normalized.Weight
####################################

plyr::ddply(household, .(section1.location.district), transform, count = length(num))

household$section1.location.district.count <- ave(df$num, household[,section1.location.district], FUN=length)

household$countdist <- (table(household$section1.location.district))
household$countdist
test <- household[ c("section1.location.district",   "countdist")]
test2 <- paste(household$section1.location.district, household$count.dist, sep = "-")

table(test2)
unique.test.1 <- unique(test2)

unique.test.1

unique.test <- unique(test, incomparables = FALSE)
unique.test

rm(fpc)
fpc <- household$fpc

svymean(~case_number_details$any.assist, case_number_details.survey, na.rm = TRUE)
svymean(~case_number_details$inkind.assist, case_number_details.survey, na.rm = TRUE)
svymean(~household$crisis, household.survey, na.rm = TRUE)
svymean(~household$emergency, household.survey, na.rm = TRUE)
svymean(~household$stress, household.survey, na.rm = TRUE)
svymean(~household$adult, household.survey, na.rm = TRUE)
svymean(~household$child, household.survey, na.rm = TRUE)
svymean(~household$F, household.survey, na.rm = TRUE)
svymean(~household$M, household.survey, na.rm = TRUE)
svymean(~household$child.per.hh, household.survey, na.rm = TRUE)
svymean(~individual_biodata$dependency, individual_biodata.survey, na.rm = TRUE)
svymean(~household$HH4, household.survey, na.rm = TRUE)
svymean(~household$HH56, household.survey, na.rm = TRUE)
svymean(~household$HH7, household.survey, na.rm = TRUE)
svymean(~household$lessthan4, household.survey, na.rm = TRUE)
svymean(~household$over59, household.survey, na.rm = TRUE)
svymean(~household$under2, household.survey, na.rm = TRUE)
svymean(~household$under5, household.survey, na.rm = TRUE)
svymean(~household$y12to14, household.survey, na.rm = TRUE)
svymean(~household$y15to17, household.survey, na.rm = TRUE)
svymean(~household$MEB125, household.survey, na.rm = TRUE)
svymean(~household$MEB125.greater, household.survey, na.rm = TRUE)
svymean(~household$service.cost, household.survey, na.rm = TRUE)
svymean(~household$SMEB, household.survey, na.rm = TRUE)
svymean(~household$SMEB.MEB, household.survey, na.rm = TRUE)
svymean(~household$prim.notrec.req, household.survey, na.rm = TRUE)
svymean(~household$prim.rec.req, household.survey, na.rm = TRUE)
svymean(~household$sec.notrec.req, household.survey, na.rm = TRUE)
svymean(~individual_biodata$birthdocs, individual_biodata.survey, na.rm = TRUE)
svymean(~household$legal.res, household.survey, na.rm = TRUE)
svymean(~household$HHdensitypp, household.survey, na.rm = TRUE)
svymean(~household$density1, household.survey, na.rm = TRUE)
svymean(~household$density2, household.survey, na.rm = TRUE)
svymean(~household$density3, household.survey, na.rm = TRUE)
svymean(~household$density4, household.survey, na.rm = TRUE)
# issue with Infinity 
#svymean(~household$ppl.room.rm, household.survey, na.rm = TRUE)
svymean(~individual_biodata$dependents, individual_biodata.survey, na.rm = TRUE)
svymean(~household$hh.dependents, household.survey, na.rm = TRUE)
svytotal(~household$hh.dependents, household.survey, na.rm = TRUE)

svymean(~household$needs, household.survey, na.rm = TRUE)
svymean(~individual_biodata$needs.ind, individual_biodata.survey, na.rm = TRUE)
svymean(~individual_biodata$non.related, individual_biodata.survey, na.rm = TRUE)
svymean(~household$norelation, household.survey, na.rm = TRUE)
svymean(~individual_biodata$SHH, individual_biodata.survey, na.rm = TRUE)
svymean(~household$share.toil, household.survey, na.rm = TRUE)
svymean(~household$wash.access, household.survey, na.rm = TRUE)
svymean(~household$wash.inadeq, household.survey, na.rm = TRUE)
svymean(~household$age_HHHead, household.survey, na.rm = TRUE)
svymean(~household$CHHH, household.survey, na.rm = TRUE)
svymean(~household$HHH15, household.survey, na.rm = TRUE)
svymean(~household$HHH15.18, household.survey, na.rm = TRUE)
svymean(~household$HHH59, household.survey, na.rm = TRUE)

# assistance 
svymean(~household$any.assist.hh >=1, household.survey, na.rm = TRUE)
svymean(~household$inkind.assist.hh >=1, household.survey, na.rm = TRUE)
svymean(~household$hh.cash.assist, household.survey, na.rm = TRUE)
svymean(~household$hh.food.vouchers, household.survey, na.rm = TRUE)
svymean(~household$hh.mcap.assist, household.survey, na.rm = TRUE)
svymean(~household$hh.win.assist, household.survey, na.rm = TRUE)
svymean(~household$hh.fuel.card, household.survey, na.rm = TRUE)
svymean(~household$hh.cash.for.rent, household.survey, na.rm = TRUE)
svymean(~household$hh.cash.for.water, household.survey, na.rm = TRUE)
svymean(~household$hh.cash.for.hygiene, household.survey, na.rm = TRUE)
svymean(~household$hh.tech.assist, household.survey, na.rm = TRUE)
svymean(~household$hh.technical.assistance, household.survey, na.rm = TRUE)
svymean(~household$hh.food.inkind, household.survey, na.rm = TRUE)
svymean(~household$hh.health.care, household.survey, na.rm = TRUE)
svymean(~household$hh.fuel.subsidy, household.survey, na.rm = TRUE)
svymean(~household$hh.rent.subsidy, household.survey, na.rm = TRUE)
svymean(~household$hh.hygiene.kits, household.survey, na.rm = TRUE)
svymean(~household$hh.other.nfi, household.survey, na.rm = TRUE)
svymean(~household$hh.edu.hygiene, household.survey, na.rm = TRUE)
svymean(~household$hh.desludging.ser, household.survey, na.rm = TRUE)
svymean(~household$hh.solid.waste.bins, household.survey, na.rm = TRUE)
svymean(~household$hh.solid.was.services, household.survey, na.rm = TRUE)
svymean(~household$hh.water.trucking, household.survey, na.rm = TRUE)

library(dplyr)

# box plots with reordered district level
debtdist <- household[ , c("section1.location.district", "income_expenditure.borrowing_debt.credit_total")]

debtdist <- mutate(debtdist, district.ordered = reorder(section1.location.district, income_expenditure.borrowing_debt.credit_total, median, na.rm = TRUE))
bwplot(district.ordered ~ income_expenditure.borrowing_debt.credit_total, data = debtdist)

# rent by shelter 
rent.shelt <- household[ , c("section3_household.housing.type_of_housing", "section3_household.housing.rent_amount")]
rent.shelt <- mutate(rent.shelt, shelt.ordered = reorder(section3_household.housing.type_of_housing, section3_household.housing.rent_amount, mean, na.rm = TRUE))
bwplot(shelt.ordered ~ section3_household.housing.rent_amount, xlim=c(-10000,3000000), data = rent.shelt, main="Average rent by shelter type", xlab="Rent (LBP)")

rent.shelt2 <- household[ , c("section3_household.housing.type_of_housing", "section3_household.housing.rent_amount")]
rent.shelt2 <- mutate(rent.shelt2, shelt.ordered = reorder(section3_household.housing.type_of_housing, section3_household.housing.rent_amount, median, na.rm = TRUE))
bwplot(shelt.ordered ~ section3_household.housing.rent_amount, xlim=c(-10000,3000000), data = rent.shelt, main="Median rent by shelter type", xlab="Rent (LBP)")

plot(densityplot(~section3_household.housing.rent_amount, data = household, groups = section3_household.housing.type_of_housing, auto.key = T, n=300000))
plot(densityplot(~section3_household.housing.rent_amount, data = household, groups = section3_household.housing.type_of_housing == "Tent", auto.key = T))
apt <- (household$section3_household.housing.type_of_housing == "Apartment/house (Not shared)") [ , c("TRUE")]
####### graphing

crosssfrequ.weight <-as.data.frame(prop.table(svytable(~section3_household.housing.rent_amount + section3_household.housing.type_of_housing, design =household.survey  ), margin = 2))
names(crosssfrequ.weight)[1] <- "quest"
names(crosssfrequ.weight)[2] <- "disag"
crosssfrequ.weight$Freq2 <- paste0(round(crosssfrequ.weight$Freq*100,digits=1),"%")
## Reorder factor
cross <- dcast(crosssfrequ.weight, disag  ~ quest, value.var = "Freq")
cross <- cross[ order(cross[ ,2], decreasing = TRUE) ,  ]
crosssfrequ.weight$disag <- factor(crosssfrequ.weight$disag, levels = as.character(cross[ ,1]))


## and now the graph
ggplot(crosssfrequ.weight, aes(fill=crosssfrequ.weight$quest, y=crosssfrequ.weight$Freq, x=crosssfrequ.weight$disag)) +
  geom_bar(colour="#2a87c8", stat ="identity", width=.8, aes(fill = quest), position = position_stack(reverse = TRUE)) +
  guides(fill=FALSE) +
  geom_label_repel(aes(label = Freq2), fill = "#2a87c8", color = 'white') +
  ylab("Frequency") +
  scale_y_continuous(labels=percent)+
  xlab("") +
  coord_flip() +
  ggtitle("Rent amount by shelter type",
          subtitle = paste0("Weighted results. Question response rate: ",percentreponse," .")) +
  theme(plot.title=element_text(face="bold", size=9),
        plot.background = element_rect(fill = "transparent",colour = NA))

# rent by district 

expdist <- household[ , c("section1.location.district", "income_expenditure.expenditure.expenditure_total")]

expdist <- mutate(expdist, district.ordered = reorder(section1.location.district, income_expenditure.expenditure.expenditure_total, median, na.rm = TRUE))
expdist
bwplot(district.ordered ~ income_expenditure.expenditure.expenditure_total, data = expdist, main = "Median Expenditure by District", xlab = "Expenditure")

# by mean
expdist2 <- household[ , c("section1.location.district", "income_expenditure.expenditure.expenditure_total")]
expdist2 <- mutate(expdist2, district.ordered = reorder(section1.location.district, income_expenditure.expenditure.expenditure_total, FUN = mean, na.rm = TRUE))
bwplot(district.ordered ~ income_expenditure.expenditure.expenditure_total, data = expdist2, main = "Median Expenditure by District", xlab = "Expenditure")

svymean(household$income_expenditure.expenditure.expenditure_total, design = household.survey)
svyby(~section3_household.housing.rent_amount, ~section3_household.housing.type_of_housing, household.survey, svymean)
check <- svyby(~section3_household.housing.rent_amount, ~section3_household.housing.type_of_housing, household.survey, svymean, na.rm=TRUE)

### checking needs

check.needs <- dcast(individual_biodata, individual_biodata$SET.OF.section2.case_number_details ~ (individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.disability == "Yes"  | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.preg_lactating == "Yes" | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.chronic_illness == "Yes"  | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.temp_illness == "Yes"  | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.serious_med_cond == "Yes"  | ((individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.need_assistance == "Yes") & (individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age >=60))))
summary(check.needs)



#Checking hh versus case object for frequencies 

# load the two survey objects 
household.survey <- svydesign(ids = ~ section1.location.district ,  data = household ,  weights = ~Normalized.Weight)
case.survey <- svydesign(ids = ~ section1.location.district ,  data = case_number_details ,  weights = ~Normalized.Weight)
household.survey2 <- svydesign(ids = ~ section1.location.district ,  data = household ,  weights = ~Normalized.Weight, nest=TRUE)

# cash for rent 
rentcash1 <- case_number_details$section2.case_number_details.assistance.cash_for_rent
rentcash2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.cash_for_rent == "Yes"))[ , c("TRUE")]
svymean(~rentcash1 == "Yes", case.survey, na.rm = TRUE)
svymean(~rentcash2 >=1, household.survey)

# cash for water 
watercash1 <- case_number_details$section2.case_number_details.assistance.cash_for_water == "Yes"
watercash2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.cash_for_water == "Yes"))[ , c("TRUE")]
svymean(~watercash1, case.survey, na.rm = TRUE)
svymean(~watercash2 >=1, household.survey)

# cash for hygiene items
hygcash1 <- case_number_details$section2.case_number_details.assistance.cash_for_hygiene == "Yes"
hygcash2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.cash_for_hygiene == "Yes"))[ , c("TRUE")]
svymean(~hygcash1, case.survey, na.rm = TRUE)
svymean(~hygcash2 >=1, household.survey)

# desludging assist
desludg1 <- case_number_details$section2.case_number_details.assistance.desludging_ser == "Yes"
desludg2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.desludging_ser == "Yes"))[ , c("TRUE")]
svymean(~desludg1, case.survey, na.rm = TRUE)
svymean(~desludg2 >=1, household.survey)

# technical assist
techass1 <- case_number_details$section2.case_number_details.assistance.technical_assistance == "Yes"
techass2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.technical_assistance == "Yes"))[ , c("TRUE")]
svymean(~techass1, case.survey, na.rm = TRUE)
svymean(~techass2 >=1, household.survey)

# reg. technical assist
techassfreq1 <- case_number_details$section2.case_number_details.assistance.tech_assist_freq == "Yes, I still receive it regularly" | case_number_details$section2.case_number_details.assistance.tech_assist_freq == "It was regular, but don’t receive it any more"
techassfreq2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.tech_assist_freq == "Yes, I still receive it regularly" | case_number_details$section2.case_number_details.assistance.tech_assist_freq == "It was regular, but don’t receive it any more"))[ , c("TRUE")]
svymean(~techassfreq1, case.survey, na.rm = TRUE)
svymean(~techassfreq2 >=1, household.survey)

# health assist
summary(case_number_details$section2.case_number_details.assistance.health_care)
healthass1 <- case_number_details$section2.case_number_details.assistance.health_care == "Yes"
healthass2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.health_care == "Yes"))[ , c("TRUE")]
svymean(~healthass1, case.survey, na.rm = TRUE)
svymean(~healthass2 >=1, household.survey)

# fuel subsidy 
table(case_number_details$section2.case_number_details.assistance.fuel_subsidy)
fuelass1 <- case_number_details$section2.case_number_details.assistance.fuel_subsidy == "Yes"
fuelass2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.fuel_subsidy == "Yes"))[ , c("TRUE")]
svymean(~fuelass1, case.survey, na.rm = TRUE)
svymean(~fuelass2 >=1, household.survey)

# rent subsidy 
table(case_number_details$section2.case_number_details.assistance.fuel_subsidy)
rentass1 <- case_number_details$section2.case_number_details.assistance.rent_subsidy == "Yes"
rentass2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.rent_subsidy == "Yes"))[ , c("TRUE")]
svymean(~rentass1, case.survey, na.rm = TRUE)
svymean(~rentass2 >=1, household.survey)

# hygiene edu
table(case_number_details$section2.case_number_details.assistance.fuel_subsidy)
reduhyg1 <- case_number_details$section2.case_number_details.assistance.edu_hygiene == "Yes"
reduhyg2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.edu_hygiene == "Yes"))[ , c("TRUE")]
svymean(~reduhyg1, case.survey, na.rm = TRUE)
svymean(~reduhyg2 >=1, household.survey)

# any cash assist
checkcash1 <- case_number_details$section2.case_number_details.assistance.cash_assist == "Yes"
checkcash2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.cash_assist == "Yes"))[ , c("TRUE")]
svymean(~checkcash1, case.survey, na.rm = TRUE)
svymean(~checkcash2 >=1, household.survey)

# edu assist
edu1 <- case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_edu == "Yes"
edu2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_edu == "Yes"))[ , c("TRUE")]
svymean(~edu1, case.survey, na.rm = TRUE)
svymean(~edu2 >=1, household.survey)
svytotal(~edu1, case.survey, na.rm = TRUE)
svytotal(~edu2 >=1, household.survey)

# food voucher
food.vouch1 <- case_number_details$section2.case_number_details.assistance.food_vouchers == "Yes"
food.vouch2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.food_vouchers == "Yes"))[ , c("TRUE")]
svymean(~food.vouch1, case.survey, na.rm = TRUE)
svymean(~food.vouch2 >=1, household.survey)
svytotal(~food.vouch1, case.survey, na.rm = TRUE)
svytotal(~food.vouch2 >=1, household.survey)

# mcap in the past 3 months
cash3month1 <- case_number_details$section2.case_number_details.assistance.mcap_assist == "Yes"
cash3month2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.mcap_assist == "Yes"))[ , c("TRUE")]
cash3month3 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.mcap_assist == "Yes"))[ , c("TRUE")]>=1

svymean(~cash3month1, case_number_details.survey, na.rm = TRUE)
svymean(~cash3month2 >=1, household.survey)
svytotal(~cash3month1, case.survey, na.rm = TRUE)
svytotal(~cash3month2 >=1, household.survey)
svymean(~cash3month3, household.survey)
# winter assist
winass1 <- case_number_details$section2.case_number_details.assistance.win_assist == "Yes"
winass2 <- dcast(case_number_details, case_number_details$SET.OF.section2.case_number_details ~ (case_number_details$section2.case_number_details.assistance.win_assist == "Yes"))[ , c("TRUE")]
svymean(~winass1, case.survey, na.rm = TRUE)
svymean(~winass2 >=1, household.survey)
svytotal(~winass1, case.survey, na.rm = TRUE)
svytotal(~winass2 >=1, household.survey)

## checking other indicators 
# dependents per HH 
HHdep <- dcast(individual_biodata, individual_biodata$SET.OF.section2.case_number_details ~ (individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age<=14 | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age>=65))[ , c("TRUE")]
svymean(~HHdep >=1, household.survey)
svytotal(~HHdep >=1, household.survey)
svymean(~HHdep, household.survey)
svytotal(~HHdep, household.survey)
svymean(~HHdep, household.survey)

HHdep <- dcast(individual_biodata, individual_biodata$SET.OF.section2.case_number_details ~ (individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age<=14 | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age>=65))[ , c("TRUE")]
HHworkingage <- dcast(individual_biodata, individual_biodata$SET.OF.section2.case_number_details ~ (individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age >=15 | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age<=65))[ , c("TRUE")]
depratio <- (HHdep/ HHworkingage)
svyratio(~HHdep, HHworkingage, household.survey)
HHdep
HHworkingage

worked <- dcast(individual_biodata, individual_biodata$SET.OF.section2.case_number_details ~(individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.work_schooling.worked == "Yes"))[ , c("TRUE")]
worked
svymean(~worked == 0, household.survey)

svymean(~worked == 0, household.survey)[household$section1.location.district=="North Akkar"]
worked[household$section1.location.district=="North Akkar"]
workedNA <- worked[household$section1.location.district=="North Akkar"]
svymean(~workedNA == 0, household.surveyNA)
household.surveyNA <- household.survey[household$section1.location.district=="North Akkar"]

# health care where needed
accessreq <- household$section7.critical_info.health_access2 == "Yes"[household$section7.critical_info.health_required2 == "Yes"]
summary(household$section7.critical_info.health_required2 == "Yes")
summary(household$section7.critical_info.health_required2 == "Yes"[household$section7.critical_info.health_access2 == "Yes"])
summary(household$section7.critical_info.health_access2 == "Yes")
summary(household$section7.critical_info.health_access2 == "Yes" & household$section7.critical_info.health_required2 == "Yes")

summary(household$section7.critical_info.health_access2 == "Yes"[household$section7.critical_info.health_required2 == "Yes"])
reqandgot <- household$section7.critical_info.health_access2 == "Yes" & household$section7.critical_info.health_required2 == "Yes"
summary(reqandgot)
hh.req <- subset(household.survey, household$section7.critical_info.health_required2 == "Yes")
svymean(~household$section7.critical_info.health_access2 == "Yes", hh.req)
svyby(~household$section7.critical_info.health_access2 == "Yes",~I(household$section7.critical_info.health_required2 == "Yes"),design=household.survey, svymean)
svymean(~household$section7.critical_info.health_access2 == "Yes", subset(household.survey, section7.critical_info.health_required2 == "Yes" & household$section7.critical_info.health_access2 == "Yes"))
length(household$section7.critical_info.health_access2 == "Yes")
length(household$section7.critical_info.health_required2 == "Yes")

# subsetting dataset for health and the survey object for required health so lengths match  
svymean(household$section7.critical_info.health_required2 == "Yes", design = household.survey)
req2 <- subset(household, household$section7.critical_info.health_required2 == "Yes")
hh.svy.req2 <- subset(household.survey, household$section7.critical_info.health_required2 == "Yes")
svymean(~req2$section7.critical_info.health_access2 == "Yes", design = hh.svy.req2)

# subsetting dataset for health and the survey object for required health so lengths match  
svymean(household$section7.critical_info.health_required1 == "Yes", design = household.survey)
req1 <- subset(household, household$section7.critical_info.health_required1 == "Yes")
hh.svy.req1 <- subset(household.survey, household$section7.critical_info.health_required1 == "Yes")
svymean(~req1$section7.critical_info.health_access1 == "Yes", design = hh.svy.req1)
#svymean(~req1$section7.critical_info.health_access_bar == "fees" req1$section7.critical_info.health_access_bar == "costs", design = hh.svy.req1)
#prop.table(table(req1$section7.critical_info.health_access_bar))
svyby(~section7.critical_info.health_access1 == "Yes", ~section1.location.district, hh.svy.req1, svymean)
breakdown.req1 <- svyby(~section7.critical_info.health_access1 == "Yes", ~section1.location.district, hh.svy.req1, svymean)
names(breakdown.req1)
names(breakdown.req1)[2] <- "Did not receive primary health care"
names(breakdown.req1)[3] <- "Received primary health care"
names(breakdown.req1)[4] <- "Standard errors - Did not receive"
names(breakdown.req1)[5] <- "Standard errors - Received primary health care"
breakdown.req1 <- breakdown.req1[ , c(2:5)]
summary(breakdown.req1)
breakdown.req1.rec <- breakdown.req1[ , c(1,2)]
write.csv(breakdown.req1.rec, file = "out/primarybydist.csv")

table <- svyby(~section7.critical_info.health_access1 == "Yes", ~section1.location.district, household.survey, svymean, na.rm=TRUE)

svymean(~household$MEB125, household.survey, na.rm = TRUE)
svymean(~household$MEB125.greater, household.survey, na.rm = TRUE)
svymean(~household$SMEB, household.survey, na.rm = TRUE)
svymean(~household$SMEB.MEB, household.survey, na.rm = TRUE)