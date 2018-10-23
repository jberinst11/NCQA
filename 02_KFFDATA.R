#Install Packages
install.packages("readr")
install.packages("data.table")

#Load Library
library(readr)
library(data.table)

# Total Residents in 2016 from KFF 
total_residents_2016 <-fread("Total Number of Residents.csv")
total_residents_2016 <- total_residents_2016[-1, ]   
names(total_residents_2016) <- c("year", "state", "total_residents_2016")


# Population Distribution by Age in 2016 from KFF
pop_dis_by_age <- fread("Population Distribution by Age.csv")
pop_dis_by_age<- pop_dis_by_age[-1, ]
names(pop_dis_by_age) <- c("year", "state", "no_pop_age0-18_2016", "no_pop_age19-25_2016",
                                 "no_pop_age26-34_2016", "no_pop_age35-54_2016", "no_pop_age55-64_2016", 
                                 "no_pop_age65+_2016", "no_pop_total_2016","per_pop_age0-18_2016", "per_pop_age19-25_2016",
                                 "per_pop_age26-34_2016", "per_pop_age35-54_2016", "per_pop_age55-64_2016", 
                                 "per_pop_age65+_2016", "per_pop_total_2016")



#Population Distribution by Citizenship Status for 2016 from KFF
pop_dis_by_cit_stat <- fread("Population Distribution by Citizenship Status.csv") 
pop_dis_by_cit_stat <- pop_dis_by_cit_stat[-1, -c(5,8)]
names(pop_dis_by_cit_stat) <- c("year", "state", "no_cit_stat_2016", "no_noncit_stat_2016", "per_cit_stat_2016", "per_noncit_stat_2016")


#Population Distribution by Family Structure for 2016 from KFF
pop_dis_fam_str <- fread("Population Distribution by Family Structure.csv")
pop_dis_fam_str <- pop_dis_fam_str[-1, -c(5,8)] 
names(pop_dis_fam_str) <- c("year", "state", "no_with_child_2016", "no_without_child_2016", "per_with_child_2016", "per_without_child_2016")

#Population Distribution by Gender for 2016 from KFF
pop_dis_gen <- fread("Population Distribution by Gender.csv")
pop_dis_gen <- pop_dis_gen[-1, -c(5,8)] 
names(pop_dis_gen) <- c("year", "state", "no_male_2016", "no_female_2016", "per_male_2016", "per_female_2016")

#Population Distribution by Race/Ethnicity
pop_dis_race <- fread("Population Distribution by RaceEthnicity.csv")
pop_dis_race <- pop_dis_race[-1, -c(10, 18)]
names(pop_dis_race) <- c("year", "state","no_white_2016", "no_black_2016", "no_hispanic_2016", "no_asian_2016", "no_native_2016", "no_pacific_2016", "no_two_or_more_races_2016",
                         "per_white_2016", "per_black_2016", "per_hispanic_2016", "per_asian_2016", "per_native_2016", "per_pacific_2016", "per_two_or_more_races_2016")

