setwd("~/Desktop/census_tract")
library(tidyr)
library(dplyr)
library(chron)
library(ggplot2)
######################## Loading data #############
chi <- read.csv('chi_data_2.csv')
phi <- read.csv('phi_data_2.csv')
nyc <- read.csv('nyc_data.csv')
################################### Cleaning Data #################
########## Viewing censu tract with population of zero 
pop_zero_nyc <- filter(nyc,total_population == 0 )
pop_zero_chi <- filter(chi,total_population == 0 )
pop_zero_phi <- filter(phi,total_population == 0 )
##################################
####### Viewing columns with NA's in them
na_count <-sapply(nyc, function(y) sum(length(which(is.na(y)))))
data.frame(na_count)
na_count_phi <-sapply(phi, function(y) sum(length(which(is.na(y)))))
data.frame(na_count_phi)
na_count_chi <-sapply(chi, function(y) sum(length(which(is.na(y)))))
data.frame(na_count_chi)
################################### took the median of each column for NA's ########
fill_missing <- function(dataframe) {
  
  df_to_return <- dataframe
  for(i in 3:25){
    df_to_return[is.na(df_to_return[,i]), i] <- median(df_to_return[,i], na.rm = TRUE)
  }
  return(df_to_return)
}
#############
nyc2 <- fill_missing(nyc)  
na_count <-sapply(nyc2, function(y) sum(length(which(is.na(y)))))
data.frame(na_count)
############
phi2 <- fill_missing(phi)  
na_count <-sapply(phi2, function(y) sum(length(which(is.na(y)))))
data.frame(na_count)
#############
############
chi2 <- fill_missing(chi)  
na_count <-sapply(chi2, function(y) sum(length(which(is.na(y)))))
data.frame(na_count)
######################## Created columns in clean data city
chi2$city <- 'chicago'
phi2$city <- 'philadelphia'
nyc2$city <- 'manhattan'
names(chi2)[2] <- 'tract_name'
names(phi2)[2] <- 'tract_name'
df_all <- rbind(chi2, phi2, nyc2)
names(df_all)
##################### Created new columns ###############
df_all2 <- df_all %>%
  mutate(total_workers = male_workers + Female_workers,
         percent_workers = total_workers / total_population,
         leave_7_9 = rowSums(df_all[,10:14], na.rm = T),
         percent_leave_7_9 = leave_7_9 / total_workers,
         perc_hs_dipl = high_school_diploma / total_population,
         perc_bach_deg = bachelors_degree / total_population,
         perc_masters_deg = masters_degree / total_population,
         perc_walk_to_work = walk_to_work / total_workers,
         perc_car_to_work = car_to_work / total_workers,
         perc_pub_tran = pub_tran / total_workers,
         perc_bicycle_to_work = bicycle_to_work / total_workers,
         perc_work_from_home = work_from_home / total_workers,
         time_to_work = Total_time_to_work/total_people_time_to_work)
na <- df_all2[!complete.cases(df_all2),]
################################ filled in data with population of zero with zeros
fill_zero <- function(dataframe) {
  
  df_to_return <- dataframe
  for(i in 31:43){
    df_to_return[is.na(df_to_return[,i]), i] <- 0
  }
  return(df_to_return)
}
df_all3 <- fill_zero(df_all2)
na <- df_all3[!complete.cases(df_all3),]
write.csv(df_all3, file = 'All_columns.csv')
################################# Created final dataset
write.csv(df_all3, file = 'All_columns.csv')
df_all4 <- select(df_all3,CT,tract_name, total_population, median_age, total_households,median_hh_income,median_rent,
                  median_home_value,percent_workers, percent_leave_7_9,perc_hs_dipl,perc_bach_deg, perc_masters_deg,
                  perc_walk_to_work, perc_car_to_work, perc_pub_tran, perc_bicycle_to_work, perc_work_from_home,
                  time_to_work,weekday_tran,weekend_tran, star_count, starbucks_flag,city )
write.csv(df_all4, file = 'testing_dataset.csv')
