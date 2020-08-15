setwd("~/Desktop/census_tract")
library(tidyr)
library(dplyr)
library(chron)
library(ggplot2)
###################################################
###### Loaded census tract data from census api
nyc <- read.csv('star_nyc_census_trac.csv')
chi <- read.csv('star_chi_census_trac.csv')
phi <- read.csv('star_phi_census_trac.csv')
chi <- filter(chi, match != 'No Match')
nyc <- filter(nyc, match != 'No Match')
########## Loaded transportation data
nyc_cen <- read.csv('nyc_right2.csv')
chi_cen <- read.csv('chi_right2.csv')
phi_cen <- read.csv('phi_right2.csv')
############# Merged transportation data with census tract data #############
############# NYC
df_nyc <- nyc %>%
  group_by(CT) %>%
  summarize(count = n())
nyc2 <- merge(nyc_cen, df_nyc, by = 'CT', all = T)
nyc2$star_count <- ifelse(is.na(nyc2$count), 0 ,nyc2$count )
nyc2$count <- NULL
############# CHI 
df_chi <- chi %>%
  group_by(CT) %>%
  summarize(count = n())
chi2 <- merge(chi_cen, df_chi, by = 'CT', all = T)
chi3 <- chi2[-c(803,688),]
chi3$star_count <- ifelse(is.na(chi3$count), 0 ,chi3$count )
chi3$count <- NULL
########### PHI
df_phi <- phi %>%
  group_by(CT) %>%
  summarize(count = n())

phi2 <- merge(phi_cen, df_phi, by = 'CT', all = T)
phi2$star_count <- ifelse(is.na(phi2$count), 0 ,phi2$count )
phi2$count <- NULL
############# Created column that showed if a starbucks was in census tract
nyc2 <- nyc2 %>%
  mutate(starbucks_flag = if_else(CT %in% nyc$CT, 'Starbucks', 'Non-Starbucks'))
write.csv(nyc2, file = 'nyc_data.csv')

chi3 <- chi3 %>%
  mutate(starbucks_flag = if_else(CT %in% chi$CT, 'Starbucks', 'Non-Starbucks'))
write.csv(chi3, file = 'chi_data.csv')

phi2 <- phi2 %>%
  mutate(starbucks_flag = if_else(CT %in% phi$CT, 'Starbucks', 'Non-Starbucks'))
write.csv(phi2, file = 'phi_data.csv')
######################