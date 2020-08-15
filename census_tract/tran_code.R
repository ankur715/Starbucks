setwd("~/Desktop/census_tract")
library(tidyr)
library(dplyr)
library(chron)
library(ggplot2)
############## phi ###############3
df <- read.csv('MF_line_right.csv')
df <- mutate(df, avg_weekend = round((Average_Saturday_Ridership + Average_Sunday_Ridership)/2,0))
names(df)
MF = select(df, Station,CT, Average_Weekday_Ridership,avg_weekend)
df2 <- read.csv('BS_line_right.csv')
df2 <- mutate(df2, avg_weekend = round((Average_Saturday_Ridership + Average_Sunday_Ridership)/2,0))
BS = select(df2, Station,CT, Average_Weekday_Ridership,avg_weekend)
df3 <- read.csv('reg_line_right.csv')
names(df3)
df3 <- mutate(df3, Average_Weekday_Ridership = Weekday_Total_Boards + Weekday_Total_Leaves)
df3 <- mutate(df3, Average_Saturday_Ridership = Saturday_Total_Boards + Saturday_Total_Leaves)
df3 <- mutate(df3, Average_Sunday_Ridership = Sunday_Total_Boards + Sunday_Total_Leaves)
df3 <- mutate(df3, avg_weekend = round((Average_Saturday_Ridership + Average_Sunday_Ridership)/2,0))
names(df3)[3] <- 'Station'
reg = select(df3, Station,CT, Average_Weekday_Ridership,avg_weekend)
phi_tran <- rbind(MF,BS,reg)
ct_week <- aggregate(Average_Weekday_Ridership ~ CT, phi_tran, sum)
ct_end  <- aggregate(avg_weekend ~ CT, phi_tran, sum)
tran_all <- ct_week %>%
  inner_join(ct_end, by = c('CT' = 'CT'))
tran_all$Average_Weekday_Ridership <- round(tran_all$Average_Weekday_Ridership,0)
phi <- read.csv('phi_right.csv')
names(phi)[3] <- 'CT'
phi2 <- merge(phi, tran_all, by = 'CT', all = T)
phi3 <- phi2[-c(1,2,3,4),]
phi3$X <- NULL
phi3$weekday_tran <- ifelse(is.na(phi3$Average_Weekday_Ridership), 0 ,phi3$Average_Weekday_Ridership )
phi3$weekend_tran <- ifelse(is.na(phi3$avg_weekend), 0 ,phi3$avg_weekend )
phi3$Average_Weekday_Ridership <- NULL
phi3$avg_weekend <- NULL
write.csv(phi3, file = 'phi_right2.csv')
#####################################################################
chi_stat <- read.csv('chi_station_right.csv')
chi_ride <- read.csv('chi_ridership.csv')
df10 <- select(chi_stat, 'STATION_NAME', 'MAP_ID', "CT")
chi_ride$month_beginning <- as.POSIXct(as.character(chi_ride$month_beginning), format =  "%m/%d/%y")
chi_ride <- mutate(chi_ride, year = format(month_beginning, "%Y"))
chi_ride <- mutate(chi_ride, month = format(month_beginning, "%m"))
df_18 <- filter(chi_ride, year == '2018')
chi_week  <- aggregate(avg_weekday_rides ~ station_id + stationame, df_18, mean)
df_18 <- mutate(df_18, avg_weekend = (avg_saturday_rides + avg_sunday.holiday_rides)/2)
chi_weekend  <- aggregate(avg_weekend ~ station_id + stationame, df_18, mean)
chi_tran_all <- chi_week %>%
  inner_join(chi_weekend, by = c('station_id' = 'station_id', 'stationame' = 'stationame'))
chi_tran_all$avg_weekday_rides <- round(chi_tran_all$avg_weekday_rides,0)
chi_tran_all$avg_weekend <- round(chi_tran_all$avg_weekend,0)
df20 <- select(chi_stat, 'MAP_ID', 'STATION_NAME','CT')
df21 <- unique(df20[c('MAP_ID', 'STATION_NAME','CT')])
names(chi_tran_all)[1] <- 'MAP_ID'
chi_tran1 <- merge(chi_tran_all, df21, by = 'MAP_ID')
######################################################################
ct_week_chi <- aggregate(avg_weekday_rides ~ CT, chi_tran1, sum)
ct_end_chi <- aggregate(avg_weekend ~ CT, chi_tran1, sum)
chi_tran2 <- merge(ct_week_chi, ct_end_chi, by = 'CT')
chi <- read.csv('chi_right.csv')
names(chi)[3] <- 'CT'
chi2 <- merge(chi, chi_tran2, by = 'CT', all = T)
chi3 <- chi2[-c(686,688,689,690,691,692,693,695,696,697,698,699,700,701,702),]
chi3$X <- NULL
chi3$weekday_tran <- ifelse(is.na(chi3$avg_weekday_rides), 0 ,chi3$avg_weekday_rides )
chi3$weekend_tran <- ifelse(is.na(chi3$avg_weekend), 0 ,chi3$avg_weekend )
chi3$avg_weekday_rides <- NULL
chi3$avg_weekend <- NULL
write.csv(chi3, file = 'chi_right2.csv')
#################################################################################################
nyc_week <- read.csv('mta_ridership_week_day.csv')
nyc_weekend <- read.csv('weekend.csv')
names(nyc_weekend)[1] <- 'Check'
names(nyc_weekend)[2] <- 'line'
nyc_tran <- merge(nyc_week, nyc_weekend, by = c('Check','line'), all = T)
nyc_tran_all <- select(nyc_tran, Check, line, X2018, end_2018)
names(nyc_tran_all)[3] <- 'avg_weekday_rides'
names(nyc_tran_all)[4] <- 'avg_weekend'
write.csv(nyc_tran_all, file = 'write.test.csv')
##########3
nyc_stan <- read.csv('nyc_station_right.csv',colClasses=c("Check"="character"))
nyc_tran_all <- read.csv('write.test.csv')
nyc_stan$Stop.Name <- NULL
nyc_tran1 <- merge(nyc_stan,nyc_tran_all, by = c('Check','line'), all = T)
ct_week_nyc  <- aggregate(avg_weekday_rides ~ CT, nyc_tran1, sum)
ct_end_nyc <- aggregate(avg_weekend ~ CT, nyc_tran1, sum)
nyc_tran2 <- merge(ct_week_nyc ,ct_end_nyc , by ='CT')
nyc_dem <- read.csv('nyc_right.csv')
names(nyc_dem)[3] <- 'CT'
nyc2 <- merge(nyc_dem, nyc_tran2, by = 'CT', all = T)
nyc2$weekday_tran <- ifelse(is.na(nyc2$avg_weekday_rides), 0 ,nyc2$avg_weekday_rides )
nyc2$weekend_tran <- ifelse(is.na(nyc2$avg_weekend), 0 ,nyc2$avg_weekend )
nyc2$avg_weekday_rides <- NULL
write.csv(nyc2, file = 'nyc_right2.csv')
nyc2$avg_weekend <- NULL
