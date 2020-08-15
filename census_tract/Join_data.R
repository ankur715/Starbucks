library(plyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)
library(zoo)
options(scipen=999)
setwd("~/Desktop/kailua")
###################################3
#df <- read_csv('data_1.csv')
df <- read_csv('not_clean_data.csv')
df2 <- read_csv('PHKO_2_2017_2018.csv')  #transtit website
df2$X1 <- NULL
df$X1 <- NULL
#############
df2$DATE <- as.Date(df2$cut_date)
df2$Time <- format(df2$cut_date,"%H:%M:%S")
df2$Hour <- format(df2$cut_date, "%H" )
df2$Min <- format(df2$cut_date, "%M" )
df2$TIME_HST <- paste(df2$Hour,"",df2$Min, sep = "")
df2$TIME_HST <- as.numeric(df2$TIME_HST)
df2 <- df2[,c(9,13,2,3,4,5,6,7,8)]
colSums(is.na(df2))
df22 <- na.locf(df2, na.rm = FALSE,na.remaining = "keep", maxgap = 8)
colSums(is.na(df22))
###################3
#not_in = anti_join(df,df22, by = c('DATE', 'TIME_HST'))
############################
df_all <- left_join(df,df22, by = c('DATE','TIME_HST'))
colSums(is.na(df_all))
na <- df_all[!complete.cases(df_all),]
##################################
#df55 <- na.locf(df_all, na.rm = FALSE,na.remaining = "keep", maxgap = 8)
#colSums(is.na(df55))
#na2 <- df55[!complete.cases(df55),]
#df60 <- drop_na(df55)
#################3
write.csv(df_all, 'master_no_clean_data.csv')
################
ff = filter(df_all, Year == 2019)
write.csv(ff, '2019_q1_q2.csv')
