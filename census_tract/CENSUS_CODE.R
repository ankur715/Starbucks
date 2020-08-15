library(sf)
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(ggthemes)
library(scales)
library(mapdata)
library(tigris)
library(mapproj)
library(readr)
library(stringi)
library(stringr)
#######################################################
setwd("~/Desktop/census_tract")
chi_city <-read_csv('Census_Tract_chi.csv')
chi_dem <- read_csv('chi_dem.csv')
str(chi_dem )
chi_dem <- select(chi_dem, NAME, state, county, tract, everything())
chi_dem$county <- paste0("0", chi_dem$county)
chi_dem$tract <- stri_pad_left(chi_dem$tract, 6,0)
chi_dem$cen_tract <- paste(chi_dem$state, chi_dem$county, chi_dem$tract, sep='')

chi_dem$county <- NULL
chi_dem$tract <- NULL
chi_dem$state <- NULL
chi_dem$X1 <- NULL
chi_dem <- separate(data = chi_dem, col = NAME, into = c("tract_name", "county"), sep = ",")

names(chi_city)
names(chi_dem)
chi_city$GEOID10 <-  as.character(chi_city$GEOID10)
chi_city2 <- select(chi_city,NAMELSAD10, GEOID10 )
df_chi <- chi_city2 %>%
  inner_join(chi_dem, by  = c('GEOID10' = 'cen_tract','NAMELSAD10' = 'tract_name'))
df_chi$county <- NULL

write.csv(df_chi, file = 'chi_right.csv')
########################################
ph_city <-read_csv('Census_Tracts_phi.csv')
ph_dem <- read_csv('phi_dem.csv')
ph_dem <- select(ph_dem, NAME, state, county, tract, everything())
ph_dem$county <- paste0("0", chi_dem$county)
ph_dem$tract <- stri_pad_left(ph_dem$tract, 6,0)
ph_dem$cen_tract <- paste(ph_dem$state, ph_dem$county, ph_dem$tract, sep='')

ph_dem$county <- NULL
ph_dem$tract <- NULL
ph_dem$state <- NULL
ph_dem$X1 <- NULL
ph_dem <- separate(data = ph_dem, col = NAME, into = c("tract_name", "county"), sep = ",")


ph_city$GEOID10 <-  as.character(ph_city$GEOID10)
ph_city2 <- select(ph_city,NAMELSAD10, GEOID10 )
df_ph <- ph_city2 %>%
  inner_join(ph_dem, by  = c('GEOID10' = 'cen_tract','NAMELSAD10' = 'tract_name'))
df_chi$county <- NULL
write.csv(df_ph, file = 'phi_right.csv')
##########################################3
nyc_city <-read_csv('NYC _ct.csv')
nyc_dem <- read_csv('nyc_dem_data.csv')
names(nyc_city)
nyc_city <- filter(nyc_city,BoroName == 'Manhattan' )
nyc_city <- select(nyc_city, BoroName, the_geom, CT2010)
nyc_city$tract <- stri_pad_left(nyc_city$CT2010, 6,0)
nyc_city[181,'tract'] <- '029900'


nyc_dem$cen_tract <- stri_pad_left(nyc_dem$tract, 6,0)
nyc_dem$X1 <- NULL
df_nyc <- nyc_city %>%
  inner_join(nyc_dem, by  = c('tract' = 'cen_tract'))
df_nyc$county <- paste0("0", df_nyc$county)
df_nyc$cen_tract <- paste(df_nyc$state, df_nyc$county, df_nyc$tract, sep='')
df_nyc <- separate(data = df_nyc, col = NAME, into = c("tract_name", "county"), sep = ",")
df_nyc <- select(df_nyc, tract_name,cen_tract, county, everything() )

df_nyc$county <- NULL
df_nyc$BoroName <- NULL
df_nyc$tract.y <- NULL
df_nyc$CT2010 <- NULL
df_nyc$tract <- NULL
df_nyc$X1 <- NULL
df_nyc$the_geom <- NULL
df_nyc$state <- NULL
write.csv(df_nyc, file = 'nyc_right.csv')

zerp <- filter(df_nyc, total_population == 0)
