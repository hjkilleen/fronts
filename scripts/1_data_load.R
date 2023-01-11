#Script to load data

# Tue Jan 10 10:05:11 2023 ------------------------------

#LIBRARIES & SOURCES
#====
library(tidyverse)
library(readxl)
#====

#LOAD PLANKTON DATA
#====
#Load plankton dataset with edited species and stage IDs
plankton <- read_csv("data/biology/counts/plankton.csv")
#====

#LOAD CRUISE METADATA
#====
md <- read_csv("data/biology/metadata.csv")
#====

#LOAD HFR CURRENT DATA
#====
hfr <- read_csv("data/environment/hfr/sea_water_velocity.csv")
boonHFR <- read_csv("data/environment/hfr/boonHFR.csv")
#====

#LOAD WIND DATA
#====
#BOON Hourly (deg, mph)
wd19 <- read_csv("data/environment/BOONwind/bml_wind_direction_2019_hourly.csv")
ws19 <- read_csv("data/environment/BOONwind/bml_wind_speed_2019_hourly.csv")
wd20 <- read_csv("data/environment/BOONwind/bml_wind_direction_2020_hourly.csv")
ws20 <- read_csv("data/environment/BOONwind/bml_wind_speed_2020_hourly.csv")

#Wind data from NDBC buoy 46013
offwind2019 <- read.table("data/environment/46013wind/46013h2019.txt")
offwind2020 <- read.table("data/environment/46013wind/46013h2020.txt")
#====

#LOAD CUTI DATA
#====
cuti <- read_csv("data/environment/CUTI/cuti.csv")
#====

#LOAD BOON TEMPERATURE DATA
#====
boonTemp <- read_csv("data/environment/BOONtemp/boonTemp.csv")
#====

#LOAD CTD Data
#====
ctdMD <- read_csv("data/environment/CTD/cleaned/Profiles/ctdMD.csv")
allCTD <- read_csv("data/environment/CTD/cleaned/Profiles/allCTD.csv")
#====

#Go to 2_data_tidy_plankton
