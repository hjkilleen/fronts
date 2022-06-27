#Script to load plankton data

# Tue Dec 14 09:17:03 2021 ------------------------------

#LIBRARIES & SOURCES
#====
library(tidyverse)
library(readxl)
#====

#PLANKTON
#====
# sheet <- excel_sheets("data/biology/counts/07182019.xlsx")
# cruise1 <- lapply(setNames(sheet, sheet), 
#                   function(x) read_excel("data/biology/counts/07182019.xlsx", sheet=x))
# cruise1 <- bind_rows(cruise1, .id="Sheet")
# save(cruise1, file = "data/biology/counts/cruise1.rda")
# 
# sheet <- excel_sheets("data/biology/counts/08152019.xlsx")
# cruise3 <- lapply(setNames(sheet, sheet), 
#                     function(x) read_excel("data/biology/counts/08152019.xlsx", sheet=x))
# cruise3 <- bind_rows(cruise3, .id="Sheet")
# save(cruise3, file = "data/biology/counts/cruise3.rda")
# 
# sheet <- excel_sheets("data/biology/counts/09202019.xlsx")
# cruise4 <- lapply(setNames(sheet, sheet), 
#                   function(x) read_excel("data/biology/counts/09202019.xlsx", sheet=x))
# cruise4 <- bind_rows(cruise4, .id="Sheet")
# save(cruise4, file = "data/biology/counts/cruise4.rda")
# 
# sheet <- excel_sheets("data/biology/counts/06232020.xlsx")
# cruise5 <- lapply(setNames(sheet, sheet), 
#                   function(x) read_excel("data/biology/counts/06232020.xlsx", sheet=x))
# cruise5 <- bind_rows(cruise5, .id="Sheet")
# save(cruise5, file = "data/biology/counts/cruise5.rda")
# 
# sheet <- excel_sheets("data/biology/counts/06302020.xlsx")
# cruise6 <- lapply(setNames(sheet, sheet), 
#                   function(x) read_excel("data/biology/counts/06302020.xlsx", sheet=x))
# cruise6 <- bind_rows(cruise6, .id="Sheet")
# save(cruise6, file = "data/biology/counts/cruise6.rda")
# 
# sheet <- excel_sheets("data/biology/counts/09302020.xlsx")
# cruise9 <- lapply(setNames(sheet, sheet), 
#                   function(x) read_excel("data/biology/counts/09302020.xlsx", sheet=x))
# cruise9 <- bind_rows(cruise9, .id="Sheet")
# save(cruise9, file = "data/biology/counts/cruise9.rda")
# 
# sheet <- excel_sheets("data/biology/counts/10062020.xlsx")
# cruise10 <- lapply(setNames(sheet, sheet), 
#                   function(x) read_excel("data/biology/counts/10062020.xlsx", sheet=x))
# cruise10 <- bind_rows(cruise10, .id="Sheet")
# save(cruise10, file = "data/biology/counts/cruise10.rda")
# 
# #Bind all cruises
# plankton <- bind_rows(cruise1, cruise3, cruise4, cruise5, cruise6, cruise9, cruise10)
# save(plankton, file = "data/biology/counts/plankton.rda")

#Load plankton dataset with edited species and stage IDs
plankton <- read_xlsx("data/biology/counts/planktonEdit.xlsx")
save(plankton, file = "data/biology/counts/plankton.rda")
#====

#CRUISE METADATA
#====
md <- read_csv("data/metadata.csv")
#====

#HFR CURRENTS
#====
hfr <- read_csv("data/environment/hfr/sea_water_velocity.csv")
b1u <- read_csv("data/environment/hfr/bml_b1_u_avg_hourly.csv")
b1v <- read_csv("data/environment/hfr/bml_b1_v_avg_hourly.csv")
b2u <- read_csv("data/environment/hfr/bml_b2_u_avg_hourly.csv")
b2v <- read_csv("data/environment/hfr/bml_b2_v_avg_hourly.csv")
b6u <- read_csv("data/environment/hfr/bml_b6_u_avg_hourly.csv")
b6v <- read_csv("data/environment/hfr/bml_b6_v_avg_hourly.csv")
boonHFR <- list(b1u, b1v, b2u, b2v, b6u, b6v)
#====

#HOURLY WINDS (deg, mph)
#====
wd19 <- read_csv("data/environment/BOONwind/bml_wind_direction_2019_hourly.csv")
ws19 <- read_csv("data/environment/BOONwind/bml_wind_speed_2019_hourly.csv")
wd20 <- read_csv("data/environment/BOONwind/bml_wind_direction_2020_hourly.csv")
ws20 <- read_csv("data/environment/BOONwind/bml_wind_speed_2020_hourly.csv")
#====


