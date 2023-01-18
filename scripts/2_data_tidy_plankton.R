#Script to tidy plankton data

# Tue Jan 10 10:48:41 2023 ------------------------------

#LIBARIES & SOURCES
#====
library(lubridate)

source("scripts/1_data_load.R")
#====

#SET COLUMN CLASSES & LEVELS
#====
plankton$date <- as.POSIXct(plankton$date)#date
plankton[,c(2:5, 7:8)] <- lapply(plankton[,c(2:5, 7:8)], factor)

plankton$location <- factor(plankton$location, levels = c("onshore", "front", "offshore"))#set location and depth bin order
plankton$depth <- factor(plankton$depth, levels = c("surface", "bottom"))
#====

#DERIVED VARIABLES
#====
#create combined species_stage moniker
plankton$spStage <- paste(plankton$species, plankton$stage, sep = "_")

#create a new variable for count * split (count/five min. tow)
plankton$sampleCount <- plankton$count*plankton$split

#create a tally showing the number of samples in which each spStage was observed
x <- tally(group_by(plankton, spStage))
plankton <- left_join(plankton, x)

#merge plankton with flow meter data
x <- select(filter(md, observation == "mt"), tow_cast, start_flow, end_flow, start_time_pdt, end_time)
names(x) <- c("sample", "start_flow", "end_flow", "start_time", "end_time")
x$sample <- as.factor(x$sample)
plankton <- left_join(plankton, x)

#calculate total flow counts
plankton$total_flow <- plankton$end_flow-plankton$start_flow
plankton$total_time <- as.numeric(plankton$end_time-plankton$start_time)
plankton$flow_per_sec <- round(plankton$total_flow/plankton$total_time, digits = 0)
#save counts per sec for comparison to FM chart
#write.csv(unique(plankton$flow_per_sec), "data/biology/flowmeterConversion.csv")

#join converted flowmeter speeds with plankton dataframe
fm <- read_csv("data/biology/flowmeterConversion.csv")
names(fm) <- c("index", "flow_per_sec", "speed_cm_per_sec")
plankton <- left_join(plankton, fm[,-1])

#convert speed to volume m3 assuming net opening is 0.25m x 0.25m
plankton$volume_total <- plankton$total_time*plankton$speed_cm_per_sec*625/1000000

#create a new variable for plankton count/m3
plankton$total <- plankton$sampleCount/plankton$volume_total
#====

#FILTER DATASET
#====
plankton <- filter(plankton, depth != "neuston") %>% droplevels()#drop samples collected in neuston, not analyzed
plankton <- filter(plankton, date != as.POSIXct("2019-07-18")) %>% droplevels()#drop samples from cruise 1, note analyzed
#====

#SAVE
#====
save(plankton, file = "data/biology/counts/plankton.rda")
#====

#Go to 3_data_tidy_environment