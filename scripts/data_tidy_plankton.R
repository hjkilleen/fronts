#Script to tidy plankton data

# Tue Dec 14 09:35:51 2021 ------------------------------

#LIBARIES & SOURCES
#====
library(tidyverse)
library(ggplot2)

source("scripts/data_load.R")
#====

#SET UP
#====
#create combined species_stage moniker
plankton$spStage <- paste(plankton$species, plankton$stage, sep = "_")
#====

#CLEAN UP COLUMNS
#====
plankton$date <- as.POSIXct(plankton$date)
plankton[,c(2:5, 7:8, 10)] <- lapply(plankton[,c(2:5, 7:8, 10)], factor)
#====

#DERIVED VARIABLES
#====
#create a new variable for count * split (count/five min. tow)
plankton$sampleCount <- plankton$count*plankton$split

#FLOWMETERS
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
fm <- read_csv("data/biology/flowmeterConversion.csv")
names(fm) <- c("index", "flow_per_sec", "speed_cm_per_sec")
plankton <- left_join(plankton, fm[,-1])
#convert speed to volume m3
plankton$volume_total <- plankton$total_time*plankton$speed_cm_per_sec*625/1000000

#create a new variable for count/m3
plankton$total <- plankton$sampleCount/plankton$volume_total
#====

#SAVE
#====
save(plankton, file = "data/biology/counts/plankton.rda")
#====







# Stacked Plot to explore plankton distributions in cruise 3
#ggplot(filter(cruise3, species == "calanoid"), aes(fill=depth, y=total, x=location)) +
  #geom_bar(position="stack", stat="identity")

