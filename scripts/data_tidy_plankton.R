#Script to tidy plankton data

# Tue Dec 14 09:35:51 2021 ------------------------------

#LIBARIES & SOURCES
#====
library(tidyverse)
library(ggplot2)
load("data/biology/counts/plankton.rda")
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
plankton$total <- plankton$count*plankton$split
#create observational tally to estimate frequency of species_stage in dataset
#tally <- tally(group_by(plankton, spStage))
#====

#SAVE
#====
save(plankton, file = "data/biology/counts/plankton.rda")
#====







# Stacked Plot to explore plankton distributions in cruise 3
ggplot(filter(cruise3, species == "calanoid"), aes(fill=depth, y=total, x=location)) +
  geom_bar(position="stack", stat="identity")

