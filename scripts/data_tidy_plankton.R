#Script to tidy plankton data

# Tue Dec 14 09:35:51 2021 ------------------------------

#LIBARIES & SOURCES
#====
library(tidyverse)
library(ggplot2)
load("data/biology/counts/plankton.rda")
#====

#DERIVED VARIABLES
#====
#eliminate unused columns
plankton <- plankton[,2:10]
#create a new variable for count * split (count/five min. tow)
plankton$total <- plankton$count*plankton$split
#====









# Stacked Plot to explore plankton distributions in cruise 3
ggplot(filter(cruise3, species == "calanoid"), aes(fill=depth, y=total, x=location)) +
  geom_bar(position="stack", stat="identity")

