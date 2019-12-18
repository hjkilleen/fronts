#Exploratory plots of zooplankton data from Fronts study
#set up script
setwd("/Users/helenkilleen/Desktop/CODING/fronts/")
se <- function(x, ...)sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))
# Tue Oct  8 15:54:13 2019 ------------------------------

#LIBRARIES
#==========
library(ggplot2)
library(dplyr)
#==========

#LOAD DATA
#=========
species <- read.csv("data/speciesCounts.csv")
species[is.na(species)]<-0
str(species)
#=========

#EXPLORE
#=========
#Boxplots for most common species 
#=========
#create common species dataset
common <- select(species, "date", "tow", "station", "depth", "Balanus_cyprid", "Balanus_nauplius", "Bivalve_late_larvae", "Chaetognath", "Calanoid_adult", "Diatom", "Evadne", "Fish_egg", "Foram", "Gastropod_adult", "Larvacean", "Phoronid_larva", "Podon", "Polychaete_setager")
levels(common$station) <-c("onshore", "front", "offshore")

#create boxplots
x <- names(common)
x <- x[-c(1:4)]
results <- "figures/speciesByPosition/"

for ( spec in x ) {
    a <- ggplot(common, aes_string("station", spec, fill = "depth"))+
    geom_boxplot()
  ggsave(paste(results, spec, ".pdf", sep = ""))
}

common.m <- reshape2::melt(common, id.vars = c("date", "tow", "station", "depth"))

ggplot(common.m %>% dplyr::group_by(variable, station, depth) %>% summarize_if(.predicate = is.numeric, .fun = mean, na.rm = TRUE)) +
  # geom_point(aes(x = station, y = value, fill = variable), shape = 21) + facet_grid(depth ~ ., scales = 'free')
  geom_line(aes(x = station, y = value, color = variable, group = variable)) + facet_grid(depth ~ ., scales = 'free')

ggplot(common.m %>% dplyr::group_by(variable, station, depth) %>% summarize_if(.predicate = is.numeric, .fun = c(Mean = mean, SE = se), na.rm = TRUE)) +
  geom_point(aes(x = station, y = value_Mean, fill = depth), shape = 21) +
  geom_errorbar(aes(x = station, ymin = value_Mean - value_SE, ymax = value_Mean + value_SE, color = depth )) +
  facet_wrap(~variable, scales = 'free') + theme_bw()
