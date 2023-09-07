#Script to plot survey maps with underway data

# Wed Jan 11 11:18:10 2023 ------------------------------

#LIBRARIES & SOURCES
#====
library(tidyverse)
library(readxl)
source("scripts/functions/environmental.utils.R")

md <- read_csv("data/biology/metadata.csv")
load(file = "data/environment/mapping/musselPointMap.R")
load("data/biology/cruiseDates.rda")
load("data/environment/CTD/cleaned/Underway/allUnderway.rda")
#====

#SETUP
#====
#create colorblind friendly palette
cbPalette <- c("#000000", "#56B4E9", "#E69F00")

#set levels for segment colors
md$location <- as.factor(md$location)
levels(md$location) <- c("front", "onshore", "offshore")

#set map limits
max.long <- -123.069
min.long <- -123.090
max.lat <- 38.3305
min.lat <- 38.310

cdf <- coastline.df %>% dplyr::filter(long < max.long, long > min.long, lat > min.lat, lat < max.lat)#subset coastline
#====
  
#PLOT SURVEYS
#====
#Cruise 3
cruise3.uw <- ggplot(filter(ldf[[1]], temp <15))+
  geom_point(size = 3, shape = 21, color = "transparent", aes(x = longitude, y = latitude, fill = temp))+
  xlim(min.long, max.long) + 
  ylim(min.lat, max.lat) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[1]]$temp), name = "Temp °C") +
  geom_point(data = cdf, aes(x = long, y = lat), size = 0.1) +
  labs(x = "", y = "Latitude") +
  geom_segment(data = filter(md, date == cruiseDates[2], observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon, color = location), size = 1, inherit.aes = FALSE) +
  scale_color_manual(values = cbPalette) +
  guides(color = "none") +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 11))
ggsave(file = "figures/ctdUnderway/8.15.2019.jpg", plot = cruise3.uw, dpi = 300)

#Cruise 4
cruise4.uw <- ggplot()+
  geom_point(data = cdf, aes(x = long, y = lat), size = 0.1) +
  xlim(min.long, max.long) + 
  ylim(min.lat, max.lat) + 
  labs(x = "Longitude", y = "Latitude") +
  geom_segment(data = filter(md, date == cruiseDates[3], observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon, color = location), size = 1, inherit.aes = FALSE) +
  geom_point(data = filter(md, date == cruiseDates[3], observation =="ctd_profile"), aes(y = start_lat, x = start_lon), color = "black", shape = 4, size = 5) +
  scale_color_manual(values = cbPalette, guide = guide_legend(override.aes = list(color = "white"))) +
  theme_classic() + 
  theme(legend.text = element_text(color = "white"), legend.title = element_text(color = "white"), legend.key = element_rect(fill = "white", color = "white"), text = element_text(size = 20), axis.text = element_text(size = 11))
cruise4.uw
ggsave(file = "figures/ctdUnderway/9.20.2019.jpg")

#Cruise 5
cruise5.uw <- ggplot()+
  geom_point(data = cdf, aes(x = long, y = lat), size = 0.1) +
  xlim(min.long, max.long) + 
  ylim(min.lat, max.lat) + 
  labs(x = "", y = "Latitude") +
  geom_segment(data = filter(md, date == cruiseDates[4], observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon, color = location), size = 1, inherit.aes = FALSE) +
  geom_point(data = filter(md, date == cruiseDates[4], observation =="ctd_profile"), aes(y = start_lat, x = start_lon), color = "black", shape = 4, size = 5) +
  scale_color_manual(values = cbPalette, guide = guide_legend(override.aes = list(color = "white"))
) +
  theme_classic() +
  theme(legend.text = element_text(color = "white"), legend.title = element_text(color = "white"), legend.key = element_rect(fill = "white", color = "white"), text = element_text(size = 20), axis.text = element_text(size = 11))
ggsave(file = "figures/ctdUnderway/6.23.2020.jpg", plot = cruise5.uw, dpi = 300)

#Cruise 6
cruise6.uw <- ggplot(filter(ldf[[4]], density>1024.7))+
  geom_point(size = 3, shape = 21, color = "transparent", aes(x = longitude, y = latitude, fill = temp))+
  xlim(min.long, max.long) + 
  ylim(min.lat, max.lat) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[4]]$temp), name = "Temp °C") +
  geom_point(data = cdf, aes(x = long, y = lat), size = 0.1) +
  labs(x = "", y = "Latitude") +
  geom_segment(data = filter(md, date == cruiseDates[5], observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon, color = location), size = 1, inherit.aes = FALSE) +
  scale_color_manual(values = cbPalette) +
  geom_point(data = filter(md, date == cruiseDates[5], observation =="ctd_profile"), aes(y = start_lat, x = start_lon), color = "black", shape = 4, size = 5) +
  guides(color = "none") +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 11))
ggsave(file = "figures/ctdUnderway/6.30.2020.jpg", plot = cruise6.uw, dpi = 300)
  
#Cruise 7
ggplot(filter(ldf[[5]], density > 1023))+
    geom_point(size = 3, shape = 21, color = "transparent", aes(x = longitude, y = latitude, fill = density))+
  xlim(min.long, max.long) + 
  ylim(min.lat, max.lat) + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[5]]$density), name = "Temp °C") +
  geom_point(data = cdf, aes(x = long, y = lat), size = 0.1) +
  labs(x = "Longitude", y = "Latitude", title = paste(ldf[[5]]$date[1], "Surface Density", sep = " ")) +
    geom_segment(data = filter(md, date == as.POSIXct("2020-07-28"), observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon), color = "black", size = 1, inherit.aes = FALSE) +
    geom_point(data = filter(md, date == as.POSIXct("2020-07-28"), observation =="ctd_profile"), aes(y = start_lat, x = start_lon), color = "black", shape = 4, size = 5) +
  guides(color = "none") +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 11))
ggsave(file = "figures/ctdUnderway/7.28.2020.jpg", dpi = 300)
  
#Cruise 8
ggplot(filter(ldf[[6]], density > 1024.5))+
  geom_point(size = 3, shape = 21, color = "transparent", aes(x = longitude, y = latitude, fill = density))+
  xlim(min.long, max.long) + 
  ylim(min.lat, max.lat) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[6]]$density), name = "Temp °C") +
  geom_point(data = cdf, aes(x = long, y = lat), size = 0.1) +
  labs(x = "Longitude", y = "Latitude", title = paste(ldf[[6]]$date[1], "Surface Density", sep = " ")) +
  geom_segment(data = filter(md, date == as.POSIXct("2020-09-03"), observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon), color = "black", size = 1, inherit.aes = FALSE) +
  geom_point(data = filter(md, date == as.POSIXct("2020-09-03"), observation =="ctd_profile"), aes(y = start_lat, x = start_lon), color = "black", shape = 4, size = 5) +
  guides(color = "none") +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 11))
ggsave(file = "figures/ctdUnderway/9.3.2020.jpg", dpi = 300)

#Cruise 9
ldf[[7]] <- filter(ldf[[7]], density>1025, longitude< -123.077, latitude< 38.35)
cruise9.uw <- ggplot(ldf[[7]])+
  geom_point(size = 3, shape = 21, color = "transparent", aes(x = longitude, y = latitude, fill = temp))+
  xlim(min.long, max.long) + 
  ylim(min.lat, max.lat) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[7]]$temp), name = "Temp °C") +
  geom_point(data = cdf, aes(x = long, y = lat), size = 0.1) +
  labs(x = "Longitude", y = "Latitude") +
  geom_segment(data = filter(md, date == cruiseDates[6], observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon, color = location), size = 1, inherit.aes = FALSE) +
  scale_color_manual(values = cbPalette) +
  geom_point(data = filter(md, date == cruiseDates[6], observation =="ctd_profile"), aes(y = start_lat, x = start_lon), color = "black", shape = 4, size = 5) +
  guides(color = "none") +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 11))
ggsave(file = "figures/ctdUnderway/9.30.2020.jpg", plot = cruise9.uw, dpi = 300)

#Cruise 10
ldf[[8]] <- filter(ldf[[8]], density>1025.34)
cruise10.uw <- ggplot(ldf[[8]])+
  geom_point(size = 3, shape = 21, color = "transparent", aes(x = longitude, y = latitude, fill = temp))+
  xlim(min.long, max.long) + 
  ylim(min.lat, max.lat) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[8]]$temp), name = "Temp °C") +
  geom_point(data = cdf, aes(x = long, y = lat), size = 0.1) +
  labs(x = "Longitude", y = "Latitude") +
  geom_segment(data = filter(md, date == cruiseDates[7], observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon, color = location), size = 1, inherit.aes = FALSE) +
  scale_color_manual(values = cbPalette) + 
  #geom_point(data = filter(md, date == cruiseDates[7], observation =="ctd_profile"), aes(y = start_lat, x = start_lon), color = "black", shape = 4, size = 5) +
  guides(color = "none") +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 11))
ggsave(file = "figures/ctdUnderway/10.6.2020.jpg", plot = cruise10.uw, dpi = 300)
#====

#Go to 6_analysis_plankton_diversity