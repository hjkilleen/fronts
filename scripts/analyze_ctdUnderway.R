# Thu Oct 15 07:11:24 2020 ------------------------------

#LIBRARIES
library(lubridate)
library(ggplot2)
library(dplyr)
source("scripts/functions/environmental.utils.R")
load("data/metadata.rda")
load(file = "data/environment/mapping/flah_map_data.R")
load("data/cruiseDates.rda")
  
#LOAD DATA
filenames <- list.files("data/environment/underway/", full.names = TRUE)
ldf <- lapply(filenames, read.table)

#RESTRUCTURE/CLEAN DATA
ldf <- lapply(ldf, function(df){df = df[-1,]})
names10 <- c("date", "time_UTC", "pressure", "temp", "conductivity", "latitude", "longitude", "depth", "salinity", "density")
names8 <- c("date", "time_UTC", "pressure", "temp", "conductivity", "depth", "salinity", "density")
for(i in seq(1:9)){if(ncol(ldf[[i]]) == 10){
  names(ldf[[i]]) <- names10
}
  else {
    names(ldf[[i]]) <- names8
  }}
#set classes
for(i in c(1,2,4:9)){
  ldf[[i]]$latitude <- as.numeric(as.character(ldf[[i]]$latitude))
  ldf[[i]]$longitude <- as.numeric(as.character(ldf[[i]]$longitude))
  ldf[[i]]$density <- as.numeric(as.character(ldf[[i]]$density))
}
#add PDT time
for(i in seq(1:9)){
  ldf[[i]]$datetime <- paste(ldf[[i]]$date, ldf[[i]]$time_UTC, sep = " ")
  ldf[[i]]$datetime <- parse_date_time(ldf[[i]]$datetime, orders = "mdy HMS")
  ldf[[i]]$datetime_PDT <- with_tz(ldf[[i]]$datetime, "America/Los_Angeles")
}
#start and end times that pair with list items
times <- data.frame(
  start.times_PDT = as.POSIXct(c("2019-08-15 10:45:00 PDT", "2019-09-20 9:21:00 PDT", "2020-06-23 11:39:00 PDT", "2020-06-30 10:26:00 PDT", "2020-07-28 10:30:00 PDT", "2020-09-03 11:03:00 PDT", "2020-09-30 10:30:00 PDT", "2020-10-06 9:47:00 PDT", "2020-10-06 10:47:00 PDT")),
  end.times_PDT = as.POSIXct(c("2019-08-15 11:05:00 PDT", "2019-09-20 11:43:00 PDT", "2020-06-23 12:03:00 PDT", "2020-06-30 10:46:00 PDT", "2020-07-28 11:08:00 PDT", "2020-09-03 11:37:00 PDT", "2020-09-30 12:35:00 PDT", "2020-10-06 10:40:00 PDT", "2020-10-06 12:39:00 PDT"))
)
#filter underway data
for(i in seq(1:9)){
  ldf[[i]] <- ldf[[i]][ldf[[i]]$datetime_PDT > times[i,1],]
  ldf[[i]] <- ldf[[i]][ldf[[i]]$datetime_PDT < times[i,2],]
}

#PLOTS
#this doesn't work
#lapply(ldf, map.underway)

ggplot(filter(ldf[[1]], density>1024.8, density <1025.6))+
  geom_point(size = 3, shape = 21, color = "transparent", aes(x = longitude, y = latitude, fill = density))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[1]]$density)) +
  geom_point(data = coastline.df %>% dplyr::filter(long < max(ldf[[1]]$longitude) + 0.0075, long > min(ldf[[1]]$longitude), lat > min(ldf[[1]]$latitude) - 0.25, lat < max(ldf[[1]]$latitude)), aes(x = long, y = lat), size = 0.1) +
  labs(x = "Longitude", y = "Latitude", title = paste(ldf[[1]]$date[1], "Surface Density", sep = " ")) +
  geom_segment(data = filter(md, date == cruiseDates[2], observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon), color = "black", size = 1, inherit.aes = FALSE) +
  theme_classic()
  ggsave(file = "figures/ctdUnderway/8.15.2019.jpg")

ggplot(filter(ldf[[4]], density>1024.7))+
  geom_point(size = 3, shape = 21, color = "transparent", aes(x = longitude, y = latitude, fill = density))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[4]]$density)) +
  geom_point(data = coastline.df %>% dplyr::filter(long < max(ldf[[4]]$longitude) + 0.0075, long > min(ldf[[4]]$longitude), lat > min(ldf[[4]]$latitude) - 0.25, lat < max(ldf[[4]]$latitude)), aes(x = long, y = lat), size = 0.1) +
  labs(x = "Longitude", y = "Latitude", title = paste(ldf[[4]]$date[1], "Surface Density", sep = " ")) +
  geom_segment(data = filter(md, date == cruiseDates[5], observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon), color = "black", size = 1, inherit.aes = FALSE) +
  geom_point(data = filter(md, date == cruiseDates[5], observation =="ctd_profile"), aes(y = start_lat, x = start_lon), color = "black", shape = 4, size = 5) +
  theme_classic()
  ggsave(file = "figures/ctdUnderway/6.30.2020.jpg")
  
  ggplot(filter(ldf[[5]], density > 1023))+
    geom_point(size = 3, shape = 21, color = "transparent", aes(x = longitude, y = latitude, fill = density))+
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[5]]$density)) +
    geom_point(data = coastline.df %>% dplyr::filter(long < max(ldf[[5]]$longitude) + 0.0075, long > min(ldf[[5]]$longitude), lat > min(ldf[[5]]$latitude) - 0.25, lat < max(ldf[[5]]$latitude)), aes(x = long, y = lat), size = 0.1) +
    labs(x = "Longitude", y = "Latitude", title = paste(ldf[[5]]$date[1], "Surface Density", sep = " ")) +
    geom_segment(data = filter(md, date == as.POSIXct("2020-07-28"), observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon), color = "black", size = 1, inherit.aes = FALSE) +
    geom_point(data = filter(md, date == as.POSIXct("2020-07-28"), observation =="ctd_profile"), aes(y = start_lat, x = start_lon), color = "black", shape = 4, size = 5) +
    theme_classic()
  ggsave(file = "figures/ctdUnderway/7.28.2020.jpg")
  
  ggplot(filter(ldf[[6]], density > 1024.5))+
    geom_point(size = 3, shape = 21, color = "transparent", aes(x = longitude, y = latitude, fill = density))+
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[6]]$density)) +
    geom_point(data = coastline.df %>% dplyr::filter(long < max(ldf[[6]]$longitude) + 0.0075, long > min(ldf[[6]]$longitude), lat > min(ldf[[6]]$latitude) - 0.25, lat < max(ldf[[6]]$latitude)), aes(x = long, y = lat), size = 0.1) +
    labs(x = "Longitude", y = "Latitude", title = paste(ldf[[6]]$date[1], "Surface Density", sep = " ")) +
    geom_segment(data = filter(md, date == as.POSIXct("2020-09-03"), observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon), color = "black", size = 1, inherit.aes = FALSE) +
    geom_point(data = filter(md, date == as.POSIXct("2020-09-03"), observation =="ctd_profile"), aes(y = start_lat, x = start_lon), color = "black", shape = 4, size = 5) +
    theme_classic()
  ggsave(file = "figures/ctdUnderway/9.3.2020.jpg")
  
  ldf[[7]] <- filter(ldf[[7]], density>1025, longitude< -122.9, latitude< 38.35)
  ggplot(ldf[[7]])+
    geom_point(size = 3, shape = 21, color = "transparent", aes(x = longitude, y = latitude, fill = density))+
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[7]]$density)) +
    geom_point(data = coastline.df %>% dplyr::filter(long < max(ldf[[7]]$longitude) + 0.0075, long > min(ldf[[7]]$longitude), lat > min(ldf[[7]]$latitude) - 0.25, lat < max(ldf[[7]]$latitude)), aes(x = long, y = lat), size = 0.1) +
    labs(x = "Longitude", y = "Latitude", title = paste(ldf[[7]]$date[1], "Surface Density", sep = " ")) +
    geom_segment(data = filter(md, date == cruiseDates[6], observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon), color = "black", size = 1, inherit.aes = FALSE) +
    geom_point(data = filter(md, date == cruiseDates[6], observation =="ctd_profile"), aes(y = start_lat, x = start_lon), color = "black", shape = 4, size = 5) +
    theme_classic()
  ggsave(file = "figures/ctdUnderway/9.30.2020.jpg")
  
  ldf[[8]] <- filter(ldf[[8]], density>1025.32)
  ggplot(ldf[[8]])+
    geom_point(size = 3, shape = 21, color = "transparent", aes(x = longitude, y = latitude, fill = density))+
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[8]]$density)) +
    geom_point(data = coastline.df %>% dplyr::filter(long < max(ldf[[8]]$longitude) + 0.0075, long > min(ldf[[8]]$longitude), lat > min(ldf[[8]]$latitude) - 0.25, lat < max(ldf[[8]]$latitude)), aes(x = long, y = lat), size = 0.1) +
    labs(x = "Longitude", y = "Latitude", title = paste(ldf[[8]]$date[1], "Surface Density", sep = " ")) +
    geom_segment(data = filter(md, date == cruiseDates[7], observation =="mt"), aes(y = start_lat, x = start_lon, yend = end_lat, xend = end_lon), color = "black", size = 1, inherit.aes = FALSE) +
    geom_point(data = filter(md, date == cruiseDates[7], observation =="ctd_profile"), aes(y = start_lat, x = start_lon), color = "black", shape = 4, size = 5) +
    theme_classic()
  ggsave(file = "figures/ctdUnderway/9.30.2020.jpg")