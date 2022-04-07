# Thu Oct 15 07:11:24 2020 ------------------------------

#LIBRARIES
library(lubridate)
library(ggplot2)
library(dplyr)
source("scripts/functions/environmental.utils.R")
  
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
#set classes THIS DOESNT WORK
for(i in seq(1:9)){
  ldf[[4]]$latitude <- as.numeric(as.character(ldf[[4]]$latitude))
  ldf[[4]]$longitude <- as.numeric(as.character(ldf[[4]]$longitude))
  ldf[[4]]$density <- as.numeric(as.character(ldf[[4]]$density))
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
load(file = "data/environment/mapping/flah_map_data.R")

ggplot(filter(ldf[[1]], density>1024.8, density <1025.6))+
  geom_point(size = 3, shape = 21, color = "grey", aes(x = longitude, y = latitude, fill = density))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[1]]$density)) +
  geom_point(data = coastline.df %>% dplyr::filter(long < max(ldf[[1]]$longitude) + 0.0075, long > min(ldf[[1]]$longitude), lat > min(ldf[[1]]$latitude) - 0.25, lat < max(ldf[[1]]$latitude)), aes(x = long, y = lat), size = 0.1) +
  labs(x = "Longitude", y = "Latitude", title = paste(ldf[[1]]$date[1], "Surface Density", sep = " ")) +
  geom_segment(data = data.frame( y = 38.32622, x = -123.07793, yend = 38.32713, xend = -123.0846),
               aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1, inherit.aes = FALSE) + 
  geom_segment(data = data.frame( y = 38.32466, x = -123.0817, yend = 38.3261, xend = -123.08662),
               aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1, inherit.aes = FALSE) +
  geom_segment(data = data.frame( y = 38.32374, x = -123.08311, yend = 38.32505, xend = -123.08913),
               aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1, inherit.aes = FALSE) +
  ggsave(file = "figures/ctdUnderway/8.15.2019.jpg")

ggplot(filter(ldf[[4]], density>1024.7))+
  geom_point(size = 3, shape = 21, color = "grey", aes(x = longitude, y = latitude, fill = density))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(ldf[[4]]$density)) +
  geom_point(data = coastline.df %>% dplyr::filter(long < max(ldf[[4]]$longitude) + 0.0075, long > min(ldf[[4]]$longitude), lat > min(ldf[[4]]$latitude) - 0.25, lat < max(ldf[[4]]$latitude)), aes(x = long, y = lat), size = 0.1) +
  labs(x = "Longitude", y = "Latitude", title = paste(ldf[[4]]$date[1], "Surface Density", sep = " ")) +
  geom_segment(data = data.frame( y = 38.32622, x = -123.07793, yend = 38.32713, xend = -123.0846),
               aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1, inherit.aes = FALSE) + 
  geom_segment(data = data.frame( y = 38.32466, x = -123.0817, yend = 38.3261, xend = -123.08662),
               aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1, inherit.aes = FALSE) +
  geom_segment(data = data.frame( y = 38.32374, x = -123.08311, yend = 38.32505, xend = -123.08913),
               aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1, inherit.aes = FALSE) +
  geom_point(data = data.frame(lon = c(-123.08422, -123.08327, -123.08344, -123.08489, -123.08164), lat = c(38.32222, 38.32371, 38.32461, 38.32648, 38.33100)), aes(x = lon, y = lat), color = "black", shape = 4, size = 5) +
  ggsave(file = "figures/ctdUnderway/6.30.2020.jpg")
