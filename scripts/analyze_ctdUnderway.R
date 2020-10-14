# Wed Oct 14 11:09:34 2020 ------------------------------

#LIBRARIES
library(lubridate)
source("scripts/")

#LOAD DATA
filenames <- list.files("data/environment/CTD/cleaned/Underway/", full.names = TRUE)
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
#add PDT time
for(i in seq(1:9)){
  ldf[[i]]$datetime <- paste(ldf[[i]]$date, ldf[[i]]$time_UTC, sep = " ")
  ldf[[i]]$datetime <- parse_date_time(ldf[[i]]$datetime, orders = "mdy HMS")
  ldf[[i]]$datetime_PDT <- with_tz(ldf[[i]]$datetime, "America/Los_Angeles")
}
View(ldf[[2]])
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
