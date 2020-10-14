# Wed Oct 14 11:09:34 2020 ------------------------------

#LIBRARIES
library(lubridate)

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

#PLOTS
times <- data.frame(
  start.times <- c(),
  end.times <- c()
)