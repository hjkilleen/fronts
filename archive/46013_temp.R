t <- read.table("../../../Downloads/46013h2020.txt")
names(t) <- c("year", "month", "day", "hour", "min", "WDIR", "WSPD", "GST", "WVHT", "DPD", "APD", "MWD", "PRES", "ATMP", "WTMP", "DEWP", "VIS", "TIDE")
t$date <- paste(t$year, t$month, t$day, sep ="-")
t$time <- paste(t$hour, t$min, sep = ":")
t$datetime <- paste(t$date, t$time, sep = " ")
t$datetime <- as.POSIXct(t$datetime)
t <- dplyr::filter(t, WTMP<40)
plot(t$datetime, t$WTMP)

library(ggplot2)
library(zoo)
ggplot(t, aes(datetime, WTMP)) + 
  geom_point(alpha = 0.1) + 
  geom_line(aes(y = rollmean(WTMP, 1000, na.pad = TRUE)), color = "red")#~1 week averaging
