#Script to tidy environment data

# Thu May 12 10:37:55 2022 ------------------------------

#LIBARIES & SOURCES
#====
library(tidyverse)
library(ggplot2)

source("scripts/data_load.R")
#====

#DATES
#====
cruiseDates <- as.POSIXct(c("2019-07-18", "2019-08-15", "2019-09-20", "2020-06-23", "2020-06-30", "2020-09-30", "2020-10-06"))
save(cruiseDates, file = "data/cruiseDates.rda")
#====

#METDATA
#====
md$date <- as.POSIXct(md$date, format = "%m/%d/%Y")
md$start_lat <- as.numeric(md$start_lat)
md$start_lon <- -1*md$start_lon
md$end_lon <- -1*md$end_lon
save(md, file = "data/metadata.rda")
#====

#CURRENTS
#====
hfr <- hfr[,3:7]#omit sensor id columns
n <- c("lat", "lon", "datetime", "dir", "spe_kts")
names(hfr) <- n
hfr$datetime <- as.POSIXct(hfr$datetime)#convert date to POSIX format
hfr <- filter(hfr, as.numeric(datetime) > as.numeric(as.POSIXct("2019-07-11")))#filter to cruise date interval
hfr <- filter(hfr, as.numeric(datetime) < as.numeric(as.POSIXct("2020-10-13")))
save(hfr, file = "data/environment/hfr.rda")

n <- c("datetime_pdt", "speed")
ids <- c("b1u", "b1v", "b2u", "b2v", "b6u", "b6v")

for(i in 1:6){
boonHFR[[i]] <- boonHFR[[i]][,c(2,4)]
names(boonHFR[[i]]) <- n
boonHFR[[i]]$datetime_pdt <- as.POSIXct(boonHFR[[i]]$datetime_pdt)
boonHFR[[i]]$id <- rep(ids[i], nrow(boonHFR[[i]]))
}
boonHFR.df <- do.call(rbind.data.frame, boonHFR)#bind all hfr rows
save(boonHFR.df, file = "data/environment/hfr/boonHFR.rda")
#====

#WINDS
#====
names_d <- c("datetime", "deg")
names_s <- c("datetime", "spe")

wd19 <- wd19[,c(2,4)]
wd20 <- wd20[,c(2,4)]
ws19 <- ws19[,c(2,4)]
ws20 <- ws20[,c(2,4)]

names(wd19) <- names_d
names(wd20) <- names_d
names(ws19) <- names_s
names(ws20) <- names_s

wd <- rbind(wd19, wd20)
ws <- rbind(ws19, ws20)

save(wd, file = "data/environment/BOONwind/wd.rda")
save(ws, file = "data/environment/BOONwind/ws.rda")
#====