#Script to tidy environment data

# Thu May 12 10:37:55 2022 ------------------------------

#LIBARIES & SOURCES
#====
library(tidyverse)
library(ggplot2)
library(lubridate)
library(geosphere)
library(openair)

source("scripts/data_load.R")
source("../fish/scripts/functions/shift.wind.R")
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
md <- left_join(md, distinct(select(plankton, sample, location, depth)), by = c("tow_cast" = "sample"))
md[,c(16,17)] <- lapply(md[,c(16,17)], as.factor)
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
ids <- c("b1u", "b1v", "b2u", "b2v", "b6u", "b6v", "b7u", "b7v")

for(i in 1:8){
boonHFR[[i]] <- boonHFR[[i]][,c(2,4)]
names(boonHFR[[i]]) <- n
boonHFR[[i]]$datetime_pdt <- as.POSIXct(boonHFR[[i]]$datetime_pdt)
boonHFR[[i]]$id <- rep(ids[i], nrow(boonHFR[[i]]))
}
boonHFR.df <- do.call(rbind.data.frame, boonHFR)#bind all hfr rows
boonHFR.df$datetime_pdt <- with_tz(boonHFR.df$datetime_pdt+25200, tz = "America/Los_Angeles")
save(boonHFR.df, file = "data/environment/hfr/boonHFR.rda")
#====

#WINDS
#====
#BOON WINDS
names_d <- c("datetime", "deg")
names_s <- c("datetime", "spe")

wd19 <- wd19[,c(2,4)]
wd20 <- wd20[,c(2,4)]
ws19 <- ws19[,c(2,4)]
ws20 <- ws20[,c(2,4)]
wdDay19 <- wdDay19[,c(2,4)]
wdDay20 <- wdDay20[,c(2,4)]
wsDay19 <- wsDay19[,c(2,4)]
wsDay20 <- wsDay20[,c(2,4)]

names(wd19) <- names_d
names(wd20) <- names_d
names(ws19) <- names_s
names(ws20) <- names_s
names(wdDay19) <- names_d
names(wdDay20) <- names_d
names(wsDay19) <- names_s
names(wsDay20) <- names_s

wd <- rbind(wd19, wd20)
ws <- rbind(ws19, ws20)
wdDay <- rbind(wdDay19, wdDay20)
wsDay <- rbind(wsDay19, wsDay20)

save(wd, file = "data/environment/BOONwind/wd.rda")
save(ws, file = "data/environment/BOONwind/ws.rda")
save(wdDay, file = "data/environment/BOONwind/wdDay.rda")
save(wsDay, file = "data/environment/BOONwind/wsDay.rda")

#Add alongshore wind stress to daily wind
boonWind <- left_join(wsDay, wdDay)
#convert knots to m/s
boonWind$spe <- boonWind$spe*0.44704
#calculate alongshore wind speed
boonWind <- shift.wind(boonWind, coastline.angle = 320, dir = "deg", spe = "spe", time = "datetime")
#calculate alongshore wind stress; drag coeff=.0014, air density=1.3
boonWind$asws <- abs(boonWind$aspe)*(boonWind$aspe)*(.0014)*(1.3)
#filter to only sampling period
boonWind <- filter(boonWind, datetime>as.POSIXct("2019-07-01"), datetime<as.POSIXct("2020-10-31"))
#save .rda file
save(boonWind, file = "data/environment/46013wind/boonWind.rda")


#NDBC 46013
#tidy tables and bind into single file
windNames <- c("year", "month", "day", "hour", "min", "wd", "ws", "GST", "WVHT", "DPD", "APD", "MWD", "PRES", "ATMP", "WTMP", "DEWP", "VIS", "TIDE")
names(offwind2019) <- windNames
names(offwind2020) <- windNames
offwind <- rbind(offwind2019, offwind2020)
#remove error readings
offwind <- filter(offwind, wd<361)
#create datetime variable in GMT
offwind$datetimeGMT <- as_datetime(paste(offwind$year, offwind$month, offwind$day, offwind$hour, offwind$min, "00", sep = "-"), tz = "UTC")
#create datetime in local time PDT
offwind$datetimePDT <- with_tz(offwind$datetimeGMT, tzone = "America/Los_Angeles")
#local date for openair pkg
offwind$date <- offwind$datetimePDT
#Calculate average daily wind speeds and direction with scalar wind speeds
offwind <- timeAverage(offwind, avg.time = "day", na.action = na.omit)
#convert knots to m/s
offwind$ws <- offwind$ws/0.514444
#calculate alongshore wind speed
offwind <- shift.wind(offwind, coastline.angle = 320, dir = "wd", spe = "ws", time = "date")
#calculate alongshore wind stress; drag coeff=.0014, air density=1.3
offwind$asws <- abs(offwind$aspe)*(offwind$aspe)*(.0014)*(1.3)
#filter to only sampling period
offwind <- filter(offwind, date>as.POSIXct("2019-07-01"), date<as.POSIXct("2020-10-31"))
#save .rda file
save(offwind, file = "data/environment/46013wind/offwind.rda")
#====

#BOON TEMP
#====
names_t <- c("datetime", "temp_C")

temp2019 <- temp2019[,c(2,4)]
temp2020 <- temp2020[,c(2,4)]

names(temp2019) <- names_t
names(temp2020) <- names_t

boonTemp <- rbind(temp2019, temp2020)

save(boonTemp, file = "data/environment/BOONtemp/boonTemp.rda")
#====

#CTD
#====
#Create data table for all CTD files
list_of_files <- list.files(path = "data/environment/CTD/cleaned/Profiles/", recursive = TRUE,
                            pattern = "\\.asc$",
                            full.names = TRUE)

#clean casts
listCTD <- sapply(list_of_files, fread, simplify = FALSE)
listCTD <- lapply(listCTD, function(df){#remove soak time
  if(nrow(df)>30){
    df <- df[-c(1:30),]
  }
})
listCTD[sapply(listCTD, is.null)] <- NULL#remove empty items
listCTD <- lapply(listCTD, function(df){#remove upcast
  x <- max(df$DepSM)
  end <- which(grepl(x, df$DepSM))
  df <- slice(df, 1:end)
})

#bind and add a variable for file name
allCTD <- data.table::rbindlist(listCTD,
                          use.names = TRUE, idcol = "filename")
names(allCTD) <- c("filename", "date", "time_UTC", "PrdM", "temp_C", "C0S/m", "depth_M", "salinity_psu", "density")
#create CSV
write_csv(allCTD, path = "data/environment/CTD/cleaned/Profiles/allCTD.csv")

# #CTD metadata
# ctdMD <- data.frame(
#   filename = unique(allCTD$filename),
#   date = rep(NA, length(unique(allCTD$filename))),
#   location = rep(NA, length(unique(allCTD$filename))),
#   lat = rep(NA, length(unique(allCTD$filename))),
#   lon = rep(NA, length(unique(allCTD$filename)))
# )
# write_csv(ctdMD, path = "data/environment/CTD/cleaned/Profiles/ctdMD.csv")
ctdMD <- read_csv("data/environment/CTD/cleaned/Profiles/ctdMD.csv")

allCTD$date <- as_date(allCTD$date, format = "%m/%d/%Y")#date variable
ctdMD$date <- as_date(ctdMD$date, format = "%m/%d/%y")#date variable
allCTD <- left_join(allCTD, ctdMD)

#add distance from onshore cast variable
allCTD$lon <- -1*allCTD$lon
allCTD$trans_start_lon <- -1*allCTD$trans_start_lon
allCTD <- mutate(allCTD, trans_dist_m = as.integer(round((distHaversine(cbind(trans_start_lon, trans_start_lat), cbind(lon, lat))))))#Add distance along transect variable in m

#QA/QC Temp
allCTD <- filter(allCTD, temp_C>6)

#save
save(allCTD, file = "data/environment/CTD/cleaned/Profiles/allCTD.rda")
#====

#CUTI
#====
save(cuti, file = "data/environment/cuti.rda")
#====