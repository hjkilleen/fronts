#Script to tidy environment data

# Thu May 12 10:37:55 2022 ------------------------------

#LIBARIES & SOURCES
#====
library(lubridate)
library(geosphere)
library(openair)
library(data.table)

source("scripts/1_data_load.R")
source("scripts/functions/shift.wind.R")
load("data/biology/cruiseDates.rda")
#====

#CURRENTS
#====
#Fix NOAA currents
hfr <- hfr[,3:7]#omit sensor id columns
n <- c("lat", "lon", "datetime", "dir", "spe_kts")
names(hfr) <- n
hfr$datetime <- as.POSIXct(hfr$datetime)#convert date to POSIX format
hfr <- filter(hfr, as.numeric(datetime) > as.numeric(as.POSIXct("2019-07-11")))#filter to cruise date interval
hfr <- filter(hfr, as.numeric(datetime) < as.numeric(as.POSIXct("2020-10-13")))
save(hfr, file = "data/environment/hfr/sea_water_velocity.rda")

#Fix BOON currents
boonHFR$datetime_pdt <- with_tz(boonHFR$datetime_pdt+25200, tz = "America/Los_Angeles")
save(boonHFR, file = "data/environment/hfr/boonHFR.rda")
#====

#WINDS
#====
#BOON winds
names_d <- c("datetime", "deg")
names_s <- c("datetime", "spe")

wd19 <- wd19[,c(2,4)]#bind 2019 and 2020 wind data into a single dataframe
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

#NDBC 46013 winds
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
offwind <- filter(offwind, date>as.POSIXct("2019-05-01"), date<as.POSIXct("2020-10-31"))
#save .rda file
save(offwind, file = "data/environment/46013wind/offwind.rda")
#====

#CTD PROFILER
#====
# #Uncomment to add new CTD data
# #Create data table for all CTD files
# list_of_files <- list.files(path = "data/environment/CTD/cleaned/Profiles/", recursive = TRUE,
#                             pattern = "\\.asc$",
#                             full.names = TRUE)
# 
# #clean casts
# listCTD <- sapply(list_of_files, fread, simplify = FALSE)
# listCTD <- lapply(listCTD, function(df){#remove soak time
#   if(nrow(df)>30){
#     df <- df[-c(1:25),]
#   }
# })
# listCTD[sapply(listCTD, is.null)] <- NULL#remove empty items
# listCTD <- lapply(listCTD, function(df){#remove upcast
#   x <- max(df$DepSM)
#   end <- which(grepl(x, df$DepSM))
#   df <- slice(df, 1:end)
# })
# 
# #bind and add a variable for file name
# allCTD <- data.table::rbindlist(listCTD,
#                           use.names = TRUE, idcol = "filename")
# names(allCTD) <- c("filename", "date", "time_UTC", "PrdM", "temp_C", "C0S/m", "depth_M", "salinity_psu", "density")
# #create CSV
# write_csv(allCTD, file = "data/environment/CTD/cleaned/Profiles/allCTD.csv")

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

#UNDERWAY CTD
#====
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
  ldf[[i]]$temp <- as.numeric(as.character(ldf[[i]]$temp))
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
names(ldf) <- c("cruise3", "cruise4", "cruise5", "cruise6", "cruise7", "cruise8", "cruise9", "cruise10", "cruise10.2", "x")
save(ldf, file = "data/environment/CTD/cleaned/Underway/allUnderway.rda")
#====

#Go to 4_analysis_ctdProfiles

