therm1 <- read_csv("data/environment/thermistor/drive-download-20220711T231224Z-001/MP_Fronts_Front_Top_0.csv")
therm2 <- read_csv("data/environment/thermistor/drive-download-20220711T231224Z-001/MP_Fronts_Front_Top.csv")

therm <- rbind(therm1, therm2)
therm$datetime <- as.POSIXct(strptime(therm$datetime, format = "%m/%d/%y %H:%M"))
therm$Temp_C <- (therm$Temp_F-32)*(5/9)

load("data/environment/hfr/boonHFR.rda")
curr <- filter(boonHFR.df, datetime_pdt > as.POSIXct("2019-07-23"), datetime_pdt < as.POSIXct("2019-10-17"))

ggplot(therm) + 
  #geom_line(aes(datetime, Temp_C)) + 
  geom_line(data = curr, aes(x = datetime_pdt, y = speed))
