#Plots for environmental grouping

# Thu May 12 10:42:42 2022 ------------------------------

#L/S
#====
library(tidyverse)

load("data/cruiseDates.rda")
load("data/environment/BOONwind/ws.rda")
load("data/environment/BOONwind/wd.rda")
#====

#Currents
#====
results <- "figures/hfr/"
a <- as.POSIXct("2020-09-03")
for(i in 1:7){
p <- ggplot(filter(boonHFR.df, datetime_pdt > as.POSIXct(as.numeric(a)-86400, origin = "1970-01-01"), datetime_pdt < as.POSIXct(as.numeric(a)+86400, origin = "1970-01-01"), id %in% c("b1u", "b2u", "b6u"))) + 
  geom_point(aes(x = datetime_pdt, y = speed, color = id)) + 
  ggtitle("Eastward BOON Currents", subtitle = print(a))
ggsave(paste(results,paste(a),"_u",".jpg", sep = ""))
}

for(i in 1:7){
  p <- ggplot(filter(boonHFR.df, datetime_pdt > as.POSIXct(as.numeric(cruiseDates[i])-86400, origin = "1970-01-01"), datetime_pdt < as.POSIXct(as.numeric(cruiseDates[i])+86400, origin = "1970-01-01"), id %in% c("b1u", "b2u", "b6u"))) + 
    geom_point(aes(x = datetime_pdt, y = speed, color = id)) + 
    ggtitle("Eastward BOON Currents", subtitle = print(cruiseDates[i]))
  ggsave(paste(results,paste(cruiseDates[i]),"_u",".jpg", sep = ""))
}
#====

#WIND
#====
wind <- left_join(wd, ws)
for(i in 3:7){
  p <- ggplot(filter(wind, datetime > cruiseDates[i]-86400, datetime < cruiseDates[i]+86400)) + 
    geom_point(aes(datetime, deg/8), color = "red") + 
    geom_point(aes(datetime, spe), color = "blue") +
    ggtitle("BOON Wind", subtitle = as.character(cruiseDates[i])) +
    scale_y_continuous(name = "Speed (mph)", 
                       sec.axis = sec_axis(~.*8, name = "Direction"))
  print(p)
}
#====