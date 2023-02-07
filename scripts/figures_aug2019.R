#Script to create a figure showing relationship between foam line appearance and environmental conditions near Mussel Point

# Mon Feb  6 13:55:06 2023 ------------------------------

#LIBRARIES & SOURCES
#=====
library(tidyverse)
library(ggbreak)
library(cowplot)
library(ggbreak)
library(readr)

load("data/environment/46013wind/offwind.rda")
load("data/environment/hfr/boonHFR.rda")
load("data/environment/46013wind/hourlyAug19.rda")
tide <- read_csv("data/environment/tide_9415625.csv")
#====

#SET UP
#====
#x-axis labels
x.labs <- c("00 Aug 13", "12 Aug 13", "00 Aug 14", "12 Aug 14", "00 Aug 15", "12 Aug 15", "00 Aug 16", "12 Aug 16", "00 Aug 17")
x.breaks = c(as.numeric(as.POSIXct("2019-08-13 00:00:00")), as.numeric(as.POSIXct("2019-08-13 12:00:00")), as.numeric(as.POSIXct("2019-08-14 00:00:00")), as.numeric(as.POSIXct("2019-08-14 12:00:00")), as.numeric(as.POSIXct("2019-08-15 00:00:00")), as.numeric(as.POSIXct("2019-08-15 12:00:00")), as.numeric(as.POSIXct("2019-08-16 00:00:00")), as.numeric(as.POSIXct("2019-08-16 12:00:00")), as.numeric(as.POSIXct("2019-08-17 00:00:00")))

offwind <- filter(offwind, date > as.POSIXct("2019-08-13"), date < as.POSIXct("2019-08-17"))#filter to survey timeframe

tide$datetime <- as.POSIXct(paste(tide$Date, tide$Time, sep = " "), format = "%m/%d/%y %H:%M:%S")#fix datetime
#====

#SUBPLOTS
#=====
#WIND
wind <- ggplot() + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = .3) + 
  geom_line(data = offwind, aes(x = as.numeric(datetimePDT), y= asws), color = "grey", linewidth = .5) +
  scale_x_continuous(labels = x.labs, breaks = x.breaks) + 
  labs(x = "", y = "Alongshore\nwind stress (Pa)") + 
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 10))

#CURRENTS
current <- ggplot(filter(boonHFR, datetime_pdt > as.POSIXct("2019-08-13"), datetime_pdt < as.POSIXct("2019-08-17"), id %in% c("b7u", "b7v"))) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ylim(-30, 25) +
  geom_line(aes(x = datetime_pdt, y = speed, color = id)) +
  scale_x_continuous(labels = x.labs, breaks = x.breaks) + 
  labs(x = "", y = "Speed cm/s") + 
  scale_color_manual(values = c("grey", "black")) + 
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 10), legend.position = "none")

#TIDES
tide <- ggplot(tide) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ggalt::geom_xspline(aes(x = datetime, y = Pred)) +#make lines curved to resemble tide chart
  scale_x_continuous(labels = x.labs, breaks = x.breaks) + 
  labs(x = "Datetime", y = "NOAA tide prediction (ft)") + 
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 10), legend.position = "none")
#=====

#ARRANGE SUBPLOTS & SAVE
#====
p <- plot_grid(print(wind), print(current), print(tide), ncol = 1, align = "v")
ggsave2(p, file = "figures/aug2019.jpg", width = 8, height = 9, units = "in", dpi = 300)
#====