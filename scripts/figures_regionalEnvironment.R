#Regional environment summary figures

# Sun Jul  3 18:27:48 2022 ------------------------------

#LIBRARIES & SOURCES
#====
library(tidyverse)
library(ggbreak)
library(cowplot)
library(ggbreak)
library(openair)

load("data/environment/46013wind/boonWind.rda")
load("data/environment/46013wind/offwind.rda")
load("data/environment/hfr/boonHFR.rda")
cuti <- read_csv("data/environment/CUTI/cuti.csv")
load("data/environment/BOONtemp/boonTemp.rda")
load("data/biology/cruiseDates.rda")
#====

#SET UP
#====
#seconds since 1970
dateBreaks <- c(as.POSIXct("2019-09-30"), as.POSIXct("2020-06-01"), as.POSIXct("2020-07-15"), as.POSIXct("2020-09-01"))

cruiseDates <- cruiseDates[2:7]

#survey highlights
highlights <- data.frame(start = cruiseDates,
                         end = cruiseDates + 86400)

#x-axis labels
x.labs <- c("Aug 1", "Sept 1", "Jun 1", "Jul 1", "Sept 1", "Oct 1")
x.nolabs <- c("", "", "", "", "", "")
x.breaks = c(as.numeric(as.POSIXct("2019-08-01")), as.numeric(as.POSIXct("2019-09-01")), as.numeric(as.POSIXct("2020-06-01")), as.numeric(as.POSIXct("2020-07-01")), as.numeric(as.POSIXct("2020-09-01")), as.numeric(as.POSIXct("2020-10-01")))
#====

#SUBPLOTS
#====
#WIND
offwind <- filter(offwind, date>as.POSIXct("2019-08-01"), date<as.POSIXct("2020-10-31"))#filter to survey timeframe
boonWind <- filter(boonWind, datetime>as.POSIXct("2019-09-16"), datetime<as.POSIXct("2020-10-31"))

windPlot <- ggplot(boonWind) + 
  geom_rect(data = highlights, aes(xmin = as.numeric(start), xmax = as.numeric(end), ymin = -Inf, ymax = Inf), alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = .3) + 
  geom_line(data = boonWind, aes(x = as.numeric(datetime), y = asws), linewidth = .5) +
  geom_line(data = offwind, aes(x = as.numeric(datetimePDT), y= asws), color = "grey", linewidth = .5) +
  scale_x_break(dateBreaks, space = 1) + 
  scale_x_continuous(labels = x.labs, breaks = x.breaks) + 
  labs(x = "", y = "Alongshore\nwind stress (Pa)") + 
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 10))
ggsave("figures/regionalEnvironment/windPlot.jpg", plot = windPlot, width = 8, height = 3, units = "in", dpi = 300)

#CURRENTS
boonHFR <- filter(boonHFR, datetime_pdt > as.POSIXct("2019-08-01"), datetime_pdt < as.POSIXct("2020-10-31"), id %in% c("b7u", "b7v"))#filter to survey timeframe
boonHFR$date <- boonHFR$datetime_pdt
x <- timeAverage(boonHFR, avg.time = "day", na.action = na.omit, type = "id")

hfrPlot <- ggplot(x) + 
  geom_rect(data = highlights, aes(xmin = as.numeric(start), xmax = as.numeric(end), ymin = -Inf, ymax = Inf), alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = .3) + 
  geom_line(aes(x = datetime_pdt, y = speed, color = id), linewidth = 0.5) + 
  scale_x_break(dateBreaks, space = 1) + 
  scale_x_continuous(labels = x.labs, breaks = x.breaks) + 
  scale_color_grey() + 
  labs(x = "", y = "Current speed (cm/s)") +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 10)) +
  theme(legend.position = "none")#cruise 3 
ggsave("figures/regionalEnvironment/hfrPlot.jpg", plot = hfrPlot, width = 8, height = 3, units = "in", dpi = 300)

#UPWELLING
cuti$date <- as.POSIXct(paste(cuti$year, cuti$month, cuti$day, sep = "-"))#filter to survey timeframe
cuti <- filter(cuti, date>as.POSIXct("2019-08-01"), date<as.POSIXct("2020-10-31"))

cutiPlot <- ggplot(cuti) + 
  geom_rect(data = highlights, aes(xmin = as.numeric(start), xmax = as.numeric(end), ymin = -Inf, ymax = Inf), alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = .3) + 
  geom_line(aes(x = as.numeric(date), y = `38N`), linewidth = 0.5) +
  scale_x_break(dateBreaks, space = 1) + 
  scale_x_continuous(labels = x.labs, breaks = x.breaks) + 
  labs(x = "", y = "CUTI") + 
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 10))
ggsave("figures/regionalEnvironment/cutiPlot.jpg", plot = cutiPlot, width = 8, height = 3, units = "in", dpi = 300)

#TEMPERATURE
meanT <- mean(boonTemp$temp_C)#get climatology
boonTemp <- filter(boonTemp, datetime>as.POSIXct("2019-08-01"), datetime<as.POSIXct("2020-10-31"))#filter to survey timeframe

tempPlot <- ggplot(boonTemp) + 
  geom_rect(data = highlights, aes(xmin = as.numeric(start), xmax = as.numeric(end), ymin = -Inf, ymax = Inf), alpha = 0.5) +
  geom_hline(yintercept = meanT, color = "black", linetype = "dashed", size = .3) + 
  geom_line(aes(x = as.numeric(datetime), y = temp_C), linewidth = 0.5) +
  scale_x_break(dateBreaks, space = 1) + 
  scale_x_continuous(labels = x.labs, breaks = x.breaks) + 
  labs(x = "\nDate", y = "SST (°C)") + 
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 10))
ggsave("figures/regionalEnvironment/tempPlot.jpg", plot = tempPlot, width = 8, height = 3, units = "in", dpi = 300)
#====


#MERGE PLOTS
#====
p <- plot_grid(print(windPlot), print(hfrPlot), print(cutiPlot), print(tempPlot), ncol = 1, align = "v")
ggsave2(p, file = "figures/regionalEnvironment/regionalEnvironment.jpg", width = 8, height = 12, units = "in", dpi = 300)
#====