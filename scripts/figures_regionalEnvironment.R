#Regional environment summary figures

# Sun Jul  3 18:27:48 2022 ------------------------------

#LIBRARIES & SOURCES
#====
library(tidyverse)
library(ggbreak)
library(cowplot)
library(ggbreak)

load("data/environment/46013wind/boonWind.rda")
load("data/environment/46013wind/offwind.rda")
load("data/environment/cuti.rda")
load("data/environment/BOONtemp/boonTemp.rda")
load("data/cruiseDates.rda")
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
offwind <- filter(offwind, date>as.POSIXct("2019-08-01"), date<as.POSIXct("2020-10-31"))
boonWind <- filter(boonWind, datetime>as.POSIXct("2019-09-16"), datetime<as.POSIXct("2020-10-31"))

windPlot <- ggplot(boonWind) + 
  geom_rect(data = highlights, aes(xmin = as.numeric(start), xmax = as.numeric(end), ymin = -Inf, ymax = Inf), alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey", size = .3) + 
  geom_line(data = boonWind, aes(x = as.numeric(datetime), y = asws)) +
  geom_line(data = offwind, aes(x = as.numeric(datetimePDT), y= asws), color = "grey") +
  scale_x_break(dateBreaks, space = 1) + 
  scale_x_continuous(labels = x.labs, breaks = x.breaks) + 
  labs(x = "", y = "Alongshore\nwind stress (Pa)") + 
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 10))

#UPWELLING
cuti$date <- as.POSIXct(paste(cuti$year, cuti$month, cuti$day, sep = "-"))
cuti <- filter(cuti, date>as.POSIXct("2019-08-01"), date<as.POSIXct("2020-10-31"))

cutiPlot <- ggplot(cuti) + 
  geom_rect(data = highlights, aes(xmin = as.numeric(start), xmax = as.numeric(end), ymin = -Inf, ymax = Inf), alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey", size = .3) + 
  geom_line(aes(x = as.numeric(date), y = `38N`)) +
  scale_x_break(dateBreaks, space = 1) + 
  scale_x_continuous(labels = x.labs, breaks = x.breaks) + 
  labs(x = "", y = "CUTI") + 
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 10))

#TEMPERATURE
meanT <- mean(boonTemp$temp_C)
boonTemp <- filter(boonTemp, datetime>as.POSIXct("2019-08-01"), datetime<as.POSIXct("2020-10-31"))

tempPlot <- ggplot(boonTemp) + 
  geom_rect(data = highlights, aes(xmin = as.numeric(start), xmax = as.numeric(end), ymin = -Inf, ymax = Inf), alpha = 0.5) +
  geom_hline(yintercept = meanT, color = "grey", size = .3) + 
  geom_line(aes(x = as.numeric(datetime), y = temp_C)) +
  scale_x_break(dateBreaks, space = 1) + 
  scale_x_continuous(labels = x.labs, breaks = x.breaks) + 
  labs(x = "\nDate", y = "SST (°C)") + 
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 10))
#====


#MERGE PLOTS
#====
p <- plot_grid(print(windPlot), print(cutiPlot), print(tempPlot), ncol = 1, align = "v")
ggsave2(p, file = "figures/regionalEnvironment.pdf", height = 8, width = 8)
#====