#Script to plot high frequency radar data for each survey

# Wed Jan 11 14:58:36 2023 ------------------------------

#LIBRARIES & SOURCES
#====
library(tidyverse)

load("data/biology/cruiseDates.rda")
load("data/environment/hfr/boonHFR.rda")
load("data/environment/BOONwind/wd.rda")
load("data/environment/BOONwind/ws.rda")
#====

#Currents
#====
#Manuscript Subplots
#Aug. 15, 2019
cruise3.hfr <- ggplot(filter(boonHFR, datetime_pdt > as.POSIXct(as.numeric(cruiseDates[2]-43200), origin = "1970-01-01"), datetime_pdt < as.POSIXct(as.numeric(cruiseDates[2])+86400, origin = "1970-01-01"), id %in% c("b7u", "b7v"))) + 
  geom_rect(aes(xmin = as.POSIXct("2019-08-15 11:00:00", tz = "America/Los_Angeles"), xmax = as.POSIXct("2019-08-15 14:00:00", tz = "America/Los_Angeles"), ymin = -Inf, ymax = Inf), alpha = 0.25) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ylim(-30, 25) +
  geom_line(aes(x = datetime_pdt, y = speed, color = id)) + 
  scale_color_manual(values = c("grey", "black")) + 
  labs(x = "", y = paste(print(cruiseDates[2]), "Speed (cm/s)", sep = "\n")) +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 12), legend.position = "none", plot.margin = margin(5, 30, 5, 5))
ggsave("figures/surveyEnvironment/cruise3.hfr.jpg", dpi = 300)

#Sept. 20, 2019
cruise4.hfr <- ggplot(filter(boonHFR, datetime_pdt > as.POSIXct(as.numeric(cruiseDates[3]-43200), origin = "1970-01-01"), datetime_pdt < as.POSIXct(as.numeric(cruiseDates[3])+86400, origin = "1970-01-01"), id %in% c("b7u", "b7v"))) + 
  geom_rect(aes(xmin = as.POSIXct("2019-09-20 08:00:00"), xmax = as.POSIXct("2019-09-20 14:00:00"), ymin = -Inf, ymax = Inf), alpha = 0.25, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ylim(-30, 25) +
  geom_line(aes(x = datetime_pdt, y = speed, color = id)) + 
  scale_color_manual(values = c("grey", "black")) + 
  labs(x = "", y = paste(print(cruiseDates[3]), "Speed (cm/s)", sep = "\n")) +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 12), legend.position = "none", plot.margin = margin(5, 30, 5, 5))
ggsave("figures/surveyEnvironment/cruise4.hfr.jpg", plot = cruise4.hfr, dpi = 300)

#June 23, 2020
cruise5.hfr <- ggplot(filter(boonHFR, datetime_pdt > as.POSIXct(as.numeric(cruiseDates[4]-43200), origin = "1970-01-01"), datetime_pdt < as.POSIXct(as.numeric(cruiseDates[4])+86400, origin = "1970-01-01"), id %in% c("b7u", "b7v"))) + 
  geom_rect(aes(xmin = as.POSIXct("2020-06-23 10:45:00"), xmax = as.POSIXct("2020-06-23 15:30:00"), ymin = -Inf, ymax = Inf), alpha = 0.25) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ylim(-30, 25) +
  geom_line(aes(x = datetime_pdt, y = speed, color = id)) + 
  scale_color_manual(values = c("grey", "black")) + 
  labs(x = "", y = paste(print(cruiseDates[4]), "Speed (cm/s)", sep = "\n")) +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 12), legend.position = "none", plot.margin = margin(5, 30, 5, 5))
ggsave("figures/surveyEnvironment/cruise5.jpg", plot = cruise5.hfr, dpi = 300)

#June 30, 2020
cruise6.hfr <- ggplot(filter(boonHFR, datetime_pdt > as.POSIXct(as.numeric(cruiseDates[5]-43200), origin = "1970-01-01"), datetime_pdt < as.POSIXct(as.numeric(cruiseDates[5])+86400, origin = "1970-01-01"), id %in% c("b7u", "b7v"))) + 
  geom_rect(aes(xmin = as.POSIXct("2020-06-30 09:45:00"), xmax = as.POSIXct("2020-06-30 13:30:00"), ymin = -Inf, ymax = Inf), alpha = 0.25, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ylim(-30, 25) +
  geom_line(aes(x = datetime_pdt, y = speed, color = id)) + 
  scale_color_manual(values = c("grey", "black")) + 
  labs(x = "", y = paste(print(cruiseDates[5]), "Speed (cm/s)", sep = "\n")) +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 12), legend.position = "none", plot.margin = margin(5, 30, 5, 5))
ggsave("figures/surveyEnvironment/cruise6.jpg", plot = cruise6.hfr, dpi = 300)

#Aug. 30, 2020
cruise9.hfr <- ggplot(filter(boonHFR, datetime_pdt > as.POSIXct(as.numeric(cruiseDates[6]-43200), origin = "1970-01-01"), datetime_pdt < as.POSIXct(as.numeric(cruiseDates[6])+86400, origin = "1970-01-01"), id %in% c("b7u", "b7v"))) + 
  geom_rect(aes(xmin = as.POSIXct("2020-09-30 09:00:00"), xmax = as.POSIXct("2020-09-30 14:00:00"), ymin = -Inf, ymax = Inf), alpha = 0.25, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ylim(-30, 25) +
  geom_line(aes(x = datetime_pdt, y = speed, color = id)) + 
  scale_color_manual(values = c("grey", "black")) + 
  labs(x = "", y = paste(print(cruiseDates[6]), "Speed (cm/s)", sep = "\n")) +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 12), legend.position = "none", plot.margin = margin(5, 30, 5, 5))
ggsave("figures/surveyEnvironment/cruise9.jpg", plot = cruise9.hfr, dpi = 300)

#Oct. 6, 2020
cruise10.hfr <- ggplot(filter(boonHFR, datetime_pdt > as.POSIXct(as.numeric(cruiseDates[7]-43200), origin = "1970-01-01"), datetime_pdt < as.POSIXct(as.numeric(cruiseDates[7])+86400, origin = "1970-01-01"), id %in% c("b7u", "b7v"))) + 
  geom_rect(aes(xmin = as.POSIXct("2020-10-06 09:00:00"), xmax = as.POSIXct("2020-10-06 13:30:00"), ymin = -Inf, ymax = Inf), alpha = 0.25, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ylim(-30, 25) +
  geom_line(aes(x = datetime_pdt, y = speed, color = id)) + 
  scale_color_manual(values = c("grey", "black")) + 
  labs(x = "Local Time (PDT)", y = paste(print(cruiseDates[7]), "Speed (cm/s)", sep = "\n")) +
  theme_classic() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 12), legend.position = "none", plot.margin = margin(5, 30, 5, 5))
ggsave("figures/surveyEnvironment/cruise10.jpg", plot = cruise10.hfr, dpi = 300)
#====

#WIND PLOTS FOR SUPPLEMENT
#====
wind <- left_join(wd, ws)
for(i in 3:7){
  p <- ggplot(filter(wind, datetime > cruiseDates[i]-86400, datetime < cruiseDates[i]+86400)) + 
    geom_point(aes(datetime, deg/8), color = "red") + 
    geom_point(aes(datetime, spe), color = "blue") +
    ggtitle("BOON Wind", subtitle = as.character(cruiseDates[i])) +
    scale_y_continuous(name = "Speed (mph, blue)", 
                       sec.axis = sec_axis(~.*8, name = "Direction (red)")) +
    theme_classic() +
    theme(text = element_text(size = 20), axis.text = element_text(size = 12), legend.position = "none", plot.margin = margin(5, 30, 5, 5))
  print(p)
  ggsave(paste("figures/supplement/surveyWind/", gsub("-",".",cruiseDates[i]), ".pdf", sep = ""))
}
#====