#Script to merge survey conditions figure

# Fri Jul  1 13:00:59 2022 ------------------------------

#LOAD SOURCES AND LIBRARIES
#====
library(ggpubr)

source("scripts/figures_windAndCurrents.R")
source("scripts/4_analysis_ctdProfiles.R")
source("scripts/5_analysis_ctdUnderway.R")
#====

#MERGE FIGURES
#====
blank <- ggplot() +
  theme_void()#create blanks for missing datatypes
blank_leg <- ggplot() + #create blank with CTD y axis label
  labs(x = "Distance from onshore cast (m)", y = "") + 
  theme(text = element_text(size = 20), panel.background = element_rect(fill = "transparent"))

ggarrange(cruise3.hfr, cruise3.uw, blank, cruise4.hfr, cruise4.uw, cruise4.ctd, cruise5.hfr, cruise5.uw, cruise5.ctd, cruise6.hfr, cruise6.uw, cruise6.ctd, cruise9.hfr, cruise9.uw, cruise9.ctd, cruise10.hfr, cruise10.uw, blank_leg, ncol = 3, nrow = 6, widths = c(1.1, 1, 1))
ggsave("figures/surveyEnvironment/surveyEnvironment.jpg", width = 20, height = 14, units = "in", dpi = 300)
#====