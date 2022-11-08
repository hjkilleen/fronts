#Script to merge survey conditions figure

# Fri Jul  1 13:00:59 2022 ------------------------------

#LOAD SOURCES AND LIBRARIES
#====
library(ggpubr)

source("scripts/figures_environment.R")
source("scripts/analyze_ctdUnderway.R")
source("scripts/analyze_ctdProfiles.R")
#====

#MERGE FIGURES
#====
blank <- ggplot() +
  theme_void()#create blanks for missing datatypes

ggarrange(cruise3.hfr, cruise3.uw, blank, cruise4.hfr, cruise4.uw, cruise4.ctd, cruise5.hfr, cruise5.uw, cruise5.ctd, cruise6.hfr, cruise6.uw, cruise6.ctd, cruise9.hfr, cruise9.uw, cruise9.ctd, cruise10.hfr, cruise10.uw, blank, ncol = 3, nrow = 6)
#====