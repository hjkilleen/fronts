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

ggarrange(cruise3.hfr, cruise3.uw, blank, cruise4.hfr, blank, cruise4.ctd, cruise5.hfr, cruise5.uw, cruise5.ctd, cruise6.hfr, cruise6.uw, cruise6.ctd, cruise9.hfr, cruise9.uw, cruise9.ctd, cruise10.hfr, cruise10.uw, blank, ncol = 3, nrow = 6)
#====