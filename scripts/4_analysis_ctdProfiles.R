#Script to interpolate and plot CTD profiles from survey dates

# Tue Jan 10 15:31:53 2023 ------------------------------

#LIBRARY & SOURCES
#====
library(tidyverse)
library(viridis)
library(MBA)

load("data/environment/CTD/cleaned/Profiles/allCTD.rda")
#====

#INTERPOLATE & PLOT TEMPERATURE PROFILES
#====
results <- "figures/ctdProfiles/"
temp_list <- list()

for(i in 1:length(ctdDates)){#Interpolate and create plots for each cruise
  d <- filter(allCTD, date == ctdDates[i])
  d <- na.omit(d)#remove NAs
  mba <- mba.surf(d[,c("trans_dist_m", "depth_M", "temp_C")], no.X = 300, no.Y = 300, n = 1, m = 1)
  dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y)
  df <- reshape2::melt(mba$xyz.est$z, varnames = c("Distance", "Depth"), value.name = "Temp")
  
  p <- ggplot(df, aes(Distance, Depth)) + 
    geom_raster(aes(fill = Temp), interpolate = F) + 
    geom_contour(aes(z = Temp)) + 
    geom_point(data = d, aes(trans_dist_m, depth_M), color = "grey") + 
    # ylim(0,100) + 
    # xlim(0, 35) +
    labs(x = "Transect (m)", y = "Depth (m)") + 
    scale_y_reverse() + 
    scale_fill_gradientn(low = "blue", mid = "white", high = "red", midpoint = mean(na.omit(df$Temp))) + 
    theme_light()
  print(p)
  ggsave(paste(results, gsub("/",".",ctdDates[i]), ".pdf", sep = ""))
  temp_list[[i]] = p
}

#save cruise plots as individual objects
cruise4.ctd <- temp_list[[1]]
cruise5.ctd <- temp_list[[2]]
cruise6.ctd <- temp_list[[3]]
cruise9.ctd <- temp_list[[4]]
#====

#INTERPOLATE & PLOT SALINITY PROFILES FOR COMPARISON
#====
results <- "figures/ctdProfiles/salinity/"

for(i in 1:length(ctdDates)){#Interpolate and create plots for each cruise
  d <- filter(allCTD, date == ctdDates[i])
  d <- na.omit(d)#remove NAs
  d <- filter(d, salinity_psu>25)#omit misreads that fall well below typical salinity values
  mba <- mba.surf(d[,c("trans_dist_m", "depth_M", "salinity_psu")], no.X = 300, no.Y = 300, n = 1, m = 1)
  dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y)
  df <- reshape2::melt(mba$xyz.est$z, varnames = c("Distance", "Depth"), value.name = "Sal")
  
  p <- ggplot(df, aes(Distance, Depth)) + 
    geom_raster(aes(fill = Sal), interpolate = F) + 
    geom_contour(aes(z = Sal)) + 
    geom_point(data = d, aes(trans_dist_m, depth_M), color = "grey") + 
    # ylim(0,100) + 
    # xlim(0, 35) +
    labs(x = "Transect (m)", y = "Depth (m)") + 
    scale_y_reverse() + 
    scale_fill_gradientn(colors = viridis_pal()(20), na.value = "transparent") + 
    theme_light()
  print(p)
  ggsave(paste(results, gsub("/",".",ctdDates[i]), ".pdf", sep = ""))
}
#====

#Go to 5_analysis_ctdUnderway