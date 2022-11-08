# Thu Oct 15 07:11:15 2020 ------------------------------

library(tidyverse)
library(MBA)

load("data/environment/CTD/cleaned/Profiles/allCTD.rda")

#CTD plots
names <- c( "date", "time_UTC", "pressure", "temp", "cond", "depth", "sal", "density")
  
on <- read.table("data/environment/CTD/cleaned/Profiles/20190718__033_filter_align_derive_bin.asc", skip = 1)
names(on) <- names
on$pos <- rep("on", nrow(on))
f <- read.table("data/environment/CTD/cleaned/Profiles/20190920_HK_002_filter_align_derive_bin.asc", skip = 1)
names(f) <- names
f$pos <- rep("f", nrow(f))
off <- read.table("data/environment/CTD/cleaned/Profiles/20190718__032_filter_align_derive_bin.asc", skip = 1)
names(off) <- names
off$pos <- rep("off", nrow(off))

df <- rbind(on, f, off)
  
ggplot(df) +
  geom_point(aes(temp, depth), color = "red") + 
  geom_point(aes(sal/2.65, depth), color = "blue") + 
  scale_x_continuous(name = "temp",
                     sec.axis = sec_axis(~.*2.65, name = "sal"), limits = c(10,13)) + 
  scale_y_reverse() +
  facet_wrap(~pos)




results <- "figures/ctdProfiles/"
temp_list <- list()

ctdDates <- as.POSIXct(c("2019-09-20", "2020-06-23", "2020-06-30", "2020-09-30"), tz = "America/Los_Angeles")
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
    scale_fill_gradientn(colors = viridis_pal()(20), na.value = "transparent") + 
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
