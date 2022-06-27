# Thu Oct 15 07:11:15 2020 ------------------------------

library(tidyverse)

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

                     