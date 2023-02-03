#Script to generate a facet plot of common plankton types

# Fri Feb  3 15:29:19 2023 ------------------------------

#LIBRARIES & SOURCES
#====
library(glmmTMB)
library(reshape2)
library(tidyverse)

load("data/biology/counts/plankton.rda")
#====

#SETUP
#====
us <- filter(plankton, n>20)#create a dataframe of "usual suspects" with only the most commonly observed species (50% of all samples collected)
tows <- dplyr::select(us, sample, date, site, location, depth) %>% distinct()#get essential metadata

y.us <- dcast(us, sample~spStage, value.var = "total", fun.aggregate = mean)#create wide df
y.us[is.na(y.us)] <- 0
y.us <- left_join(y.us, distinct(dplyr::select(us, sample, location, depth, date, volume_total)))

y.us$chthamalus.spp_cyprid[which(y.us$date == "2019-08-15")] <- "NA"#place NAs for cruises where no observations were made 
y.us$copepod_nauplius[which(y.us$date == "2019-08-15")] <- "NA"

#set variable names for plotting
names(y.us) <- c("sample", "Balanus crenatus cyprid", "Balanus crenatus late nauplius", "Bivalve veliger", "Calanoid adult", "Chthamalus spp. cyprid", "Copepod metanauplius", "Copepod nauplius", "Cyclopoid adult", "Diatom adult", "Large egg", "Small egg", "Evadne adult", "Gastropod veliger", "Larvacean adult", "Pinnotherid early zoea", "Podon adult", "Polychaete setiger", "location", "depth", "date")

us <- melt(y.us, id=c("sample", "location", "depth", "date"))#tall form data with zeros
us$value <- as.numeric(us$value)
us <- us[1:612,]#drop NAs
#====

#PLOTTING
#====
ggplot(data = us, aes(x = location, y = value, color = depth)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  scale_color_grey() +
  labs(x = "Location", y = bquote('Concentration (number/'~m^3~')')) +
  guides(color = guide_legend(title = "Depth")) +
  theme_classic() +
  theme(axis.title = element_text(size = 20), legend.text = element_text(size = 10), legend.title = element_text(size = 20))
ggsave("figures/commonSpecies.jpeg", width = 13, height = 7, dpi = 300)
#====