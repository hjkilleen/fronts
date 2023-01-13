#Script to analyze plankton community diversity

# Thu Jan 12 13:21:14 2023 ------------------------------

#LIBRARIES & SOURCES
#====
library(reshape2)
library(vegan)

load("data/biology/counts/plankton.rda")
#====

#SETUP
#====
tows <- dplyr::select(plankton, sample, date, site, location, depth) %>% distinct()
#====

#SHANNON-WEINER INDEX
#====
#Create wide format table 
p <- dplyr::select(filter(plankton, n>2), location, date, depth, sample, spStage, total)
p.site.sp <- dcast(p, sample~spStage, value.var="total", fun.aggregate = mean)
p.site.sp[is.na(p.site.sp)] <- 0

#Calculate Shannon-Weiner Index
p.site.sp$diversity <- diversity(p.site.sp[,-1], index = "shannon")#calculate Shannon-Weiner diversity of each tow
#Calculate species evenness
p.site.sp$evenness <- diversity(p.site.sp[,-1], index = )

#Attach wide format table to metadata
p.site.sp <- left_join(dplyr::select(p.site.sp, sample, diversity), tows)

#ANOVA model of diversity by location relative to the front
anova.loc.div <- aov(diversity~location, data = p.site.sp)
sw.anova <- summary(anova.loc.div)#get summary table and statistics 
saveRDS(sw.anova, file = "output/swAnova.rda")

#Plot S-W index by sample for supplement
ggplot(p.site.sp) + 
  geom_boxplot(aes(location, diversity, color = depth)) +
  labs(x = "Location", y = "Shannon Index") +
  scale_color_discrete(name = "Depth", labels = c("Surface", "Bottom")) +
  scale_x_discrete(labels = c("Onshore", "Front", "Offshore")) + 
  theme_classic() +
  theme(text = element_text(size = 20))
ggsave(file = "figures/supplement/shannonWeiner.pdf")
#====

#Go to 7_analysis_plankton_ordination