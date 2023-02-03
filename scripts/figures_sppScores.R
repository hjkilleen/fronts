#Script to plot NMDS analysis

# Fri Feb  3 15:20:16 2023 ------------------------------

#LIBRARIES & SOURCES
#====
library(readr)
source("scripts/7_analysis_plankton_ordination.R")

sppScores <- read_csv("output/sppScores_annotated_1.csv")
#====

#SET UP
#====
sppScores$type <- as.factor(sppScores$type)
sppScores$group <- as.factor(sppScores$group)
sppScores <- mutate(sppScores, x = MDS1*MDS2)
sppScores.2 <- filter(sppScores, type %in% filter(plankton, n>20)$spStage)
sppScores <- filter(sppScores, !group %in% c("Shelf Meroplankton", "Generalist"))
#====

#PLOTTING
#====
ggplot(sppScores) + 
  geom_boxplot(aes(x = MDS1, y = group), fill = "lightgrey", alpha = 0.5, size = .2) +
  geom_vline(xintercept = cent2$NMDS1[1], color = "#E69F00", linewidth = 1) + 
  geom_vline(xintercept = cent2$NMDS1[2], color = "#000000", linewidth = 1) + 
  geom_vline(xintercept = cent2$NMDS1[3], color = "#56B4E9", linewidth = 1) + 
  geom_point(aes(x = MDS1, y = group), size = 2) + 
  geom_point(data = sppScores.2, aes(x = MDS1, y = group), color = "red", size = 2) + 
  labs(x = "NMDS 1", y = "") +
  theme_minimal() + 
  theme(text = element_text(size = 20), axis.text = element_text(size = 10), legend.position = "none")
ggsave("figures/sppScores.jpg", width = 6, height = 3, units = "in", dpi = 300)
#====

#GROUP COMPARISONS
#====
car::Anova(lm(MDS1~group, sppScores))
emmeans::emmeans(lm(MDS1~group, sppScores), pairwise~group)
#====