#Script to analyze plankton community data

# Thu Jan 12 16:30:53 2023 ------------------------------

#LIBRARIES & SOURCES
#====
library(reshape2)
library(vegan)

load("data/biology/counts/plankton.rda")
#====

#CREATE DISTANCE MATRIX 
#====
#select grouping metadata
p <- dplyr::select(filter(plankton, n>3), location, date, depth, sample, spStage, total)#omit very rare species
#cast dataframe to site by species format
p.site.sp <- dcast(p, date+location+depth~spStage, value.var="total", fun.aggregate = mean)
p.site.sp[is.na(p.site.sp)] <- 0

#Create dissimilarity matrix
p.comm <- p.site.sp[,4:ncol(p.site.sp)]
p.env <- p.site.sp[,1:3]
p.dist <- vegdist(p.comm)
attach(p.env)
#====

#COMMUNITY ANALYSIS OF SIMILARITY
#====
#Similarity across date
p.ano.st <- anosim(p.dist, date)
summary(p.ano.st) %>% saveRDS(file = "output/anosim_date.rda")
plot(p.ano.st)

#Similarity across location
p.ano.st <- anosim(p.dist, location)
summary(p.ano.st) %>% saveRDS(file = "output/anosim_location.rda")
plot(p.ano.st)

#Similarity across depths
p.ano.st <- anosim(p.dist, depth)
summary(p.ano.st) %>% saveRDS(file = "output/anosim_depth.rda")
plot(p.ano.st)
#====

#COMMUNITY NON-METRIC MULTIDIMENSIONAL SCALING
#====
#Setup environment column classes and levels 
levels(p.env$location) <- c("front", "onshore", "offshore")
p.env$date <- as.factor(p.env$date)

p.NMDS <- metaMDS(p.dist, k=2)#run NMDS

scrs <-#calculate centroids
  scores(p.NMDS, display = "sites", "species")
cent1 <-#for survey date
  aggregate(scrs ~ date, data = p.comm, FUN = "mean")
names(cent1) [-1] <- colnames(scrs)

cent2 <-#for location
  aggregate(scrs ~ location, data = p.comm, FUN = "mean")
names(cent2) [-1] <- colnames(scrs)

#Site Scores
siteScores <- cbind(scrs, p.env)
site.aov <- aov(NMDS1~location, siteScores)
emmeans(site.aov, pairwise~location, at = list(location = c("onshore", "front", "offshore")))#get pairwise comparisons, onshore vs. offshore significantly different < 0.05, front vs. offshore marinally different <0.12.6420.0

#Species Scores
sppscores(p.NMDS) <- p.comm
sppSum <- as.data.frame(summary(p.NMDS$species))
sppScores <- as.data.frame(p.NMDS$species)
sppScores$type <- rownames(sppScores)
write_csv(sppScores, file = "output/sppScores.csv")
#====

#Go to 8_analysis_commonTaxa