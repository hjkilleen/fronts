# Mon Oct 19 17:16:13 2020 ------------------------------

#Fronts Plankton Data Analysis

#LIBRARIES
library(tidyverse)
library(viridis)
library(reshape2)
library(vegan)

#LOAD DATA
plankton <- read.csv("data/biology/plankton.csv")

#TIDY DATA
#create species_stage variable
plankton$species_stage <- as.factor(paste(plankton$species, plankton$stage, sep = "_"))
#get rid of neuston
plankton <- filter(plankton, depth != "neuston") %>% droplevels()
#ATTEMPT 1
#cast dataframe to site by species format
plankton.site.sp <- dcast(plankton, location+depth+date~species_stage, value.var="total", fun.aggregate = mean)
#===============
#Create dissimilarity matrix
#===============
#change second value if you have added more species
p.comm <- plankton.site.sp[,4:82]
p.env <- plankton.site.sp[,1:3]
p.dist <- vegdist(p.comm, na.rm = TRUE)
attach(p.env)
#==========
#Similarity across depths
#=========
p.ano.depth <- anosim(p.dist, depth)
summary(p.ano.depth)
plot(p.ano.depth)
#==========
#Similarity across locations
#==========
p.ano.front <- anosim(p.dist, location)
summary(p.ano.front)
plot(p.ano.front)

#NMDS
coldep <- c("orange", "red", "blue")
colloc <- c("bisque3", "red", "cadetblue4")
p.NMDS <- metaMDS(p.dist, k=2, trymax = 100, distance = "bray")
stressplot(p.NMDS)

#NMDS colored by depth
par(mar=c(5,5,2,1))
plot(p.NMDS, cex.lab = 2, cex.axis = 2)
orditorp(p.NMDS, display = "sites", col = "black")

with(p.env, points(p.NMDS, display = "sites", col = colloc[location], pch = 19, cex = 2))
ordiellipse(p.NMDS, groups = p.env$location, draw = "polygon", col = colloc[location])

#NMDS colored by distance from shore
par(mar=c(5,5,2,1))
plot(hf.NMDS, cex.lab = 2, cex.axis = 2)
orditorp(hf.NMDS, display = "sites", col = "red")
with(hf.env, points(hf.NMDS, display = "sites", col = coldist[station], pch = 19, cex = 2))
ordiellipse(hf.NMDS, groups = hf.env$station, draw = "polygon", col = coldist)





#ATTEMPT 2 - Only species_stage with >1 observation
#create vector with only species_stages with >1 observation
common <- filter(plankton %>% group_by(species_stage) %>% tally(), n>2)[,1]
common_plankton <- filter(plankton, species_stage %in% common$species_stage)
#get rid of neuston samples
common_plankton <- filter(common_plankton, depth != "neuston") %>% droplevels()

#cast dataframe to site by species format
common_plankton.site.sp <- dcast(common_plankton, location+depth+date~species_stage, value.var="total", fun.aggregate = mean)
common_plankton.site.sp[is.na(common_plankton.site.sp)] <- 0
#===============
#Create dissimilarity matrix
#===============
#change second value if you have added more species
cp.comm <- common_plankton.site.sp[,4:33]
cp.env <- common_plankton.site.sp[,1:3]
cp.dist <- vegdist(cp.comm)
attach(cp.env)
#==========
#Similarity across depths
#=========
cp.ano.depth <- anosim(cp.dist, depth)
summary(cp.ano.depth)
plot(cp.ano.depth)
#==========
#Similarity across locations
#==========
cp.ano.front <- anosim(cp.dist, location)
summary(cp.ano.front)
plot(cp.ano.front)

#NMDS
coldep <- c("red", "blue")
colloc <- c("bisque3", "red", "cadetblue4")
cp.NMDS <- metaMDS(cp.dist, k=2, trymax = 100, distance = "euclidean")
sppscores(cp.NMDS) <- cp.comm
stressplot(cp.NMDS)

#NMDS colored by location
par(mar=c(5,5,2,1))
plot(cp.NMDS, cex.lab = 2, cex.axis = 2)

with(cp.env, points(cp.NMDS, display = "sites", col = colloc[location], pch = 19, cex = 2))
orditorp(cp.NMDS,display="species",col="red",air=0.01)

orditorp(cp.NMDS, display = "sites", col = "black")
ordiellipse(cp.NMDS, groups = cp.env$location, draw = "polygon", col = colloc[cp.env$location])

