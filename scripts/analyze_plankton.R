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
plankton <- plankton[,-1]

#cast dataframe to site by species format
plankton.site.sp <- dcast(plankton, location+depth~species_stage, value.var="total", fun.aggregate = mean)
plankton.site.sp[is.na(plankton.site.sp)] <- 0
#===============
#Create dissimilarity matrix
#===============
#change second value if you have added more species
p.comm <- plankton.site.sp[,3:89]
p.env <- plankton.site.sp[,1:2]
p.dist <- vegdist(p.comm)
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
