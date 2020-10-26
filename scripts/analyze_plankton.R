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




#Look at common species plots relative to front
vcommon <- filter(plankton %>% group_by(species_stage) %>% tally(), n>3)[,1]

for(i in seq(1:length(vcommon$species_stage))){
  x <- filter(plankton, species_stage == i) %>% 
    group_by_at(.vars = c("location", "depth")) %>% 
    summarize(total = mean(total))
  ggplot()
}



#Heatmap
#========
vcommon_plankton <- filter(plankton, species_stage %in% vcommon$species_stage)#only plankton with greater than 3 observations
vcommon_plankton$position <- paste(vcommon_plankton$location, vcommon_plankton$depth, sep = "_")#create position variable 
df <- vcommon_plankton %>% 
  group_by_at(.vars = c("species_stage", "position")) %>% 
  summarize(total = mean(total))

s <- as.data.frame(df) %>% 
  group_by_at(.vars = "species_stage") %>% 
  summarize(sum = sum(total))#get total mean observation values

df <- left_join(df, s)
df$total_mean <- df$total/df$sum#total_mean value is a proportion of all observations of each species_stage
df$position <- factor(df$position, levels = c("onshore_bottom", "onshore_surface", "front_bottom", "front_surface", "offshore_bottom", "offshore_surface"))

heatmap <- ggplot(df, aes(x=position, y =species_stage, fill=total_mean))+
  geom_tile()+
  scale_fill_viridis() +
  #theme_sleek() +
  theme(axis.title.x=element_blank(), 
        axis.title.y = element_text(color="black", size=30, face="bold"),
        axis.text.x=element_text(color= "black",size=10), 
        axis.text.y=element_text(size =25,color= "black")) +
  theme(legend.position="none")+ #remove legend
  labs(x='', y ='Species_stage')+
  scale_x_discrete(labels=c("onshore_bottom" = "Onshore Bottom", "onshore_surface" = "Onshore Surface", "front_bottom" = "Front Bottom", "front_surface" = "Front Surface", "offshore_bottom" = "Offshore Bottom", "offshore_surface" = "Offshore Surface"))
heatmap
ggsave(filename = "figures/heatmap.pdf", width = 15, height = 7)

#Boxplots
#=======
#Diversity
div <- plankton %>% group_by_at(.vars = c("location", "depth", "date")) %>% tally()#df with number of species_stages per observational unit
boxplot(n~location, div)

abun <- plankton %>% group_by_at(.vars = c("location", "depth", "date")) %>% summarize(plankters = sum(total))
