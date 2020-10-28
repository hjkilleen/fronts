# Mon Oct 19 17:16:13 2020 ------------------------------

#Fronts Plankton Data Analysis

#LIBRARIES
library(tidyverse)
library(viridis)
library(reshape2)
library(vegan)

#LOAD DATA
plankton <- read.csv("data/biology/plankton.csv", colClasses = c("factor", "factor", "factor", "factor", "factor", "integer", "factor", "integer", "integer", "factor"))

#TIDY DATA
#harpactacoids>calanoids based on identification QC
plankton$species[which(plankton$species=="Harpactacoid")] = "Calanoid"
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
#create vector with only species_stages with >2 observation
common <- filter(plankton %>% group_by(species_stage) %>% tally(), n>2)[,1]
common_plankton <- filter(plankton, species_stage %in% common$species_stage)

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
t <- df %>% group_by(species_stage) %>% tally()
df <- as.data.frame(left_join(df, t))
df <- df %>% mutate(species_stage = fct_reorder(species_stage, n, .fun='median' ))

#add vertical lines to define locations
my.lines<-data.frame(x=c(2.5,4.5), y=c(0,0), 
                     xend=c(2.5,4.5), yend=c(22.5,22.5))

heatmap <- ggplot(df, aes(x=position, y =species_stage, fill=total_mean))+
  geom_tile()+
  scale_fill_viridis() +
  theme_bw() +
  theme(axis.title.x=element_blank(), 
        axis.title.y = element_text(color="black", size=55, face="bold"),
        axis.text.x=element_text(color= "black",size=25), 
        axis.text.y=element_text(size =35,color= "black")) +
  theme(legend.position="none")+ #remove legend
  labs(x='', y ='Species_stage')+
  scale_x_discrete(labels=c("onshore_bottom" = "Onshore Bottom", "onshore_surface" = "Onshore Surface", "front_bottom" = "Front Bottom", "front_surface" = "Front Surface", "offshore_bottom" = "Offshore Bottom", "offshore_surface" = "Offshore Surface")) +
  geom_segment(data=my.lines, aes(x,y,xend=xend, yend=yend), size=3, inherit.aes=F)
heatmap
ggsave(filename = "figures/heatmap.pdf", width = 25, height = 20)

#try the same analysis for all taxa
plankton$position <- paste(plankton$location, plankton$depth, sep = "_")#create position variable 
df <- plankton %>% 
  group_by_at(.vars = c("species_stage", "position")) %>% 
  summarize(total = mean(total))

s <- as.data.frame(df) %>% 
  group_by_at(.vars = "species_stage") %>% 
  summarize(sum = sum(total))#get total mean observation values

df <- left_join(df, s)
df$total_mean <- df$total/df$sum#total_mean value is a proportion of all observations of each species_stage
df$position <- factor(df$position, levels = c("onshore_bottom", "onshore_surface", "front_bottom", "front_surface", "offshore_bottom", "offshore_surface"))
t <- df %>% group_by(species_stage) %>% tally()
df <- as.data.frame(left_join(df, t))

df %>%
  mutate(species_stage = fct_reorder(species_stage, n, .fun='median' )) %>%#reorder species_stage by number of observations
ggplot(aes(x=position, y =species_stage, fill=total_mean))+
  geom_tile()+
  scale_fill_viridis() +
  #theme_sleek() +
  theme(axis.title.x=element_blank(), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x=element_text(color= "black",size=10), 
        axis.text.y=element_text(size =10,color= "black")) +
  theme(legend.position="none")+ #remove legend
  labs(x='', y ='Species_stage')+
  scale_x_discrete(labels=c("onshore_bottom" = "Onshore Bottom", "onshore_surface" = "Onshore Surface", "front_bottom" = "Front Bottom", "front_surface" = "Front Surface", "offshore_bottom" = "Offshore Bottom", "offshore_surface" = "Offshore Surface")) + 
  ggsave(filename = "figures/heatmapAll.pdf")

#Daily heatmaps
df <- plankton %>% 
  group_by_at(.vars = c("species_stage", "position", "date")) %>% 
  summarize(total = mean(total))

s <- as.data.frame(df) %>% 
  group_by_at(.vars = c("species_stage", "date")) %>% 
  summarize(sum = sum(total))#get total mean observation values

df <- left_join(df, s)
df$total_mean <- df$total/df$sum#total_mean value is a proportion of all observations of each species_stage
df$position <- factor(df$position, levels = c("onshore_bottom", "onshore_surface", "front_bottom", "front_surface", "offshore_bottom", "offshore_surface"))
t <- df %>% group_by_at(.vars = c("species_stage", "date")) %>% tally()
df <- as.data.frame(left_join(df, t))
d <- levels(plankton$date)
for(i in seq(1:length(d))){
  df %>% 
    filter(date == d[i]) %>% 
    mutate(species_stage = fct_reorder(species_stage, n, .fun='median' )) %>%#reorder species_stage by number of observations
    ggplot(aes(x=position, y =species_stage, fill=total_mean))+
    geom_tile()+
    scale_fill_viridis() +
    ggtitle(d[i]) +
    #theme_sleek() +
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_text(color="black", size=10),
          axis.text.x=element_text(color= "black",size=10), 
          axis.text.y=element_text(size =10,color= "black")) +
    theme(legend.position="none")+ #remove legend
    labs(x='', y ='Species_stage')+
    scale_x_discrete(labels=c("onshore_bottom" = "Onshore Bottom", "onshore_surface" = "Onshore Surface", "front_bottom" = "Front Bottom", "front_surface" = "Front Surface", "offshore_bottom" = "Offshore Bottom", "offshore_surface" = "Offshore Surface")) + 
  ggsave(filename = paste(paste("figures/dailyHeatmaps/", i, sep = ""), ".pdf", sep = ""))
}


#Boxplots
#=======
plankton$location <- factor(plankton$location, levels = c("onshore", "front", "offshore"))
plankton$depth <- factor(plankton$depth, levels = c("surface", "bottom"))

#Diversity
div <- plankton %>% group_by_at(.vars = c("location", "depth", "date")) %>% tally()#df with number of species_stages per observational unit
boxplot(n~location, div, ylab = "Diversity (n taxa)", xlab = "", main = "Plankton Diversity Across Front", col = "orange")

abun <- plankton %>% group_by_at(.vars = c("location", "depth", "date")) %>% summarize(plankters = sum(total))
boxplot(plankters~location, abun, ylab = "Abundance (n ind.)/5 min.tow", xlab = "", main = "Plankton Abundance Across Front", col = "blue", notch = FALSE)

abun <- plankton %>% filter(species != "Calanoid") %>% group_by_at(.vars = c("location", "depth", "date")) %>% summarize(plankters = sum(total))
boxplot(plankters~location, abun)

#Daily barplots
d <- levels(plankton$date)
for(i in seq(1:length(d))){
  a <- filter(plankton, date == d[i]) %>% group_by_at(.vars = c("location", "depth")) %>% tally() %>% 
    ggplot(aes(x = location, y = n, fill = depth)) + 
    geom_bar(stat = "identity") + 
    labs(x = "Location", y = "Diversity (n taxa)", title = paste(d[i], "Diversity", sep = " ")) + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_text(color="black", size=15),
          axis.text.x=element_text(color= "black",size=15), 
          axis.text.y=element_text(size =15,color= "black")) +
    theme(legend.position = "none")
  b <- filter(plankton, date == d[i]) %>% group_by_at(.vars = c("location", "depth")) %>% summarize(plankters = sum(total)) %>% 
    ggplot(aes(x = location, y = plankters, fill = depth)) + 
    geom_bar(stat = "identity") + 
    labs(x = "Location", y = "Abundance (n ind.)", title = paste(d[i], "Abundance", sep = " ")) +
  theme(axis.title.x=element_blank(), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x=element_text(color= "black",size=15), 
        axis.text.y=element_text(size =15,color= "black"))
  x <- gridExtra::grid.arrange(a, b, ncol = 2)
  ggsave(x, filename = paste("figures/dailyBoxplots/", i, ".pdf", sep = ""), width = 10, height = 7)
}
