#Script to analyze plankton community data

# Fri Dec 17 08:23:38 2021 ------------------------------

library(tidyverse)
library(taxize)
library(viridis)
library(reshape2)
library(vegan)
library(lubridate)

#TOTAL ABUNDANCE
#====
plankton <- mutate(plankton, name_stage = paste(species, stage, sep = "."))#create species name with stage modifier
plankton <- filter(plankton, !is.na(location), depth != "neuston")
plankton$location <- factor(plankton$location, levels = c("onshore", "front", "offshore"))#set depth bin order
x <- tally(group_by(plankton, name_stage))
plankton <- left_join(plankton, x)
plankton <- filter(plankton, n>1)#get rid of species/stages that were only observed once

y <- summarize(group_by(plankton, name_stage), n = mean(n))#frequency of occurence
us <- filter(y, n>9)#create list of the usual suspects, i.e. occur in ~50% of samples

#Boxplot of bulk abundance
ggplot(summarize(group_by_at(plankton, vars(date, location, depth)), total = mean(total))) + 
  geom_boxplot(aes(location, total)) + 
  ylim(0, 15000) + 
  labs(x = "Location", y = "Count/5 min tow") + 
  theme(text = element_text(size = 15), axis.text = element_text(size = 15))

#Boxplot of diversity
div <- plankton %>% group_by_at(.vars = c("location", "depth", "date")) %>% tally()#df with number of species_stages per observational unit

ggplot(div) + 
  geom_boxplot(aes(location, n)) + 
  labs(x = "Location", y = "Count/5 min tow") + 
  theme(text = element_text(size = 15), axis.text = element_text(size = 15))
#====


#ANALYSIS OF SIMILARITY & CLUSTERING
#==============
#ALL SPECIES - species and stage

#select grouping metadata
p <- select(plankton, location, depth, sample, name_stage, count)
#cast dataframe to site by species format
p[, 1:4] <- lapply(p[, 1:4], as.factor)
p.site.sp <- dcast(p, location+depth~name_stage, value.var="count", fun.aggregate = mean)
p.site.sp[is.na(p.site.sp)] <- 0

#Create dissimilarity matrix
#change second value if you have added more species
p.comm <- p.site.sp[,3:88]
p.env <- p.site.sp[,1:2]
p.dist <- vegdist(p.comm)
attach(p.env)

#Similarity across location
p.ano.st <- anosim(p.dist, location)
summary(p.ano.st)
plot(p.ano.st)

#Similarity across depths
p.ano.st <- anosim(p.dist, depth)
summary(p.ano.st)
plot(p.ano.st)

#NMDS
coldep <- c("red", "blue")
coldist <- c("bisque3", "green", "darkblue")
p.NMDS <- metaMDS(p.dist, k=2)
plot(p.NMDS)
sppscores(p.NMDS) <- p.comm
with(p.env, points(p.NMDS, display = "sites", col = coldist[location], pch = 19, cex = 2))
orditorp(p.NMDS,display="species",col="red",air=0.01)

#====

#TAXONOMIC SPECIFIC PLOTS
#====
z <- unique(us$name_stage)
results <- "figures/exploratoryPlots/usualSuspects/"
plankton$date <- as.factor(plankton$date)

for (i in z) {#create plots for each species/stage
  filter(plankton, name_stage==i) %>% 
    ggplot(aes(x = location, y = total, fill = depth))+
    geom_col(position = "stack")+
    facet_wrap(~date)+
    ggtitle(print(i))+
    labs(x = "Location", y = "Count/5 min tow")+
    theme(text = element_text(size = 10), axis.text = element_text(size = 10))
  ggsave(paste(results,i,".jpg", sep = ""))
}


#Weirdos
x <- summarize(group_by_at(plankton, vars(name_stage, date)), daily_total_spp_stg = sum(total))
plankton <- left_join(plankton, x)
plankton$rel_abund <- plankton$total/plankton$daily_total_spp_stg
weirdos <- summarize(group_by_at(filter(plankton, n<10), vars(location, depth, name_stage)), relAbun_mean = mean(rel_abund))
weirdos$loc.dep <- paste(weirdos$location, weirdos$depth, sep = ".")

ggplot(summarize(group_by_at(filter(plankton, n<10), vars(date, location, depth)), total = mean(total))) + 
  geom_boxplot(aes(location, total)) + 
  labs(x = "Location", y = "Count/5 min tow") + 
  theme(text = element_text(size = 15), axis.text = element_text(size = 15))

#Barnacles
barnacles <- c("b.crenatus.naup2.3", "b.glandula.naup2.3", "chth.naup2.3", "p.polymerus.naup2.3")
for (i in barnacles) {#create plots for each species/stage
  filter(plankton, name_stage==i) %>% 
    ggplot(aes(x = location, y = total, fill = depth))+
    geom_col(position = "stack")+
    ggtitle(print(i))+
    labs(x = "Location", y = "Count/5 min tow")+
    theme(text = element_text(size = 10), axis.text = element_text(size = 10))
  ggsave(paste(results,i,".jpg", sep = ""))
}

#Heatmap
plankton$loc.dep <- paste(plankton$location, plankton$depth, sep = ".")

#add vertical lines to define locations
my.lines<-data.frame(x=c(2.5,4.5), y=c(0,0), 
                     xend=c(2.5,4.5), yend=c(19,19))

heatmap <- ggplot(filter(plankton, name_stage %in% us$name_stage), aes(x=loc.dep, y =name_stage, fill=rel_abund))+
  geom_tile()+
  scale_fill_viridis() +
  theme_bw() +
  labs(x='', y ='Species_stage')+
  geom_segment(data=my.lines, aes(x,y,xend=xend, yend=yend), size=3, inherit.aes=F)
heatmap
