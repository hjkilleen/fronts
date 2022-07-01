#Script to analyze plankton community data

# Fri Dec 17 08:23:38 2021 ------------------------------

library(tidyverse)
library(taxize)
library(gridExtra)
library(viridis)
library(reshape2)
library(vegan)
library(lubridate)
library(broom)
library(cowplot)
load("data/biology/counts/plankton.rda")
load("data/metadata.rda")

#TOTAL ABUNDANCE
#====
plankton <- filter(plankton, !is.na(location), depth != "neuston")
plankton$location <- factor(plankton$location, levels = c("onshore", "front", "offshore"))#set depth bin order
plankton$depth <- factor(plankton$depth, levels = c("surface", "bottom"))
x <- tally(group_by(plankton, species))
plankton <- left_join(plankton, x)
us <- filter(plankton, n>10)


#Boxplot of bulk abundance
plankton <- left_join(plankton, summarize(group_by(plankton, sample, tot_per_tow = sum(total))))
ggplot(plankton) + 
  geom_boxplot(aes(location, tot_per_tow)) + 
  labs(x = "Location", y = "Count/5 min tow") + 
  theme(text = element_text(size = 15), axis.text = element_text(size = 15))

#Boxplot of diversity
div <- plankton %>% group_by_at(.vars = c("location", "depth", "date")) %>% tally()#df with number of species_stages per observational unit

ggplot(div) + 
  geom_boxplot(aes(location, n)) + 
  labs(x = "Location", y = "Count/5 min tow") + 
  theme(text = element_text(size = 15), axis.text = element_text(size = 15))
#====

#SHANNON-WEINER INDEX
#====
#Set Up
p <- select(plankton, location, date, depth, sample, spStage, total)
p <- filter(p, depth != "neuston", date != as.POSIXct("2019-07-18"))#remove neuston and cruise 1
p.site.sp <- dcast(p, sample~spStage, value.var="total", fun.aggregate = mean)
p.site.sp[is.na(p.site.sp)] <- 0
tows <- select(us, sample, date, site, location, depth)
tows <- distinct(tows)

#Calculate Shannon-Weiner Index
p.site.sp$diversity <- diversity(p.site.sp[,-1], index = "shannon")#calculate Shannon-Weiner diversity of each tow

#Attach to metadata
p.site.sp <- left_join(select(p.site.sp, sample, diversity), tows)

#Plot
boxplot(diversity~location, data = p.site.sp)

#ANOVA
anova.loc.div <- aov(diversity~location, data = p.site.sp)
summary(anova.loc.div)
#====


#ANALYSIS OF SIMILARITY & CLUSTERING
#==============
#ALL SPECIES - species and stage

#select grouping metadata
p <- select(us, location, date, depth, sample, spStage, count)
p <- filter(p, depth != "neuston", date != as.POSIXct("2019-07-18"))#remove neuston
#cast dataframe to site by species format
p.site.sp <- dcast(p, date+location+depth~spStage, value.var="count", fun.aggregate = mean)
p.site.sp[is.na(p.site.sp)] <- 0

#Create dissimilarity matrix
#change second value if you have added more species
p.comm <- p.site.sp[,4:53]
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
coldist <- c("green", "darkblue", "bisque")
p.NMDS <- metaMDS(p.dist, k=2)
plot(p.NMDS)
with(p.env, points(p.NMDS, display = "sites", col = coldist[location], pch = 19, cex = 2))
ordiellipse(p.NMDS, groups = p.env$location, draw = "polygon", col = coldist)
orditorp(p.NMDS,display="sites",col="red",air=0.01)

#====

#TAXONOMIC SPECIFIC PLOTS BY SURVEY
#====
z <- unique(us$date)
results <- "figures/exploratoryPlots/taxaBySurvey/"
plankton$spStage <- as.factor(plankton$spStage)
i<-8
for (i in 1:length(z)) {#create plots for each species/stage
  filter(plankton, date==z[i]) %>% 
    ggplot(aes(x = location, y = total, fill = depth))+
    geom_col(position = "stack")+
    facet_wrap(~spStage, scales = "free")+
    ggtitle(print(z[i]))+
    labs(x = "Location", y = "Count/5 min tow")+
    theme(text = element_text(size = 10), axis.text = element_text(size = 10))
  ggsave(paste(results,i,".jpg", sep = ""))
}


#Weirdos
x <- summarize(group_by_at(plankton, vars(spStage, date)), daily_total_spp_stg = sum(total))
plankton <- left_join(plankton, x)
plankton$rel_abund <- plankton$total/plankton$daily_total_spp_stg
weirdos <- summarize(group_by_at(filter(plankton, n<10), vars(location, depth, spStage)), relAbun_mean = mean(rel_abund))
weirdos$loc.dep <- paste(weirdos$location, weirdos$depth, sep = ".")

ggplot(summarize(group_by_at(filter(plankton, n<10), vars(date, location, depth)), total = mean(total))) + 
  geom_boxplot(aes(location, total)) + 
  labs(x = "Location", y = "Count/5 min tow") + 
  theme(text = element_text(size = 15), axis.text = element_text(size = 15))

#Barnacles
barnacles <- c("b.crenatus.naup2.3", "b.glandula.naup2.3", "chth.naup2.3", "p.polymerus.naup2.3")
for (i in barnacles) {#create plots for each species/stage
  filter(plankton, spStage==i) %>% 
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

heatmap <- ggplot(filter(plankton, spStage %in% us$spStage), aes(x=loc.dep, y =spStage, fill=rel_abund))+
  geom_tile()+
  scale_fill_viridis() +
  theme_bw() +
  labs(x='', y ='Species_stage')+
  geom_segment(data=my.lines, aes(x,y,xend=xend, yend=yend), size=3, inherit.aes=F)
heatmap
#====

#ANOVA SPECIES/STAGE BY LOCATION
#====
x <- unique(us$spStage)
results <- "output/anova_speciesStage/"
y <- dcast(us, sample~spStage, value.var="total", fun.aggregate = mean)
y[is.na(y)] <- 0
for (i in 1:length(x)) {#create plots for each species/stage
  df <- filter(us, spStage==x[i])
  stats <- summary(aov(total~location, df))
  a <- ggplot(df, aes(location, total)) + geom_boxplot()
  b <- ggplot(df, aes(sample, total)) + geom_point()
  c <- ggdraw(a) + draw_label(paste("p = ", stats[[1]][1,5], sep = ""))
  grid.arrange(c, b)
  ggsave(paste(results,x[i],".jpg", sep = ""))
}#====

#GLM
#====
sums <- summarize(group_by_at(us, vars(spStage, date)), sum = sum(total))
us <- left_join(us, sums)#add column for sum total of each spStage per survey
us$prop <- us$total/us$sum#add column for proportion of each spStage per survey EU
y <- dcast(us, sample~spStage, value.var="prop", fun.aggregate = mean)#create wide df
y[is.na(y)] <- 0#Nas to zeros
y <- left_join(y, distinct(select(us, sample, location, depth, date)))#add date
taxa <- names(y)[2:49]#get list of all spStages

for(i in 1:length(taxa)){#replace all missing observations by taxa
df <- select(y, sample, date, as.character(taxa[i]))
names(df) <- c("sample", "date", "taxa")
df.2 <- summarize(group_by(df, date), sum = sum(taxa))#identify cruises where none of this taxa were observed (false zeros)
no.obs <- df.2$date[which(df.2$sum ==0)]
y[,1+i][which(y$date%in%no.obs)] <- "NA"#replace cruises without observations with NA
}
y[,2:49] <- lapply(y[,2:49], as.numeric)#change classes back to numeric
#====
