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
p <- dplyr::select(filter(plankton, n>2), location, date, depth, sample, spStage, total)
p <- dplyr::filter(p, depth != "neuston", date != as.POSIXct("2019-07-18"))#remove
x <- tally(group_by(plankton, spStage))
plankton <- left_join(plankton, x)
us <- filter(plankton, n>20)

#Boxplot of diversity
div <- plankton %>% group_by_at(.vars = c("location", "depth", "date")) %>% tally()#df with number of species_stages per observational unit

ggplot(div) + 
  geom_boxplot(aes(location, n)) + 
  labs(x = "Location", y = "Richness") + 
  theme(text = element_text(size = 15), axis.text = element_text(size = 15))
anova(lm(n~location, div))
#====

#SHANNON-WEINER INDEX
#====
#Set Up

p.site.sp <- dcast(p, sample~spStage, value.var="total", fun.aggregate = mean)
p.site.sp[is.na(p.site.sp)] <- 0
tows <- dplyr::select(us, sample, date, site, location, depth)
tows <- distinct(tows)

#Calculate Shannon-Weiner Index
p.site.sp$diversity <- diversity(p.site.sp[,-1], index = "shannon")#calculate Shannon-Weiner diversity of each tow

#Attach to metadata
p.site.sp <- left_join(dplyr::select(p.site.sp, sample, diversity), tows)

#Plot
ggplot(p.site.sp) + 
  geom_boxplot(aes(location, diversity, color = depth)) +
  labs(x = "Location", y = "Shannon Index") +
  theme_classic() +
  theme(text = element_text(size = 20))

#ANOVA
anova.loc.div <- aov(diversity~location, data = p.site.sp)
summary(anova.loc.div)
#====


#ANALYSIS OF SIMILARITY & CLUSTERING
#==============
#ALL SPECIES - species and stage

#select grouping metadata
p <- dplyr::select(filter(plankton, n>2), location, date, depth, sample, spStage, total)
p <- dplyr::filter(p, depth != "neuston", date != as.POSIXct("2019-07-18"))#remove neuston
#cast dataframe to site by species format
p.site.sp <- dcast(p, date+location+depth~spStage, value.var="total", fun.aggregate = mean)
p.site.sp[is.na(p.site.sp)] <- 0

#Create dissimilarity matrix
#change second value if you have added more species
p.comm <- p.site.sp[,4:65]
p.env <- p.site.sp[,1:3]
p.dist <- vegdist(p.comm)
attach(p.env)

#Similarity across date
p.ano.st <- anosim(p.dist, date)
summary(p.ano.st)
plot(p.ano.st)

#Similarity across location
p.ano.st <- anosim(p.dist, location)
summary(p.ano.st)
plot(p.ano.st)

#Similarity across depths
p.ano.st <- anosim(p.dist, depth)
summary(p.ano.st)
plot(p.ano.st)

#NMDS
levels(p.env$location) <- c("front", "onshore", "offshore")
coldep <- c("red", "blue")
coldist <- c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pchdat <- c(15, 17, 18, 19, 21, 22)
coldat <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pchloc <- c(15, 18, 22)
p.env$date <- as.factor(p.env$date)

p.NMDS <- metaMDS(p.dist, k=2)

scrs <-#calculate centroids
  scores(p.NMDS, display = "sites", "species")
cent1 <-
  aggregate(scrs ~ date, data = p.comm, FUN = "mean")
names(cent1) [-1] <- colnames(scrs)

cent2 <-
  aggregate(scrs ~ location, data = p.comm, FUN = "mean")
names(cent2) [-1] <- colnames(scrs)

#PLOT
layout(mat = matrix(c(1, 2, 3, 4), 
                    nrow = 2, 
                    ncol = 2),
       heights = c(10, 10),    # Heights of the two rows
       widths = c(10, 5))     # Widths of the two columns


par(mar=c(3, 3, 0, 0))

plot(p.NMDS)
with(p.env, points(p.NMDS, display = "sites", col = coldat[date], pch = 19, cex = 2))
ordiellipse(p.NMDS, groups = p.env$date, col = coldat, conf = 0.95, kind = "se", draw = "polygon")
points(cent1 [,-1],
       pch = c( 8 , 8, 8, 8, 8, 8),
       col = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
       bg = c("black"),
       lwd = 3.0,
       cex = 2.0 # Plots centroids as points on ordination
)

plot(p.NMDS)
with(p.env, points(p.NMDS, display = "sites", col = coldist[location], pch = 19, cex = 2))
ordiellipse(p.NMDS, groups = p.env$location, col = coldist, draw = "polygon")
points(cent [,-1],
       pch = c( 8 , 8, 8, 8, 8, 8),
       col = c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
       bg = c("black"),
       lwd = 3.0,
       cex = 2.0 # Plots centroids as points on ordination
)

plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend(x = 0, y = 1, legend = c("15 August 2019", "20 September 2019", "23 June 2020", "30 June 2020", "30 September 2020", "6 October 2020"), bty = 'n', fill = coldat, title = "Date")

plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend(x = 0, y = 1, legend = c("onshore", "front", "offshore"), bty = 'n', fill = coldist, title = "Location")

#Species Scores
sppscores(p.NMDS) <- p.comm
summary(p.NMDS$species)
View(p.NMDS$species)



#Hierarchical agglomerative clustering using complete linkage for nearshore species/stages
p.sp.site <- dcast(p, spStage~sample, value.var="total", fun.aggregate = mean)
p.sp.site[is.na(p.sp.site)] <- 0
p.env <- p.sp.site[,2:37]
p.comm <- p.sp.site[,1]
p.dist <- vegdist(p.env)
#perform clustering
p.clust <- hclust(p.dist, method = "average")
#change cluster labels for plotting
p.clust$labels <- as.factor(p.comm[p.clust$order])
#plot dendrogram
plot(p.clust)

View(p.NMDS$points)
View(p.NMDS$species)#quartiles for NMDS1
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

#GLM 1
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

#GLM 2
#====
plankton <- filter(plankton, date != as.POSIXct("2019-07-18"))#get rid of 7/18 (incomplete cruise)
us <- filter(us, date != as.POSIXct("2019-07-18"))
y <- dcast(plankton, sample~spStage, value.var = "sampleCount", fun.aggregate = mean)#create wide df
y.us <- dcast(us, sample~spStage, value.var = "sampleCount", fun.aggregate = mean)#create wide df
y[is.na(y)] <- 0#Nas to zeros (cruises without observations as well as real zeros)
y.us[is.na(y.us)] <- 0

y <- left_join(y, distinct(dplyr::select(plankton, sample, location, depth, date, volume_total)))#add date
y.us <- left_join(y.us, distinct(dplyr::select(us, sample, location, depth, date, volume_total)))

taxa <- names(y)[2:124]#get list of all spStages
taxa.us <- names(y.us)[2:18]

m1 <- MASS::glmmPQL(calanoid_adult~location+depth:location, random = ~ 1|date, family = "negbinomial", data = filter(select(y, c("location", "depth", "date", "calanoid_adult")), calanoid_adult<400000))
m2 <- MASS::glm.nb(gastropod_veliger~location+depth:location, data = select(y, location, depth, date, gastropod_veliger))

m3 <- glmer.nb(gastropod_veliger~location+depth:location+(1|date), data = filter(select(y, c("location", "depth", "date", "gastropod_veliger"))))
ss <- getME(m3, c("theta", "fixef"))
m4 <- update(m3, start = ss, control = glmerControl(optCtrl = list(maxfun = 2e4)))
m5 <- glmm.zinb(gastropod_veliger~location+location:depth, random = ~ 1 | date, data = dplyr::select(y, location, depth, date, gastropod_veliger), zi_fixed = ~1)
summary(m5)
pscl::odTest(m2)
check_zeroinflation(m2)
car::Anova(m5)

tt <- getME(m3,"theta")
ll <- getME(m3,"lower")
min(tt[ll==0])
#====

ggplot(data = y) +
  geom_boxplot(aes(x = location, y = gastropod_veliger))
