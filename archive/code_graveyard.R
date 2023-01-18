#Script to analyze plankton community data

# Fri Dec 17 08:23:38 2021 ------------------------------

library(tidyverse)
library(taxize)
library(gridExtra)
library(viridis)
library(reshape2)
library(glmmTMB)
library(vegan)
library(lubridate)
library(broom)
library(cowplot)
library(pscl)
library(performance)
library(lme4)
library(emmeans)
library("bbmle") ## for AICtab
library(multcomp)
load("data/biology/counts/plankton.rda")
load("data/metadata.rda")

#SETUP
#====
plankton <- filter(plankton, !is.na(location), depth != "neuston")
plankton$location <- factor(plankton$location, levels = c("onshore", "front", "offshore"))#set depth bin order
plankton$depth <- factor(plankton$depth, levels = c("surface", "bottom"))
x <- tally(group_by(plankton, spStage))
plankton <- left_join(plankton, x)
us <- filter(plankton, n>20)

plankton <- filter(plankton, date != as.POSIXct("2019-07-18"))#get rid of 7/18 (incomplete cruise)
us <- filter(us, date != as.POSIXct("2019-07-18"))
y <- dcast(plankton, sample~spStage, value.var = "sampleCount", fun.aggregate = mean)#create wide df
y.us <- dcast(us, sample~spStage, value.var = "sampleCount", fun.aggregate = mean)#create wide df
y[is.na(y)] <- 0#Nas to zeros (cruises without observations as well as real zeros)
y.us[is.na(y.us)] <- 0

y <- left_join(y, distinct(dplyr::select(plankton, sample, location, depth, date, volume_total)))#add date
y.us <- left_join(y.us, distinct(dplyr::select(us, sample, location, depth, date, volume_total)))
#====

#SPECIES SPECIFIC MODELS
#====
#Balanus crenatus cyprids
mods.balanus.crenatus_cyprid <- list()#set up
x <- dplyr::select(y.us, date, location, depth, balanus.crenatus_cyprid, volume_total)

ggplot(x) + #search for outliers
  geom_boxplot(aes(x = location, y = balanus.crenatus_cyprid, color = depth))

ggplot(x) + 
  geom_histogram(aes(balanus.crenatus_cyprid))

m0 <- glmmTMB(balanus.crenatus_cyprid~location+depth:location+offset(log(volume_total)), data = y.us, ziformula = ~1, family = truncated_poisson)
m1 <- update(m0, family = truncated_nbinom1)
m2 <- update(m1, family = truncated_nbinom2)
m3 <- update(m2, ~. + (1|date))

AICtab(m0, m1, m2, m3)


summary(m3)#Simple effects and model diagnostics
car::Anova(m3, test = "Chisq")#main effects

x$fit <- exp(fitted(m3))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = balanus.crenatus_cyprid, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)

emmeans(m3, pairwise~location:depth)

#=====

#1 - Model the impact of front and the interaction with depth on the concentration of plankton
#====
y.us.all <- melt(y.us, id = c("location", "depth", "date", "volume_total", "sample"))
y.us.all$date <- as.factor(y.us.all$date)

fit_all <- glmmTMB(value~location+depth:location+offset(log(volume_total))+(1|date)+(1|variable), data = y.us.all, ziformula = ~1, family = truncated_nbinom2)
fit_all1 <- update(fit_all, family = truncated_nbinom1)
fit_all2 <- update(fit_all, family = truncated_poisson)

AICtab(fit_all, fit_all1, fit_all2)#nbinom2 is the best fit

summary(fit_all)#location matters
car::Anova(fit_all)
performance::r2(fit_all)#condition and marginal R2 show that the fixed effects have little biological relevance compared to the date and sppStage
#====

#4 - Model Diversity
#====
#Calculate Shannon-Weiner Index per sample
y$diversity <- diversity(y[,2:113], index = "shannon")#calculate Shannon-Weiner diversity of each tow
#Calculate species evenness per sample
y$evenness <- y$diversity/log(specnumber(y[,2:113]))

#Plot
ggplot(y) + 
  geom_boxplot(aes(location, evenness, color = depth)) +
  labs(x = "Location", y = "Shannon Index") +
  theme_classic() +
  theme(text = element_text(size = 20))

#ANOVA
fit_div <- lmer(diversity~location+(1|date), data = y)
summary(fit_div)
#====



#Add alongshore wind stress to daily wind
boonWind <- left_join(wsDay, wdDay)
#convert knots to m/s
boonWind$spe <- boonWind$spe*0.44704
#calculate alongshore wind speed
boonWind <- shift.wind(boonWind, coastline.angle = 320, dir = "deg", spe = "spe", time = "datetime")
#calculate alongshore wind stress; drag coeff=.0014, air density=1.3
boonWind$asws <- abs(boonWind$aspe)*(boonWind$aspe)*(.0014)*(1.3)
#filter to only sampling period
boonWind <- filter(boonWind, datetime>as.POSIXct("2019-07-01"), datetime<as.POSIXct("2020-10-31"))
#save .rda file
save(boonWind, file = "data/environment/46013wind/boonWind.rda")


results <- "figures/hfr/"
for(i in 1:7){
  p <- ggplot(filter(boonHFR, datetime_pdt > as.POSIXct(as.numeric(cruiseDates[i])-86400, origin = "1970-01-01"), datetime_pdt < as.POSIXct(as.numeric(cruiseDates[i])+86400, origin = "1970-01-01"), id %in% c("b1u", "b2u", "b6u"))) + 
    geom_point(aes(x = datetime_pdt, y = speed, color = id)) + 
    ggtitle("Eastward BOON Currents", subtitle = print(cruiseDates[i]))
  ggsave(paste(results,paste(cruiseDates[i]),"_u",".jpg", sep = ""))
}

for(i in 1:7){
  p <- ggplot(filter(boonHFR.df, datetime_pdt > as.POSIXct(as.numeric(cruiseDates[i])-86400, origin = "1970-01-01"), datetime_pdt < as.POSIXct(as.numeric(cruiseDates[i])+86400, origin = "1970-01-01"), id %in% c("b1u", "b2u", "b6u"))) + 
    geom_point(aes(x = datetime_pdt, y = speed, color = id)) + 
    ggtitle("Eastward BOON Currents", subtitle = print(cruiseDates[i]))
  ggsave(paste(results,paste(cruiseDates[i]),"_v",".jpg", sep = ""))
}

boonHFR.df$id <- as.factor(boonHFR.df$id)
levels(boonHFR.df$id) <- c("b1v", "b1u", "b2v", "b2u", "b6v", "b6u", "b7v", "b7u")

#Plankton 
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
vcommon <- filter(plankton %>% group_by(species_stage) %>% tally(), n>3)[,1]
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
        axis.title.y = element_text(color="black", size=110, face="bold"),
        axis.text.x=element_text(color= "black",size=56), 
        axis.text.y=element_text(size =70,color= "black")) +
  theme(legend.position="none")+ #remove legend
  labs(x='', y ='Species_stage')+
  scale_x_discrete(labels=c("onshore_bottom" = "Onshore\nBottom", "onshore_surface" = "Onshore\nSurface", "front_bottom" = "Front\nBottom", "front_surface" = "Front\nSurface", "offshore_bottom" = "Offshore\nBottom", "offshore_surface" = "Offshore\nSurface")) +
  geom_segment(data=my.lines, aes(x,y,xend=xend, yend=yend), size=3, inherit.aes=F)
heatmap
ggsave(filename = "figures/heatmap.jpg", width = 38, height = 30)

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
jpeg(filename = "figures/boxplotDiv.jpeg", width = 800, height = 500)
par(cex.lab = 4)
par(cex.axis = 2.8)
par(cex.main = 3.5)
par(mar=c(3,8,4,3))
par(mgp = c(5,2,0))
boxplot(n~location, div, ylab = "Diversity (n taxa)", xlab = "", main = "Plankton Diversity Across Front", col = "orange")
dev.off()

abun <- plankton %>% group_by_at(.vars = c("location", "depth", "date")) %>% summarize(plankters = sum(total))
jpeg(filename = "figures/boxplotAbun.jpeg", width = 800, height = 500)
par(cex.lab = 3)
par(cex.axis = 2.8)
par(cex.main = 3.5)
par(mar=c(4,10.5,4,3.5))
par(mgp = c(5,2,0))
boxplot(plankters~location, abun, ylab = "Abundance\n(n ind.)/5 min.tow", xlab = "", main = "Plankton Abundance Across Front", col = "blue", notch = FALSE)
dev.off()

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
          axis.title.y = element_text(color="black", size=30),
          axis.text.x=element_text(color= "black",size=25), 
          axis.text.y=element_text(size =20,color= "black"),
          plot.title = element_text(color = "black", size = 33, face = "bold"))+
    theme(legend.position = "none")
  b <- filter(plankton, date == d[i]) %>% group_by_at(.vars = c("location", "depth")) %>% summarize(plankters = sum(total)) %>% 
    ggplot(aes(x = location, y = plankters, fill = depth)) + 
    geom_bar(stat = "identity") + 
    labs(x = "Location", y = "Abundance (n ind.)", title = paste(d[i], "Abundance", sep = " ")) +
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_text(color="black", size=30),
          axis.text.x=element_text(color= "black",size=25), 
          axis.text.y=element_text(size =20,color= "black"),
          plot.title = element_text(color = "black", size = 33, face = "bold")) +
    theme(legend.title = element_text(color = "black", size = 18),
          legend.text = element_text(color = "black", size = 15))+
    theme(legend.position = "none")
  x <- gridExtra::grid.arrange(a, b, ncol = 2)
  ggsave(x, filename = paste("figures/dailyBoxplots/", i, ".pdf", sep = ""), width = 14, height = 9)
}


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
}
#====



#GLM STUFF



#GLM 1 Using proportion rather than count
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


#Facet plot of rare species

rare <- filter(plankton, !spStage %in% taxa.us, n > 2)
y.rare <- dcast(rare, sample~spStage, value.var = "total", fun.aggregate = mean)#create wide df
y.rare[is.na(y.rare)] <- 0#Nas to zeros (cruises without observations as well as real zeros)


ggplot(data = rare, aes(x = location, y = total, color = depth)) +
  geom_boxplot() +
  facet_wrap(~spStage, scales = "free") +
  labs(x = "Location", y = "Log(Total Count)") +
  guides(color = guide_legend(title = "Depth")) +
  theme_classic() +
  theme(axis.title = element_text(size = 20), legend.text = element_text(size = 15), legend.title = element_text(size = 20))

#Type Model Selection
#Set up
y.us <- dcast(us, sample~spStage, value.var = "sampleCount", fun.aggregate = mean)#create wide df
y.us[is.na(y.us)] <- 0
y.us <- left_join(y.us, distinct(dplyr::select(us, sample, location, depth, date, volume_total)))




#GLM 2 Using a mix of glm packages
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

x$fit <- exp(fitted(m3))#get fitted values
ggplot(x) + #plot fitted values over raw data
  geom_boxplot(aes(x = location, y = balanus.crenatus_cyprid, color = depth)) +
  geom_boxplot(aes(x = location, y = fit, color = depth), width = 0.1)