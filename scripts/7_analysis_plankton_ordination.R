#Script to analyze plankton community data

# Thu Jan 12 16:30:53 2023 ------------------------------

#LIBRARIES & SOURCES
#====
library(reshape2)
library(vegan)

load("data/biology/counts/plankton.rda")
#====

#COMMUNITY ANALYSIS OF SIMILARITY
#====
#select grouping metadata
p <- dplyr::select(filter(plankton, n>2), location, date, depth, sample, spStage, total)#omit very rare species
#cast dataframe to site by species format
p.site.sp <- dcast(p, date+location+depth~spStage, value.var="total", fun.aggregate = mean)
p.site.sp[is.na(p.site.sp)] <- 0

#Create dissimilarity matrix
p.comm <- p.site.sp[,4:ncol(p.site.sp)]
p.env <- p.site.sp[,1:3]
p.dist <- vegdist(p.comm)
attach(p.env)

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

#Plot NMDS
#Setup colors and point characteristics for plotting
coldep <- c("red", "blue")
coldist <- c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pchdat <- c(15, 17, 18, 19, 21, 22)
coldat <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pchloc <- c(15, 18, 22)

layout(mat = matrix(c(1, 2, 3, 4), #setup print area
                    nrow = 2, 
                    ncol = 2),
       heights = c(10, 10),    # Heights of the two rows
       widths = c(10, 5))     # Widths of the two columns

par(mar=c(3, 3, 0, 0))#set plot margins

plot(p.NMDS)#plot with colors by date
with(p.env, points(p.NMDS, display = "sites", col = coldat[date], pch = 19, cex = 2))
ordiellipse(p.NMDS, groups = p.env$date, col = coldat, conf = 0.95, kind = "se", draw = "polygon")
points(cent1 [,-1],
       pch = c( 8 , 8, 8, 8, 8, 8),
       col = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
       bg = c("black"),
       lwd = 3.0,
       cex = 2.0 # Plots centroids as points on ordination
)

plot(p.NMDS)#plot with colors by location
with(p.env, points(p.NMDS, display = "sites", col = coldist[location], pch = 19, cex = 2))
ordiellipse(p.NMDS, groups = p.env$location, col = coldist, draw = "polygon")
points(cent2 [,-1],
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
saveRDS(summary(p.NMDS$species), file = "output/nmds_spp_scores_summary.rda")
saveRDS(p.NMDS$species, file = "output/nmds_spp_scores.rda")
#====