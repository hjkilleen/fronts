#Script to plot NMDS analysis

# Tue Jan 17 14:59:07 2023 ------------------------------

#LIBRARIES & SOURCES
#====
library(vegan)

source("scripts/7_analysis_plankton_ordination.R")
#====

#SETUP
#====
coldat <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
coldist <- c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#====

#PLOTTING
#====
pdf(file = "figures/nmds.pdf", width = 12, height = 10)#open figure file

layout(mat = matrix(c(1, 2, 3, 4), #setup print area
nrow = 2,
ncol = 2),
heights = c(10, 10),    # Heights of the two rows
widths = c(12, 5))     # Widths of the two columns

par(mar=c(8, 8, 2, 0))#set plot margins

#NMDS by date
plot(p.NMDS, ylab = "", xlab = "", cex = 2, cex.lab = 3, cex.axis = 2)#plot with colors by date
title(ylab="NMDS 2", line=4, cex.lab=3)
with(p.env, points(p.NMDS, display = "sites", col = coldat[date], pch = 19, cex = 2))
ordiellipse(p.NMDS, groups = p.env$date, col = coldat, conf = 0.95, kind = "se", draw = "polygon")
points(cent1 [,-1],
pch = c( 8 , 8, 8, 8, 8, 8),
col = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
bg = c("black"),
lwd = 3.0,
cex = 2.0 # Plots centroids as points on ordination
)

#NMDS by location
plot(p.NMDS, ylab = "", xlab = "", cex = 2, cex.lab = 3, cex.axis = 2)#plot with colors by location
title(ylab = "NMDS 2", xlab = "NMDS 1", line = 4, cex.lab = 3)
with(p.env, points(p.NMDS, display = "sites", col = coldist[location], pch = 19, cex = 2))
ordiellipse(p.NMDS, groups = p.env$location, col = coldist, draw = "polygon")
points(cent2 [,-1],
pch = c( 8 , 8, 8, 8, 8, 8),
col = c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
bg = c("black"),
lwd = 3.0,
cex = 2.0 # Plots centroids as points on ordination
)

#Legends
par(mar=c(2, 2, 4, 3))#set legend margins

#Date
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend(x = 0, y = 1, legend = c("15 August 2019", "20 September 2019", "23 June 2020", "30 June 2020", "30 September 2020", "6 October 2020"), bty = 'n', fill = coldat, title = "Date", title.cex = 2, title.adj = 0, cex = 1.5)

#Location
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend(x = 0, y = 1, legend = c("Onshore", "Front", "Offshore"), bty = 'n', fill = coldist, title = "Location", title.cex = 2, title.adj = 0, cex = 1.5)

dev.off()#close plot
#====