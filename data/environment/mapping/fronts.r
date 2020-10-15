# fronts.r
# ===================================================================================================
# Setup
# ===================================================================================================
require(maptools) # readShapeLines ft to read shape file of shoreline
require(rgeos) # gDistance function to calc min dist to shore and readWKT (takes strings, converts to geospatial points)
require(rgdal) # needed for rgeos "spTransform" method, which transforms datum projections
require(geosphere) # for bearing() function and other useful geographic functions
require(raster)
require(dplyr)
require(ggplot2)
	# Map data
		load(file  = "/Users/Connor/Documents/Graduate School/Dibble_Research/FLAH_2016/Analysis/flah_map_data.R")

# ===================================================================================================
# Read Data
# ===================================================================================================
fr <- read.csv(file = "/Users/connor/Documents/Graduate School/Dibble_Research/Fronts_MusselPoint/Data/Field_081519/20190815_TT_001_bin.asc")

fr$Density <- oce::swSigma(salinity = fr$Sal00, temperature = fr$Tv290C, pressure = 0) + 1000

ggplot(fr) + geom_point(aes(x = Longitude, y = Latitude, fill = Tv290C), size = 2, shape = 21) + coord_equal() + theme_bw() +
	geom_point(data = coastline.df %>% dplyr::filter(long < max(fr$Longitude) + 0.0075, long > min(fr$Longitude), lat > min(fr$Latitude) - 0.25, lat < max(fr$Latitude)),
		aes(x = long, y = lat), size = 0.1) + scale_fill_gradient2(midpoint = mean(fr$Tv290C)) +
	geom_segment(data = data.frame( y = 38.32491, x = -123.08141, yend = 38.32513, xend = -123.08129),
		aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 6, inherit.aes = FALSE)


ggplot(fr) + geom_point(aes(x = Longitude, y = Latitude, fill = Sal00), size = 2, shape = 21) + coord_equal() + theme_bw() +
	geom_point(data = coastline.df %>% dplyr::filter(long < max(fr$Longitude) + 0.0075, long > min(fr$Longitude), lat > min(fr$Latitude) - 0.25, lat < max(fr$Latitude)),
		aes(x = long, y = lat), size = 0.1) + scale_fill_gradient2(midpoint = mean(fr$Sal00)) +
	geom_segment(data = data.frame( y = 38.32491, x = -123.08141, yend = 38.32513, xend = -123.08129),
		aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 6, inherit.aes = FALSE)


ggplot(fr) + geom_point(aes(x = Longitude, y = Latitude, fill = Density), size = 2, shape = 21) + coord_equal() + theme_bw() +
	geom_point(data = dplyr::filter(fr, TimeJ == min(TimeJ) | TimeJ == max(TimeJ)), aes(x = Longitude, y = Latitude), fill = "white", shape = 21, size = 3, inherit.aes = FALSE) + 
	geom_point(data = coastline.df %>% dplyr::filter(long < max(fr$Longitude) + 0.0075, long > min(fr$Longitude), lat > min(fr$Latitude) - 0.25, lat < max(fr$Latitude)),
		aes(x = long, y = lat), size = 0.1) + scale_fill_gradient2(midpoint = mean(fr$Density)) +
	geom_segment(data = data.frame( y = 38.32491, x = -123.08141, yend = 38.32513, xend = -123.08129),
		aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 6, inherit.aes = FALSE) +
	theme(legend.position = c(0.1, 0.3))

ggplot(fr) + geom_point(aes(x = Longitude, y = Latitude, fill = TimeJ, size = Density), size = 2, shape = 21) + coord_equal() + theme_bw() +
	geom_point(data = dplyr::filter(fr, TimeJ == min(TimeJ) | TimeJ == max(TimeJ)), aes(x = Longitude, y = Latitude), fill = "white", shape = 21, size = 3, inherit.aes = FALSE) + 
	geom_point(data = coastline.df %>% dplyr::filter(long < max(fr$Longitude) + 0.0075, long > min(fr$Longitude), lat > min(fr$Latitude) - 0.25, lat < max(fr$Latitude)),
		aes(x = long, y = lat), size = 0.1) +
	geom_segment(data = data.frame( y = 38.32491, x = -123.08141, yend = 38.32513, xend = -123.08129),
		aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 6, inherit.aes = FALSE) +
	theme(legend.position = c(0.1, 0.3))