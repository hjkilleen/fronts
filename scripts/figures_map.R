#Study Area Map

# Sat Jul  9 14:03:19 2022 ------------------------------

#LOAD LIBRARIES
#====
library(marmap)
#====

#SETUP
#====
#make base map
bhsp <- getNOAA.bathy(lon1 = -122.059046, lon2 = -123.880030,
                      lat1 = 37.436805, lat2 = 38.686682, 
                      resolution = 1)

blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")
#====

#PLOT
#====
p1 <- recordPlot()
plot(bhsp, image = TRUE, land = TRUE, lwd = 0.1,
     bpal = list(c(0, max(bhsp), "beige"),
                 c(min(bhsp),0,blues)))
plot(bhsp, deep = 0, shallow = 0, step = 0, lwd = 5, add = TRUE)
scaleBathy(bhsp, deg = .5, x = "bottomleft", inset = 30)
points(-123.317, 38.235, pch = 4, col = "black")
text(-123, 38.7, "Stewarts Point")
text(-122.75, 38.3, expression("Bodega\nHead"))




#GGMAP
library(ggmap)
register_google(key = "AIzaSyA5gYfO8bTgnXZp5e3Hxv994eyQ7vjloaE")

#Big Map
df <- data.frame(lon = -123.079257, lat = 38.323404)
gmap = get_map(location = c(lon = -123.079257, lat = 38.323404), zoom = 8)

ggmap(gmap) + 
  geom_point(data = df, aes(x = lon, lat), color = "red", size = 3) + 
  labs(x = "Longitude", y = "Latitude") +     
  theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent'),
  text = element_text(size = 20)
)

#Small Map
df2 <- data.frame(lon = -123.071690, lat = 38.317757)
gmap2 = get_map(location = c(lon = -123.079257, lat = 38.323404), zoom = 13, maptype = "satellite")

ggmap(gmap2) +
  geom_point(data = df, aes(x = lon, lat), color = "red", size = 3) + 
  geom_point(data = df2, aes(x = lon, lat), color = "yellow", size = 3, shape = 17) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme(text = element_text(size = 20))

#Small Watercolor Map
df <- data.frame(lon = -123.071690, lat = 39.617757)
gmap2 = get_map(location = c(lon = -123.079257, lat = 38.323404), zoom = 13, maptype  = "satellite")

ggmap(gmap2) + labs(x = "Longitude", y = "Latitude") + theme(text = element_text(size = 20))

ggmap(gmap2) + theme_void()
