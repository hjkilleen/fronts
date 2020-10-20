library(ggplot2)
#Plot Underway Map
map.underway <- function(x){
  ggplot(ldf[[x]], aes(x = longitude, y = latitude, color = density))+
    geom_point(size = 3)+
    scale_color_gradient2(low = scales::muted("blue"), mid = "white", high = scales::muted("red"), midpoint = mean(ldf[[x]]$density)) +
    labs(x = "Longitude", y = "Latitude", title = paste(ldf[[x]]$date[1], "Surface Density", sep = " ")) 
}
