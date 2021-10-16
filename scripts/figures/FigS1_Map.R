########### Fisher et al. Fig S1 ##############
#
# Map sampling sites against previous studies
#
# MF 6/2/2020
#
###############################################



# Load Packages -----------------------------------------------------------
# library(devtools; devtools::install_github("slowkow/ggrepel"))
library(tidyverse)
library(maps)
library(maptools)
library(ggrepel)
library(viridis)
library(ggplot2)
library(pals)
library(grid)
library(geosphere)
library(here)



# Additional Functions ----------------------------------------------------
# THESE ARE THE FUNCTIONS YOU NEED TO MAKE THE SCALE BAR
#The code in the chunk below was written by Ewen Gallic (bless his heart for being so sharing).

# Result #
#--------#
# Return a list whose elements are :
# 	- rectangle : a data.frame containing the coordinates to draw the first rectangle ;
# 	- rectangle2 : a data.frame containing the coordinates to draw the second rectangle ;
# 	- legend : a data.frame containing the coordinates of the legend texts, and the texts as well.
#
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distance_lon : length of each rectangle ;
# distance_lat : width of each rectangle ;
# distance_legend : distance between rectangles and legend texts ;
# dist_units : units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles).
create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
  # First rectangle
  bottom_right <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon, dist.units = dist_units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_lat, dist.units = dist_units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottom_right[1,"long"], bottom_right[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottom_right2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon*2, dist.units = dist_units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottom_right[1,"long"], bottom_right[1,"long"], bottom_right2[1,"long"], bottom_right2[1,"long"], bottom_right[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_legend, dist.units = dist_units, model = "WGS84")
  on_top2 <- on_top3 <- on_top
  on_top2[1,"long"] <- bottom_right[1,"long"]
  on_top3[1,"long"] <- bottom_right2[1,"long"]
  
  legend <- rbind(on_top, on_top2, on_top3)
  legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}
#We also need a function to obtain the coordinates of the North arrow:

#
# Result #
#--------#
# Result #
#--------#
# Returns a list containing :
#	- res : coordinates to draw an arrow ;
#	- coordinates of the middle of the arrow (where the "N" will be plotted).
#
# Arguments : #
#-------------#
# scale_bar : result of create_scale_bar() ;
# length : desired length of the arrow ;
# distance : distance between legend rectangles and the bottom of the arrow ;
# dist_units : units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles).
create_orientation_arrow <- function(scale_bar, length, distance = 1, dist_units = "km"){
  lon <- scale_bar$rectangle2[1,1]
  lat <- scale_bar$rectangle2[1,2]
  
  # Bottom point of the arrow
  beg_point <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist_units, model = "WGS84")
  lon <- beg_point[1,"long"]
  lat <- beg_point[1,"lat"]
  
  # Let us create the endpoint
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist_units, model = "WGS84")
  
  left_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 225, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  right_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 135, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  res <- rbind(
    cbind(x = lon, y = lat, xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = left_arrow[1,"long"], y = left_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = right_arrow[1,"long"], y = right_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]))
  
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  
  # Coordinates from which "N" will be plotted
  coords_n <- cbind(x = lon, y = (lat + on_top[1,"lat"])/2)
  
  return(list(res = res, coords_n = coords_n))
}
#The last function enables the user to draw the elements:

#
# Result #
#--------#
# This function enables to draw a scale bar on a ggplot object, and optionally an orientation arrow #
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distance_lon : length of each rectangle ;
# distance_lat : width of each rectangle ;
# distance_legend : distance between rectangles and legend texts ;
# dist_units : units of distance "km" (kilometers) (by default), "nm" (nautical miles), "mi" (statute miles) ;
# rec_fill, rec2_fill : filling colour of the rectangles (default to white, and black, resp.);
# rec_colour, rec2_colour : colour of the rectangles (default to black for both);
# legend_colour : legend colour (default to black);
# legend_size : legend size (default to 3);
# orientation : (boolean) if TRUE (default), adds an orientation arrow to the plot ;
# arrow_length : length of the arrow (default to 500 km) ;
# arrow_distance : distance between the scale bar and the bottom of the arrow (default to 300 km) ;
# arrow_north_size : size of the "N" letter (default to 6).
scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6){
  the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, distance_lat = distance_lat, distance_legend = distance_legend, dist_unit = dist_unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour)
  
  # Legend
  scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=""), x = the_scale_bar$legend[,"long"], y = the_scale_bar$legend[,"lat"], size = legend_size, colour = legend_colour)
  
  res <- list(rectangle1, rectangle2, scale_bar_legend)
  
  if(orientation){# Add an arrow pointing North
    coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length, distance = arrow_distance, dist_unit = dist_unit)
    arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"], y = coords_arrow$coords_n[1,"y"], size = arrow_north_size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}






# Mapping Data ------------------------------------------------------------

# Get the world polygon and extract USA and Canada, Korea

S_Korea <- map_data("world") %>% 
  filter(region=="South Korea")

N_Korea <- map_data("world") %>% 
  filter(region=="North Korea")

China <- map_data("world") %>% 
  filter(region=="China")

Japan <- map_data("world") %>% 
  filter(region=="Japan")

nw <- map_data("world") %>%
  filter(region %in% c("Russia","China","North Korea"))




# Site Data ---------------------------------------------------------------
Sampling.Site <- c("Block 161", "Boryeong", "Namhae", "Geoje", "Jinhaeman Bay", "Pohang", "Jukbyeon")
latitude<- c(37, 
             36.3,
             34.725,
             34.8,
             35.05,
             36.1,
             37.05)
longitude <- c(124.6,
               126.5,
               127.836,
               128.7,
               128.7,
               129.5,
               129.4)
Fisher_data <- data.frame(Sampling.Site, latitude, longitude) %>%
  mutate(gwak =c("West","West","South","South","South","N/A","East"),
         kim = c("West","West","West","West","West","East","East"))
Fisher_data$gwak <- factor(Fisher_data$gwak, levels=c("West","South","East","N/A"))
Fisher_data$kim <- factor(Fisher_data$kim, levels=c("West","East"))
head(Fisher_data)

latitude <- c(37.23, 36.5,34.55,34.6,35.3,36.3,37.25)
longitude <- c(125.2,125.9,127.4,128.85,129.61,129.9,129.95)
label_data <- data.frame(Sampling.Site, latitude, longitude)




myplot <- ggplot() +
  geom_polygon(data = S_Korea, aes(x=long, y = lat, group = group), fill="grey62", alpha=0.4) +
  geom_polygon(data = N_Korea, aes(x=long, y = lat, group = group), fill="grey8", alpha=0.5)+
  geom_polygon(data = China, aes(x=long, y = lat, group = group), fill="grey8", alpha=0.5) +
  geom_polygon(data = Japan, aes(x=long, y = lat, group = group), fill="grey8", alpha=0.5) +
  geom_point(data=Fisher_data, aes(x=longitude, y=latitude,fill=gwak,color=kim), pch = 21, stroke=1.2, size = 4) +
  scale_shape_manual(values=c(0,2)) +
  scale_color_manual(values=c("firebrick4","forestgreen"), name='Kim et al. (2010)') +
  scale_fill_manual(values=c("firebrick4","deepskyblue","forestgreen","white"), name='Gwak & Nakayama (2011)') +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = NA),
        panel.border=element_rect(color='black', fill='transparent'),
        axis.text=element_text(size=12),
        axis.title =element_text(size=12),
        legend.position="top",legend.box="vertical", legend.margin=margin(),
        legend.text=element_text(size=12), legend.title=element_text(size=12)) +
  coord_map(xlim= c(123.5, 131.5),  ylim = c(33,39)) +
  labs(x = "Longitude", y = "Latitude") +
  annotate("text", x = 128, y = 36.5, label = "Republic of\nKorea", size = 4, color="grey8") +
  annotate("text", x = 124.5, y = 36, label = "Yellow Sea", fontface = "italic", size = 4, color="darkslategrey") +
  annotate("text", x = 131, y = 37, label = "East Sea", fontface = "italic", size = 4, color="darkslategrey") +
  geom_text(data=label_data, aes(x=longitude, y=latitude, label=Sampling.Site), size=4) +
  scale_bar(lon = 130.2, lat = 33.1, 
            distance_lon = 50, distance_lat = 15, distance_legend = 40, 
            dist_unit = "km", orientation = FALSE)


myplot

png(here::here('results','FigureS1.png'), res=200, height=1200,width=1200)
print(myplot)
dev.off()



