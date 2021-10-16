########### Fisher et al. Fig1 ##############
#
# Map sampling sites
#
# MF 6/20/2020
#
#############################################



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
# fisher et al. sampling sites
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
  mutate(Marker="RAD") %>%
  mutate(population =c("Yellow Sea","Yellow Sea","Tsushima Current-Korea","Tsushima Current-Korea","Tsushima Current-Korea","Tsushima Current-Korea","East Sea"))
head(Fisher_data)


# canino et al. sampling sites
canino_data <- data.frame(Sampling.Site=c("",""),
                          Marker="msDNA (Canino et al. 2010)",
                          latitude=c(38.9833, 44.333),
                          longitude=c(128.700,145.866),
                          population=c("East Sea", "Japan")) %>%
  dplyr::select(all_of(colnames(Fisher_data)))

# suda et al sampling sites
suda_data <- read.csv(here::here('data','suda_et_al_2017_sites.csv')) %>%
  rename(Sampling.Site=site,latitude=long,longitude=lat) %>%
  filter(season== 'winter' | season == 'spring') %>%
  mutate(Marker=c('msDNA (Suda et al. 2017)'),
         population=ifelse(Sampling.Site%in%c('Shimane','Tottori'), 'Tsushima Current-Japan','Japan')) %>%
  mutate(Sampling.Site="") %>%
  dplyr::select(all_of(colnames(Fisher_data)))

#smirnova et al. sampling sites
smirnova_data <- data.frame(Sampling.Site=c("","",""),
                            Marker="msDNA (Smirnova et al. 2018)",
                            latitude=c(36.290214,38.5833,42.787925),
                            longitude=c(124.188296,128.700,131.919408),
                            population=c("Yellow Sea","East Sea","East Sea"))
##-- approx japan for smirnova: 44.808216, 146.948674

# combine data sets 
all_data <- rbind(Fisher_data,canino_data, suda_data, smirnova_data) %>%
  mutate(Sampling.Site = as.character(Sampling.Site),
         sites1 =ifelse(!(Sampling.Site %in% c("Block 161", "Boryeong")), Sampling.Site, ""),
         sites2=ifelse(Sampling.Site %in% c("Block 161", "Boryeong"), Sampling.Site, ""))
all_data$population <- factor(all_data$population, levels=c("Yellow Sea","Tsushima Current-Korea","Tsushima Current-Japan","East Sea","Japan"))
all_data$Marker <- factor(all_data$Marker, levels=c("RAD","msDNA (Smirnova et al. 2018)",'msDNA (Suda et al. 2017)',"msDNA (Canino et al. 2010)"))



# Figure 1. Map -----------------------------------------------------------

# pop_cols <- c("firebrick4","deepskyblue","deepskyblue4","forestgreen","olivegreen4")
pop_cols <- c("firebrick4","deepskyblue","dodgerblue4","darkgreen","olivedrab3")

# add tsushima warm current and degrees to axes
ylabel <- "Latitude (°N)"
xlabel <- "Longitude (°E)"

# base map with sites
myplot <- ggplot() +
  geom_polygon(data = S_Korea, aes(x=long, y = lat, group = group), fill="grey62", alpha=0.4) +
  geom_polygon(data = Japan, aes(x=long, y = lat, group = group), fill="grey30", alpha=0.5)+
  geom_polygon(data = nw, aes(x=long, y = lat, group = group), fill="grey20", alpha=0.5) +
  geom_point(data=all_data, aes(x=longitude, y=latitude,col=population,pch=Marker), stroke=1.5, size = 2) +
  annotate("text", x = 123, y = 38.2, label = "Yellow Sea", fontface = "italic", size = 3, color="darkslategrey") +
  annotate("text", x = 131.5, y = 39.8, label = "East Sea", size = 3, color="darkslategrey", fontface = "italic") +
  annotate("text", x = 129.9, y = 34.6, label = "Korea Strait", angle=45,fontface = "italic", size = 3, color="darkslategrey") +
  geom_label_repel(data=all_data, aes(x=longitude, y=latitude, label=sites1, color=population), 
                   nudge_x = c(rep(-10.5,5),133-filter(all_data, sites1 %in% c("Jukbyeon","Pohang"))$longitude,rep(0,13)), #adjust x for south / east
                   nudge_y = c(rep(0,2),rep(-1.5,3),rep(1.2,2),rep(0,13)),  #adjust y for south / east
                   direction='y', box.padding=0.18,
                   segment.colour = 'grey40',
                   force=10, segment.size=0.5, size=3.5, show.legend=FALSE) +
  geom_label_repel(data=all_data, aes(x=longitude, y=latitude, label=sites2, color=population), 
                   nudge_y = c(42-filter(all_data, sites2 != '')$latitude,rep(0,18)),
                   direction='x', 
                   segment.colour = 'grey40',
                   force=1, segment.size=0.5, size=3.5, show.legend=FALSE) +
  scale_color_manual(values=pop_cols, name='Population') +
  scale_shape_manual(values=c(16,5,2,0)) +
  coord_map(xlim= c(120, 146),  ylim = c(31,45)) +
  labs(x = xlabel, y = ylabel) +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = NA),
        panel.border=element_rect(color='black', fill='transparent'),
        axis.text=element_text(size=12),
        axis.title =element_text(size=12),
        legend.position="right",legend.box="vertical", legend.margin=margin(),
        legend.text=element_text(size=12), legend.title=element_text(size=12))
myplot
# add current
myplot2 <- myplot + geom_segment(aes(x=125,xend=128.4,y=31, yend=33.6), linetype=2,
                                  col="orange4") +
  geom_segment(aes(x=128.4,xend=132,y=33.6, yend=35.5), linetype=2, 
               arrow = arrow(length=unit(0.2,"cm"), type = "closed"),
               col="orange4") +
  geom_segment(aes(x=128.4,xend=130,y=33.6, yend=35.8), linetype=2, 
               arrow = arrow(length=unit(0.2,"cm"), type = "closed"),
               col="orange4") +
  annotate("text", x = 125, y = 32, label = "TWC", fontface = "italic", size = 3, color="orange4")

png(here::here('results','Figure1.png'), res=200, height=1800,width=1600)
print(myplot2)
dev.off()

