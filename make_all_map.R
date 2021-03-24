#' @title Make occurrence maps for all species combined
#' @description  Function to map occurrence for all species for both regions combined
#' @details INPUT: 1) uses data of thinned species records, with 20 records or more, 2) sample area name, bounding coordinates
#' @details OUTPUT: 1) occurrence map
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


make_all_map <- function(combined.data, min.lon= -130.00, max.lon=-50, min.lat=-10.00, max.lat=40.00){
  
  world <- ne_countries(scale = "large", returnclass = "sf")
  class(world)
  
  world_points<- st_centroid(world)
  world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
  
  model.map <- ggplot(data = world) +
    geom_sf() +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering) +
    #geom_text(data= world_points,aes(x=X, y=Y, label=name),
    #          color = "darkgrey", check_overlap = FALSE) +
    coord_sf(xlim = c(min.lon, max.lon), ylim = c(min.lat, max.lat), expand = FALSE)+
    geom_point(data =combined.data, aes(x = lon, y = lat),
               colour = "darkblue", size=0.01) +
    annotation_scale(location = "bl", width_hint = 0.2) +
    xlab("Lon")+
    ylab("Lat")+
    theme_bw()+
    annotate(geom="text", x=-103, y=25, label="Mexico",
             color="black")+
    theme(plot.title = element_text(color="black", size=12, face="italic"))+
    theme(axis.text = element_text(size = 8)) 
  
  ggsave("all_occurrence_map.png", units = "cm", width = 14, height = 9)   
  
}

