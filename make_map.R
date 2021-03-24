#' @title Make area maps 
#' @description  Function to map occurrence by species
#' @details INPUT: 1) Coordinates for map, scale and bar position
#' @details OUTPUT: 1) Location map
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


make_map <- function(shape.file, file.name,scale.factor, bar.position, min.long, max.long, min.lat,max.lat) {
  
  model.shape <- readOGR(shape.file)
  
  # Reformat shape for mapping purposes
  model.shape.df <- broom::tidy(model.shape)
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
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
    coord_sf(xlim = c(min.long, max.long), ylim = c(min.lat, max.lat), expand = FALSE)+
    geom_path(data = model.shape.df, aes(x = long, y = lat, group=group),
              colour = "lightgrey") +
    annotation_scale(location = "tl", width_hint = 0.2) +
    xlab("Lon")+
    ylab("Lat")+
    theme_bw()+
    annotate(geom="text", x=-102, y=24, label="Mexico",
             color="black")
  
  ggsave(file.name, model.map, width = 16,height = 12, units="cm", dpi = 400)
  
  return(model.map)
  
}

