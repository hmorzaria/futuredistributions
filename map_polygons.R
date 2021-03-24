#' @title Mapping Atlantis polygons
#' @description  Functions to plot shapefile
#' @details INPUT: 1) An Atlantis model polygon shape file
#' @details OUTPUT: 1) Map of Atlantis polygons
#' @details Used code from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com

# set locale to avoid multibyte errors
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/

#' LIBRARIES
#' -----------------
# List of packages for session
.packages = c("rgdal","data.table","tidyverse","here","maptools","broom","ggmap",
              "sf", "rnaturalearth", "rnaturalearthdata","ggspatial","rgeos")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

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
