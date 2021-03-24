#' @title Make occurrence maps 
#' @description  Function to map occurrence by species
#' @details INPUT: 1) Coordinates for species, with 20 records or more 2) list of species, 3) sample area name, bounding coordinates
#' @details OUTPUT: 1) occurrence maps
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


make_occ_map <- function(thisspecies, thissamplearea,min.lon, max.lon, min.lat, max.lat, map.dir) {
  
  eachspecies <- capitalize(thisspecies)
  
  test.file <-  file.exists(paste(map.dir,thissamplearea,"_",eachspecies,"_map.png",sep=""))   
  
  sp.folder <- thisspecies %>% 
    str_split("full_log_file.txt") %>% 
    unlist %>% 
    .[1] %>% 
    gsub("_thinned","",.)
  
  sp.folder.path <- sp.folder %>% 
    gsub(" ","_",.)
  
  sp.thin.list.files <- list.files(path=paste("~/conapescaspecies/thindata_",thissamplearea,"/",thissamplearea,"_",sp.folder, "_full",sep=""), pattern = "*.*csv$")
  
  max.file <- regmatches(sp.thin.list.files, regexpr( "\\d+", sp.thin.list.files)) %>% 
    max()
  
  thin.file <- sp.folder %>% 
    gsub("_","",.) %>% 
    paste(thissamplearea,"_",.,"_thinned_thin",max.file,".csv", sep="")
  
  thin.data <- read_csv(paste("~/conapescaspecies/thindata_",thissamplearea,"/",thissamplearea,"_",sp.folder, "_full/",thin.file,sep=""))
  
  
  if(test.file == FALSE) {
       
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
      geom_point(data = thin.data, aes(x = lon, y = lat, group=species),
                 colour = "darkblue", size=0.5) +
      annotation_scale(location = "bl", width_hint = 0.2) +
      xlab("Lon")+
      ylab("Lat")+
      theme_bw()+
      #annotate(geom="text", x=-101, y=23, label="Mexico",
      #         color="black")+
      ggtitle(eachspecies)+
      theme(plot.title = element_text(color="black", size=12, face="italic"))+
      theme(axis.text = element_text(size = 8)) 
    
    ggsave(paste(map.dir,thissamplearea,"_",eachspecies,"_map.png",sep=""), units = "cm", width = 13.5, height = 9)   
    
  }
  
  return(thin.data)
  
}
