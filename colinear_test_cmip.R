#' @title generate ensemble models by species
#' @details INPUT: 1) Coordinates for species, 2) raster set name, 3) sample area, 4) raster path, 5) set of selected rasters
#' @details OUTPUT: 1) ensemble of models
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


colinear_test_cmip <- function(thissamplearea, list.rasters, raster.name) {
  
  raster.stack <- raster::stack(list.rasters)
  
  projection(raster.stack) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
  
  collin.rast <- removeCollinearity(raster.stack, multicollinearity.cutoff = 0.75,
                                    select.variables = TRUE, sample.points = FALSE, plot = FALSE)
  
  rasters.selected <- subset(raster.stack, collin.rast)
  
  write.csv(names(rasters.selected), paste(thissamplearea,"_",raster.name,"_rasters_selected.csv",sep=""))
  
  #get correlation matrix for environmental layers from ENMtools
  #cor.matrix <- raster.cor.matrix(rasters.env)
  
  #plot environmental variables
  #cor.plot <- raster.cor.plot(rasters.env)
  
  #correlation plot
  #mds.plot <- cor.plot$cor.mds.plot
  
  #heatmap
  #cor.heatmap <- cor.plot$cor.heatmap
  
  #save correlation and heatmap 
  #ggsave(paste(thissamplearea,"correlation_plot.png",sep="_"), mds.plot, width = 10, height = 10, dpi = 150, device='png')
  
  #ggsave(paste(thissamplearea,"heatmap_plot.png",sep="_"), cor.heatmap, width = 10, height = 10, dpi = 150, device='png')
  
 
}
