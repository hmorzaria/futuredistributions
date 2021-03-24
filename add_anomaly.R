#' @title Add anomaly
#' @details Add anomaly to interpolated ensemble model
#' @details INPUT: 1) Ensemble model per species, 2) future raster set name, 3) raster path, 4) set of selected rasters
#' @details OUTPUT: 1) ensemble of projected models
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


add_anomaly <- function(eachraster, cmip.rasters, save.dir, obs.dir, thismodel){
  
  print(eachraster)
  print(thismodel)
  
  raster.name <- eachraster %>% 
    str_split("_") %>% 
    unlist 
  
  if(length(raster.name)==3){
    
    raster.var <- raster.name %>% 
      .[3] %>% 
      gsub("[.]asc","",.)
  } 
  
  if(length(raster.name)==2){
    
    raster.var <- raster.name %>% 
      .[2] %>% 
      gsub("[.]asc","",.)
  } 
  
  cmip.ras.name <- grep(raster.var,cmip.rasters, value = TRUE)
  
  print("read rasters")
  
  this.obs <- raster(paste0(obs.dir,"/",eachraster))
  this.cmip <- raster(cmip.ras.name)
  
  print("set CRS")
  projection(this.obs) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
  
  projection(this.cmip) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
  
  print("Create new raster")
  new.raster <- this.obs + this.cmip
  
  writeRaster(new.raster,filename=paste0(save.dir,"/",eachraster),format="ascii",overwrite=TRUE) 
}