#' @title generate projections using ensemble models by species
#' @details INPUT: 1) Ensemble model per species, 2) future raster set name, 3) raster path, 4) set of selected rasters
#' @details OUTPUT: 1) ensemble of models
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


ensemble_project <- function(this.species,raster.name,thissamplearea,raster.path,rasters.selected, sdm.dir, ocean.shp, thismodel) {
  
  print(this.species)
  
  sdm.dir.sp <- paste0(sdm.dir,"/",this.species,"_esdm")
  
  test.file <- file.exists(paste0(sdm.dir.sp,"/",this.species,"_proj"))
  
  # if(test.dir==FALSE) {
  #   
  #   sp.folder <- this.species
  #   
  #   sp.folder.path <- sp.folder %>% 
  #     gsub(" ","_",.)
  #   
  #   esdm.model.name <- paste0(this.species,"_model")
  #   
  #   setwd(sdm.dir.sp)
  #   
  #   esdm.model <- readRDS(esdm.model.name)
  #   
  #   esdm.projection <- SSDM::project(esdm.model,rasters.selected)
  #   
  #   sp.name <- str_to_sentence(this.species)
  #   
  #   proj.proj <- esdm.projection@projection
  #   
  #   model.proj.mask.ocean <- raster::mask(proj.proj, ocean.shp)
  #   
  #   model.proj.df <- as.data.frame(model.proj.mask.ocean, xy = TRUE) 
  #   
  #   model.proj.plot <- ggplot() +
  #     geom_raster(data = model.proj.df , aes(x = x, y = y, fill = Probability)) +
  #     scale_fill_viridis_c(na.value = "gray85") +
  #     coord_quickmap()+
  #     theme_minimal()+
  #     ggtitle(sp.name) +
  #     xlab("Lon") + 
  #     ylab("Lat") +
  #     theme(plot.title = element_text(color="black", size=11, face="bold.italic"))
  #   
  #   save_plot(filename= paste("esdm",this.species,thismodel,proj.png,sep="_"), plot = model.proj.plot, path =  sdm.dir.sp, ncol = 2, base_asp = 0.9)
  #   
  #   saveRDS(esdm.projection, file=paste0(sdm.dir.sp,"/",this.species,"_",thismodel,"_proj"))
  #   
  #   return(sp.name)
  #   
  #   #to read model  
  #   # esdm.model <- readRDS(file=paste("/home/atlantis/conapescaspecies/sdm/",this.species,"_esdm/",this.species,"_model",sep=""))
  # } else if (test.dir==TRUE){
    
  if(test.file==TRUE){
    
    esdm.projection <- readRDS(paste0(sdm.dir.sp,"/",this.species,"_proj"))
    
    proj.proj <- esdm.projection@projection
    
    model.proj.mask.ocean <- raster::mask(proj.proj, ocean.shp)
    
    writeRaster(model.proj.mask.ocean,file=paste0(sdm.dir.sp,"/Rasters/",this.species,"_",thismodel,"_projection"), format="ascii")
    
  }
  
  
}


