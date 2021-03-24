#' @title generate ensemble models by species
#' @details INPUT: 1) Coordinates for species, 2) raster set name, 3) sample area, 4) raster path, 5) set of selected rasters
#' @details OUTPUT: 1) ensemble of models
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


ensemble_smd <- function(this.species,raster.name,thissamplearea,raster.path,rasters.selected, sdm.dir, ocean.shp) {
  
  print(this.species)
  
  
  sdm.dir.sp <- paste0(sdm.dir,"/",this.species,"_esdm")

  test.dir <- file.exists(paste0(sdm.dir.sp,"/",this.species,"_model"))
  
  if(test.dir==FALSE) {
    
    dir.create(paste0(sdm.dir,"/",this.species,"_esdm"))
    
    sp.folder <- this.species
    
    sp.folder.path <- sp.folder %>% 
      gsub(" ","_",.)
    
    sp.thin.list.files <- list.files(path=paste("~/conapescaspecies/thindata_",thissamplearea,"/",thissamplearea,"_",sp.folder, "_full",sep=""), pattern = "*.*csv$")
    
    max.file <- regmatches(sp.thin.list.files, regexpr( "\\d+", sp.thin.list.files)) %>% 
      max()
    
    thin.file <- sp.folder %>% 
      gsub("_","",.) %>% 
      paste(thissamplearea,"_",.,"_thinned_thin",max.file,".csv", sep="")
    
    occ.data <- load_occ(path = paste("~/conapescaspecies/thindata_",thissamplearea,"/",thissamplearea,"_",sp.folder, "_full/",sep=""), rasters.selected,
                         Xcol = 'lon', Ycol = 'lat', Spcol = 'species',
                         file = thin.file, sep = ',', verbose = FALSE) %>% 
      mutate(species=as.factor(species))
    
    
    if(nrow(occ.data)<20){
      
      print(paste("species", this.species, "has insufficient records"))
      
    } else if(nrow(occ.data)>=20){
      
      #Ensemble species distribution models (ESDMs)
      # 'RF', took out because it takes so much time in SSDM and in LightSSDM I kept getting a warning, same with Support vector machines (KSVM) 
      #Multivariate adaptive regression splines (MARS), 
      #Generalized boosted regressions model (GBM), 
      #Classification tree analysis (CTA), 
      #Artificial neural network (ANN)
       
      esdm.model <- ensemble_modelling(c('SVM','ANN','CTA','GBM','MARS'), 
                                       subset(occ.data, occ.data$species == this.species),
                                       rasters.selected, 
                                       Xcol = 'lon', 
                                       Ycol = 'lat',
                                       ensemble.thresh = c(0.75),
                                       cv = "holdout", 
                                       cv.param = c(0.7,10),
                                       uncertainty= TRUE, 
                                       verbose = TRUE)
      
      save.esdm(esdm.model, paste(this.species,"esdm",sep="_"),
                path = sdm.dir, verbose = TRUE, GUI = FALSE)
      
      
      sp.name <- str_to_sentence(this.species)
      
      
      model.proj.un <- esdm.model@uncertainty
      
      model.proj.un.mask.ocean <- raster::mask(model.proj.un, ocean.shp)
      
      model.proj.un.df <- as.data.frame(model.proj.un.mask.ocean, xy = TRUE) %>% 
        dplyr::rename(Uncertainty = uncertainty.map)
      
      
      model.proj.un.plot <- ggplot() +
        geom_raster(data = model.proj.un.df , aes(x = x, y = y, fill = Uncertainty)) +
        scale_fill_viridis_c(na.value = "gray85", option = "magma") +
        coord_quickmap()+
        theme_minimal()+
        ggtitle(sp.name) +
        xlab("Lon") + 
        ylab("Lat") +
        theme(plot.title = element_text(color="black", size=11, face="bold.italic"))
      
       save_plot(filename= paste("esdm",this.species,"un.png",sep="_"), plot = model.proj.un.plot, path =  sdm.dir.sp, ncol = 2, base_asp = 0.9)
      
      # ggsave(paste("esdm",this.species,"un.png",sep="_"), model.proj.un.plot, height = 8.75, width = 10, units = "cm", device = "png",
      #        path = sdm.dir.sp)
      # 
      # ggsave(paste("esdm",this.species,"un.png",sep="_"), model.proj.un.plot, device = "png", dpi= 400,
      #        path = sdm.dir.sp)
      
      
      model.proj <- esdm.model@projection
      
      model.proj.mask.ocean <- raster::mask(model.proj, ocean.shp)
      
      model.proj.df <- as.data.frame(model.proj.mask.ocean, xy = TRUE) 
      
      model.proj.plot <- ggplot() +
        geom_raster(data = model.proj.df , aes(x = x, y = y, fill = Probability)) +
        scale_fill_viridis_c(na.value = "gray85") +
        coord_quickmap()+
        theme_minimal()+
        ggtitle(sp.name) +
        xlab("Lon") + 
        ylab("Lat") +
        theme(plot.title = element_text(color="black", size=11, face="bold.italic"))
      
      save_plot(filename= paste("esdm",this.species,"prob.png",sep="_"), plot = model.proj.plot, path =  sdm.dir.sp, ncol = 2, base_asp = 0.9)
      
      saveRDS(esdm.model, file=paste0(sdm.dir.sp,"/",this.species,"_model"))
      
      return(sp.name)
      
      #to read model  
      # esdm.model <- readRDS(file=paste("/home/atlantis/conapescaspecies/sdm/",this.species,"_esdm/",this.species,"_model",sep=""))
    }
    
  }
  
  
}
