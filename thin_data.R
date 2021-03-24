#' @title Thin data
#' @description  Function to thin occurrence records in geographic space to correct for bias
#' @details INPUT: 1) Coordinates for species, with 20 records or more 2) list of species, 3) sample area name
#' @details OUTPUT: 1) folder with thinned data
#' @details Species names were updated manually checking list. There were spelling errors as well
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


thin_data <- function(this.species, goc.20.sp, thissamplearea){
  
  thinpath <- paste("~/conapescaspecies/thindata",thissamplearea,sep="_")
  dir.create(thinpath)
  
  print(this.species)
  
  
  print(paste("Analyzing species",this.species, ":",length(sp.goc.20),sep=" "))
  
  setwd(thinpath)
  
  if(!file.exists(paste(this.species,"_thinned_full_log_file.txt",sep=""))){
    
    this.data <- goc.20.sp %>% 
      filter(species==this.species) %>% 
      dplyr::select(species,lat,lon)
    
    thinned_dataset_full <-
      thin( loc.data = this.data, 
            lat.col = "lat", long.col = "lon", 
            spec.col = "species", 
            thin.par = 10, reps = 100, 
            locs.thinned.list.return = TRUE, 
            write.files = TRUE, 
            max.files = 5, 
            out.dir = paste(thissamplearea,"_",this.species,"_full","/",sep=""), out.base = paste(thissamplearea,"_",this.species,"_thinned",sep=""), 
            write.log.file = TRUE,
            log.file = paste(this.species,"_thinned_full_log_file.txt",sep=""))
    #used code from https://cran.r-project.org/web/packages/MaxentVariableSelection/vignettes/MaxentVariableSelection.pdf
    
    setwd(paste(thinpath,"/",thissamplearea,"_",this.species,"_full",sep=""))
    png(sprintf('thinplot_%s.png', this.species))
    
    plotThin(thinned_dataset_full, which = 2, ask=FALSE)
    dev.off()
    
    return(thinned_dataset_full)
    
  }
  
}
