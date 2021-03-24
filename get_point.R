#' @title Subset occurrence records for each ocean
#' @description  Check if occurrence records are within 
#' @details #' Get shark and seagrass data files from Ulloa et al. 2006, these were the only groups with species-level data
#' @details INPUT: 1) csv files
#' @details OUTPUT: 1) common csv file
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



get_point <- function(eachrow, biodiver.coor, area.shape, area.name) {
  
 # print(eachrow)
  #read in shapefile
  
  
  this.row <- biodiver.coor[eachrow,] %>% 
    as.data.frame
  
  test.file <- file.exists(paste0("~/conapescaspecies/biodiverdata/",area.name,"_data_files/",area.name,"_",eachrow,".csv"))
  
  if(test.file==FALSE){
   # print(this.row)
  
    coordinates(this.row) <- c("lon", "lat")  # set spatial coordinates
    proj4string(this.row) <- crs.geo.wgs  # define projection system of our data
    
    # subset ocurrence points within GOC
    #get table from shapefile
    
    biodiversity.sec <- this.row[area.shape, ] %>% 
      as("data.frame")
    
    test.bio  <- nrow(biodiversity.sec)==0
    
    if (test.bio==FALSE)
    {
      
        write_csv(biodiversity.sec, paste0("~/conapescaspecies/biodiverdata/",area.name,"_data_files/",area.name,"_",eachrow,".csv"))
      
      
    }
    
  } # end if this row !0
  
}


