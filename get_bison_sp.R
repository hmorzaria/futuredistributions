#' @title Get species records from Bison
#' @description  Extract species occurrencerecords from Bison
#' @details INPUT: 1) scientific names
#' @details OUTPUT: 1) occurrence records
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


#' Bison

get_bison_sp <- function(eachspecies, areapolygon){
  
  print(eachspecies)
  
  record.file <- paste0("~/conapescaspecies/biodiverdata/data_files/",eachspecies,"_bison.csv")
  
  if(file.exists(record.file)==FALSE){
    

  spocc.data  <-  try(bison(species = eachspecies, type = "scientific_name", aoi=areapolygon, config=verbose()))
  # spocc.data  <-  bison(species = this.species, type = "scientific_name")
  
  if(!inherits(spocc.data, "try-error")){
    
    spocc.data.source  <-  spocc.data$points %>% 
      as_tibble
    
    if(any(spocc.data.source$geo=="Yes")) {
      
      biodiversity.sp <- spocc.data.source %>% 
        mutate(source = "bison") %>% 
        dplyr::select(name,decimalLongitude,decimalLatitude,source) %>% 
        na.omit %>% 
        dplyr::rename(species=name, lon=decimalLongitude, lat=decimalLatitude) %>% 
        distinct(species, lat, lon, .keep_all=TRUE) %>% 
        na.omit %>% 
        mutate(submitted_name = eachspecies)
      
      
      write_csv(biodiversity.sp,paste0("~/conapescaspecies/biodiverdata/data_files/",eachspecies,"_bison.csv"))
      
      Sys.sleep(5)
      
      
    } 
    
  } }


        
}      