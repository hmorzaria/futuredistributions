#' @title Get species records from OBIS
#' @description  Extract species occurrencerecords from OBIS
#' @details INPUT: 1) scientific names
#' @details OUTPUT: 1) occurrence records
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


#' OBIS

get_obis_sp <- function(eachspecies, areapolygon){
  
  print(eachspecies)
  
  record.file<- paste0("~/conapescaspecies/biodiverdata/data_files/",eachspecies,"_obis.csv")
  
  if(file.exists(record.file)==FALSE){

     obis.data.table  <- tryCatch(robis::occurrence(scientificname=eachspecies, geometry =areapolygon, fields=c("species","decimalLongitude", "decimalLatitude"),verbose=TRUE),error=function(e) as.character()) 
        
       
        if(!nrow(obis.data.table)==0){
          
          biodiversity.sp  <-  obis.data.table %>% 
            mutate(source = "obis") %>% 
            dplyr::select(species, decimalLongitude, decimalLatitude,source) %>% 
            dplyr::rename(lon=decimalLongitude, lat=decimalLatitude) %>% 
            distinct(species, lat, lon, source) %>% 
            na.omit %>%  #eliminate rows with NA
            mutate(submitted_name = eachspecies)
          
          write_csv(biodiversity.sp,paste0("~/conapescaspecies/biodiverdata/data_files/",eachspecies,"_obis.csv"))
          
          Sys.sleep(5)
          
        }
      }
    }
    
    