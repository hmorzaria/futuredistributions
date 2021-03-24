#' @title Get species records from Ecoengine
#' @description  Extract species occurrencerecords from ecoengine
#' @details INPUT: 1) scientific names
#' @details OUTPUT: 1) occurrence records
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


#' Berkley ecoengine

get_ecoengine_sp <- function(eachspecies, areapolygon){
  
  print(eachspecies)
  
  record.file<- paste0("~/conapescaspecies/biodiverdata/data_files/",eachspecies,"_ecoengine.csv")
  
  if(file.exists(record.file)==FALSE){

    #extract data from Berkley ecoengine
    
     
      ee.data  <-  tryCatch(ee_observations(scientific_name=eachspecies, bbox = '-50,-10,-130,40', page="all",georeferenced = TRUE),error=function(e) as.character()) 
      
      if(length(ee.data)!=0){
        
        ee.data.frame  <-  ee.data %>% 
          .$data %>%
          tbl_df %>% 
          mutate(source = this.source) %>% 
          dplyr::select(scientific_name, longitude, latitude, source) %>% 
          setnames(c("species","lon","lat","source")) %>% 
          #remove duplicates based on the combination of latitude, longitude and species
          distinct(species, lat, lon, source) %>% 
          na.omit %>% #eliminate rows with NA 
          mutate(submitted_name = eachspecies)
        
        
        write_csv(biodiversity.sp,paste0("~/conapescaspecies/biodiverdata/data_files/",eachspecies,"_ecoengine.csv"))
          
          Sys.sleep(5)
          
        }
      }
    }
    
     }
  #rec_count <- try(idig_count_records(rq=list(scientificname=eachspecies, geopoint=list(type="exists")), fields=c("scientificname", "geopoint")))
  