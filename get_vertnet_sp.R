#' @title Get species records from Vertnet
#' @description  Extract species occurrencerecords from Vertnet
#' @details INPUT: 1) scientific names
#' @details OUTPUT: 1) occurrence records
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


#' Vertnet

get_vertnet_sp <- function(eachspecies){
  
  print(eachspecies)
  
  record.file<- paste0("~/conapescaspecies/biodiverdata/data_files/",eachspecies,"_vertnet.csv")
  
  name.vect <- str_split(eachspecies," ") %>% unlist
  
  length.name <- length(name.vect)
  
  if(file.exists(record.file)==FALSE){

    if(length.name==2){
      
      vertnet.data.table <- try(searchbyterm(genus = name.vect[1], specificepithet = name.vect[2], limit = 100000, mappable=TRUE))
      
    }
      
    if(length.name==3){
      
      vertnet.data.table <- try(searchbyterm(genus = name.vect[1], specificepithet = name.vect[2], infraspecificepithet = name.vect[3], limit = 100000, mappable=TRUE))
      
    }
  
        if(!is.null(vertnet.data.table)){
          
          biodiversity.sp  <-  vertnet.data.table$data %>% 
            mutate(source = "vertnet") %>% 
            dplyr::select(scientificname, decimallongitude, decimallatitude,source) %>% 
            dplyr::rename(species=scientificname, lon=decimallongitude, lat=decimallatitude) %>% 
            distinct(species, lat, lon, source) %>% 
            na.omit %>%  #eliminate rows with NA
            mutate(submitted_name = eachspecies)
          
          if(!nrow(biodiversity.sp)==0){
            
          write_csv(biodiversity.sp,paste0("~/conapescaspecies/biodiverdata/data_files/",eachspecies,"_vertnet.csv"))
          
          }
          Sys.sleep(5)
          
        }
      }
    }