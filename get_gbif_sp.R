#' @title Get species records from GBIF
#' @description  Extract species occurrencerecords from GBIF
#' @details INPUT: 1) scientific names
#' @details OUTPUT: 1) occurrence records
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


#' GBIF

get_gbif_sp <- function(eachspecies, areapolygon){
  
  print(eachspecies)
  
  record.file<- paste0("~/conapescaspecies/biodiverdata/data_files/",eachspecies,"_gbif.csv")
  
  if(file.exists(record.file)==FALSE){

      this.key <- name_backbone(name=eachspecies)$speciesKey %>% unlist
      
      if(!is.null(this.key)) {
        
         
       gbif.goc <- try(occ_data(taxonKey=this.key, geometry=areapolygon, hasGeospatialIssue=FALSE, limit=100000, curlopts=list(verbose=TRUE)))
         #  gbif.goc <- try(occ_data(taxonKey=this.key, hasCoordinate=TRUE, geometry=areapolygon, hasGeospatialIssue=FALSE, limit=100000, curlopts=list(verbose=TRUE)))
       
       
        if(!is.null(gbif.goc$data)){
          
          biodiversity.sp  <-  gbif.goc$data %>% 
            mutate(source = "gbif") %>% 
            dplyr::select(scientificName, decimalLongitude, decimalLatitude,source) %>% 
            dplyr::rename(species=scientificName, lon=decimalLongitude, lat=decimalLatitude) %>% 
            distinct(species, lat, lon, source) %>% 
            na.omit %>%  #eliminate rows with NA
            mutate(submitted_name = eachspecies)
          
          write_csv(biodiversity.sp,paste0("~/conapescaspecies/biodiverdata/data_files/",eachspecies,"_gbif.csv"))
          
          Sys.sleep(5)
          
        }
      }
    }
    
     }
  #rec_count <- try(idig_count_records(rq=list(scientificname=eachspecies, geopoint=list(type="exists")), fields=c("scientificname", "geopoint")))
  