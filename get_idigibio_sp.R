#' @title Get species records from iDigBio
#' @description  Extract species occurrencerecords from iDigBio
#' @details INPUT: 1) scientific names
#' @details OUTPUT: 1) occurrence records
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


#' iDigBio

get_idigibio_sp <- function(eachspecies){
  
  print(eachspecies)
  
  record.file<- paste0("~/conapescaspecies/biodiverdata/data_files/",eachspecies,"_idigibio.csv")
  
  if(file.exists(record.file)==FALSE){

    df1 <- try(idig_search_records(rq=list(scientificname=eachspecies, geopoint=list(type="exists")), fields=c("scientificname", "geopoint")))
    
    if(!inherits(df1, "try-error")){
      
      if(nrow(df1)!=0){
        
        biodiversity.sp <- df1 %>% 
          dplyr::select(scientificname,geopoint.lon,geopoint.lat) %>% 
          as_tibble %>% 
          mutate(source = "idigbio") %>% 
          dplyr::rename(species=scientificname, lon=geopoint.lon,lat=geopoint.lat) %>% 
          na.omit %>% 
          mutate(submitted_name = eachspecies)
        
        write_csv(biodiversity.sp,paste0("~/conapescaspecies/biodiverdata/data_files",eachspecies,"_idigibio.csv"))
        
        Sys.sleep(5)
        # return(biodiversity.sp) 
        
      }
      
    }      

  }
  #rec_count <- try(idig_count_records(rq=list(scientificname=eachspecies, geopoint=list(type="exists")), fields=c("scientificname", "geopoint")))
  
}
