#' @title Combine Ulloa data with occurrence records
#' @description  Combine csv files from Ulloa et al. 2006 with occurrence records
#' @details #' Get shark and seagrass data files from Ulloa et al. 2006, these were the only groups with species-level data
#' @details INPUT: 1) csv files
#' @details OUTPUT: 1) common csv file
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



get_csv <- function(eachfile) {

  print(eachfile)
  
    ulloa.data  <-  fread(eachfile, header=TRUE) %>% 
      tbl_df %>% 
      dplyr::select(NOM_CIEN, LONGITUD, LATITUD) %>% 
      mutate(source="ulloa") %>% 
      setnames(c("species","lon","lat","source")) %>% 
      mutate(species=trimws(species)) %>%
      mutate(species=tolower(species)) %>%
      distinct(species, lat, lon, source) %>% 
      na.omit

    return(ulloa.data)    

}



