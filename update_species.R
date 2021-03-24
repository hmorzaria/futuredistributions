#' @title Update species
#' @description  Function to update occurrence file with updated taxonomy
#' @details INPUT: 1) Coordinates for species 2) Updated species list
#' @details OUTPUT: 1) Coordinates for species with updated taxonomy
#' @details Species names were updated manually checking list. There were spelling errors as well
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


update_species <- function(thissamplearea){
  
  if(thissamplearea=="atlantic"){

    new.taxonomy <- read_csv(paste0(thissamplearea,"_sp.csv")) %>% 
        filter(!is.na(especie_rev)) %>% 
        mutate(new_species=paste(genero_rev,especie_rev, subesp_rev, sep=" ")) %>%
        mutate(new_species = gsub(" NA","",new_species)) %>% 
        dplyr::select(species, new_species)
    
  }
  
  if(thissamplearea=="pacific"){
    
    new.taxonomy <- read_csv(paste0(thissamplearea,"_sp.csv")) %>% 
      filter(!is.na(especie_rev)) %>% 
      mutate(new_species=paste(genero_rev,especie_rev, sep=" ")) %>%
      dplyr::select(species, new_species)
    
  }
  
  goc.20.sp <- read_csv(paste(thissamplearea,"_CONAPESCA_sp_20reg.csv"))
  
  
  updated.sp <- goc.20.sp %>% 
    left_join(new.taxonomy, by="species") %>% 
    mutate(new_species=if_else(is.na(new_species),species,new_species)) %>% 
    dplyr::select(-species) %>% 
    dplyr::rename(species=new_species) %>% 
    distinct(lat,lon,species)
  
  write_csv(updated.sp , paste(thissamplearea,"_CONAPESCA_sp_20reg_rev.csv"))
}