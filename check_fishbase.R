#' @title Check taxonomy
#' @description  Extract synonyms from WORMS
#' @details INPUT: 1) scientific names
#' @details Gets synonyms for every name in WORMS
#' @details OUTPUT: 1) updated scientific names
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com




check_fishbase <-  function(thisspecies){
  
  name.match <- thisspecies %>% 
    trimws()
  
  print(paste("Analyzing",name.match))
  
  file.test <- file.exists(paste0("/home/atlantis/conapescaspecies/synonym_res/synonyms_worms_",name.match))

  if(file.test==FALSE){
 
  name.tsn <- ritis::search_scientific(name.match)
  

  if(nrow(name.tsn)!=0){
    
    synonym.itis <- ritis::synonym_names(name.tsn$tsn)
    
  if(nrow(synonym.itis)!=0){
  
    final.names <- synonym.itis %>% 
      dplyr::select(sciName) %>% 
      dplyr::rename(synonym = sciName) %>% 
      mutate(old_name = name.match, valid_name = name.match)
    
    write_csv(final.names,paste0("/home/atlantis/conapescaspecies/synonym_res/synonyms_worms_",name.match,".csv"))
    
    table.return <- tibble(old_name=name.match,found =1)
    
    return(table.return)
    
  }  
    
  } 
  
  #this uses package ritis
  #search_scientific(name.match)
 
  
  }
  
  Sys.sleep(5)
  
  }
