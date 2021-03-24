#' @title Check taxonomy
#' @description  Extract synonyms from ITIS
#' @details INPUT: 1) scientific names
#' @details Gets synonyms for every name in ITIS
#' @details OUTPUT: 1) updated scientific names
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com




check_itis <-  function(thisspecies){
  
  name.match <- thisspecies %>% 
    trimws()
  
  print(paste("Analyzing",name.match))
  
  file.test <- file.exists(paste0("/home/atlantis/conapescaspecies/synonym_res/synonyms_itis_",name.match))

  if(file.test==FALSE){
 
  name.tsn <- ritis::search_scientific(name.match)

  if(nrow(name.tsn)!=0){
    
    print("get tsn")
    
  this.tsn <- name.tsn %>% 
    mutate(combinedName=tolower(combinedName)) %>% 
    filter(grepl(name.match,combinedName)) %>% 
    distinct(tsn) %>% 
    pull(tsn)  

#  ritis::accepted_names(this.tsn)
#  ritis::search_scientific(this.tsn)
  
  if(!is.null(this.tsn)){
    
    synonym.list <- list()
    
    for(eachtsn in 1:length(this.tsn)){
      
      thistsn <- this.tsn[eachtsn]
      
      this.synonym.itis <- ritis::synonym_names(thistsn)
      
      synonym.list[[eachtsn]] <- this.synonym.itis
      
    }
      synonym.itis <- synonym.list %>% 
        bind_rows()
      
  if(nrow(synonym.itis)!=0){
  
    final.names <- synonym.itis %>% 
      dplyr::select(sciName) %>% 
      dplyr::rename(synonym = sciName) %>% 
      mutate(old_name = name.match, valid_name = name.match)
    
    write_csv(final.names,paste0("/home/atlantis/conapescaspecies/synonym_res/synonyms_itis_",name.match))
    
     
  } else if(nrow(synonym.itis)==0){
    
    table.return <- tibble(old_name=name.match,found =0)
    
    return(table.return)
    
    
  }
  } else  if(is.null(this.tsn)){
    
    table.return <- tibble(old_name=name.match,found =0)
    
    return(table.return)
    
  } 
    
    
  } else if(nrow(name.tsn)==0){
    
    table.return <- tibble(old_name=name.match,found =1)
    
    return(table.return)
    
    
  }
  
  #this uses package ritis
  #search_scientific(name.match)
 
  } 

  
  Sys.sleep(5)
  
  }
