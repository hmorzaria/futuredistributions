#' @title Check taxonomy
#' @description  Function to check taxonomic validity of names
#' @details INPUT: 1) scientific names
#' @details Gives preference to WORMS but also got names from ITIS
#' @details OUTPUT: 1) updated scientific names
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com




check_taxonomy <-  function(thisspecies){
  
  name.match <- thisspecies %>% 
    trimws()
  
  print(paste("Analyzing",name.match))
  
  file.test.worms <- file.exists(paste0("/home/atlantis/conapescaspecies/synonym_res/synonyms_worms_",name.match))

  if(file.test.worms==FALSE){
  #fishbase.synonym <- rfishbase::synonyms(name.match)
  
  #this uses package ritis
  #search_scientific(name.match)
  
   
  worms.synonym <- taxize::synonyms(name.match, db='worms')
  
  worms.syn.df <- worms.synonym %>% 
    synonyms_df() 
  
  if(nrow(worms.syn.df)!=0){
    
    final.names <- worms.syn.df %>% 
      dplyr::select(.id, scientificname, valid_name) %>% 
      dplyr::rename(old_name = .id, synonym = scientificname)
    
    write_csv(final.names,paste0("/home/atlantis/conapescaspecies/synonym_res/synonyms_worms_",name.match))
    
    table.return <- tibble(old_name=name.match,found =1)
   
     return(table.return)
    
  } else if(nrow(worms.syn.df)==0){
    
    table.return <- tibble(old_name=name.match,found =0)
    
    return(table.return)
  }
  
  }
  
  Sys.sleep(5)
  
}
  # itis.synonym <- taxize::synonyms(name.match, db='itis')
  # 
  # itis.syn.df <- itis.synonym %>% 
  #   synonyms_df() 
  # 
  # itis.names <- names(itis.syn.df)
  # 
  # if(acc_name %in% itis.names){
  # 
  #   final.names <- itis.syn.df %>% 
  #     dplyr::select(.id,acc_name,syn_name) %>% 
  #     setNames(c("old_name","valid_name","synonym")) %>% 
  #     mutate(source="itis", valid_name=tolower(valid_name),synonym=tolower(synonym))
  #   
  # }
  # 
  # if(acc_tsn %in% itis.names){
  #   
  #   final.names <- itis.syn.df %>% 
  #     dplyr::select(.id,syn_name) %>%
  #     setNames(c("old_name","synonym")) %>% 
  #     mutate(source="itis", valid_name=name.match,synonym=tolower(synonym))
  #   
  # }
  
  
