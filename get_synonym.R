#' @title Get synonyms
#' @description  Function to get synonyms of both valid and non-valid taxonomic names
#' @details INPUT: 1) scientific names
#' @details OUTPUT: 1) synonyms
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



get_synonym <-  function(species){
  
  name_match <- species %>% 
    trimws()
  
  print(paste("Analyzing",name_match))
  
  
  
  itis.class <- try(name_match %>% get_gbifid_(.) %>% rbindlist() %>% tbl_df)
    
    
    if(!inherits(itis.class, "try-error")){ 
      
      if(!nrow(itis.class)==0){
        
        itis.class.sp <- itis.class %>% filter(status ==  "ACCEPTED" & matchtype=="EXACT")
        
        if(nrow(itis.class)!=0 && nrow(itis.class.sp)==0){
          
          itis.class.sp <- itis.class %>% filter(matchtype=="EXACT"| matchtype=="FUZZY")  %>% filter(canonicalname==name_match)
          
          if(nrow(itis.class.sp)==0){
            
            itis.class.sp <- itis.class %>% filter(status ==  "ACCEPTED" & matchtype=="FUZZY")
            
          }
          
          if(nrow(itis.class.sp)==0){
            
            itis.class.sp <- itis.class %>% filter(status ==  "SYNONYM" & matchtype=="FUZZY")
            
          }
          
          if(nrow(itis.class.sp)==0){
            
            itis.class.sp <- itis.class %>% filter(status ==  "SYNONYM" & matchtype=="EXACT")
            
          }
          
          if(nrow(itis.class.sp)==0){
            
            itis.class.sp <- itis.class %>% filter(matchtype=="EXACT")
            
          }
          
          if(nrow(itis.class.sp)==0){
            
            itis.class.sp <- itis.class %>% filter(matchtype=="FUZZY")
            
          }
          
          if(nrow(itis.class.sp)==0){
            
            itis.class.sp <- itis.class %>% filter(matchtype=="HIGHERRANK") 
            
            if(!any(colnames(itis.class.sp)=="species")){
              
              itis.class.sp <- itis.class.sp %>% mutate(species=name_match)
            }
            
          }
        }
        
        
        if(!any(colnames(itis.class.sp)=="kingdom")){
          
          itis.class.sp <- itis.class.sp %>% mutate(kingdom="NA")
        }
        
        if(!any(colnames(itis.class.sp)=="order")){
          
          itis.class.sp <- itis.class.sp %>% mutate(order="NA")
        }
        
        if(!any(colnames(itis.class.sp)=="class")){
          
          itis.class.sp <- itis.class.sp %>% mutate(class="NA")
        }
        
        if(!any(colnames(itis.class.sp)=="family")){
          
          itis.class.sp <- itis.class.sp %>% mutate(family="NA")
        }
        
        if(!any(colnames(itis.class.sp)=="genus")){
          this.genus <- strsplit(name_match," ") %>% unlist %>% .[1]
          itis.class.sp <- itis.class.sp %>% mutate(genus=this.genus)
        }
        
        
        if(!any(colnames(itis.class.sp)=="species")){
          
          itis.class.sp <- itis.class.sp %>% mutate(species=canonicalname)
        }
        
        if(nrow(itis.class.sp)>1) itis.class.sp <- itis.class.sp[1,]
        
        if(nrow(itis.class.sp)!=0){
          
          itis.class.sp %>% 
            mutate(old_name = name_match) %>% 
            select(old_name,kingdom,class,order,family,genus,species) %>% 
            setNames(c("old_name","kingdom","class","order","family","genus","valid_name")) %>% 
            mutate(match= TRUE) %>% 
            mutate(source="ITIS")
          
          
        }
      } else {
        
        genus.sp <-  name_match %>% strsplit(" ") %>% unlist %>% .[1]
        
        itis.class <- try(genus.sp %>% get_gbifid_(.) %>% rbindlist() %>% tbl_df) %>%  
          filter(status ==  "ACCEPTED" & matchtype=="EXACT")
        
        if(nrow(itis.class)==0){
          
          itis.class <- matrix("NA",1,5) %>% tbl_df() %>% 
            setNames(c("kingdom","class","order","family","genus"))
        }
        
        itis.class %>% .[1,] %>% 
          mutate(old_name = name_match) %>% 
          select(old_name,kingdom,class,order,family,genus) %>% 
          mutate(valid_name = name_match) %>% 
          mutate(match= FALSE) %>% 
          mutate(source="ITIS")
      }
      
      
    } else {
      
      genus.sp <-  name_match %>% strsplit(" ") %>% unlist %>% .[1]
      
      itis.class <- try(genus.sp %>% get_gbifid_(.) %>% rbindlist() %>% tbl_df) %>%  
        filter(status ==  "ACCEPTED" & matchtype=="EXACT")
      
      itis.class %>% .[1,] %>% 
        mutate(old_name = name_match) %>% 
        select(old_name,kingdom,class,order,family,genus) %>% 
        mutate(valid_name = name_match) %>% 
        mutate(match= FALSE) %>% 
        mutate(source="ITIS")
    }
    
  }) %plan% multiprocess
  
  while (!resolved(itis.value)) {
    print(cat("Resolving ITIS ...\n"))
    Sys.sleep(0.2)
  }
  
  while (!resolved(worms.value)) {
    print(cat("Resolving WORMS ...\n"))
    Sys.sleep(0.2)
  }
  
  
  accepted.name <- value(worms.value) %>% bind_rows(value(itis.value))
  
  #gives preference to ITIS
  
  test.itis <- accepted.name %>% 
    filter(source=="ITIS") %>% 
    .$match
  
  if(test.itis==TRUE){
    
    accepted.result <- accepted.name %>% 
      filter(source=="ITIS")
  } else {
    accepted.result <- accepted.name %>% 
      filter(source=="WORMS")
  }
  
  #accepted.name <- accepted.worms.name %>% bind_rows(accepted.itis.name)
  
  
  
  return(accepted.result)
}
