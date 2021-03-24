#' @title Get Fishbase
#' @description  Extract data from Fishbase
#' @details INPUT: 1) ecorrregions
#' @details Gets occurrence tables by region from Fishbase
#' @details OUTPUT: 1) Occurence data
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com

get_fishbase <- function(this.site){
  print("Now querying Fishbase")

  this.link <- paste(mirror,"/map/EcosystemOccurrencesList.php?e_code=",this.site,sep="")
  fishbase.data <- getURL(this.link) %>% readHTMLTable()
  
  biodiversity <- fishbase.data %>% .$dataTable %>% tbl_df %>% 
    dplyr::select(2,6,5) %>% 
    mutate(source = "fishbase") %>% 
    setNames(c("species","lon","lat","source")) %>% 
    #eliminate empty spaces and duplicate records
    mutate_each(funs(gsub("^$|^ $", NA, .))) %>% 
    na.omit %>% 
    distinct(species, lat, lon, source)
  
  str(biodiversity)
  return(biodiversity) 
  
}
#' print records


