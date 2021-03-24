#' @title Combine xls files with occurrence records
#' @description  XLS files from UABCS with occurrence records
#' @details INPUT: 1) XLS files
#' @details OUTPUT: 1) common csv file
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' Read excel files from UABCS, Pangas, etc.



#loop to read in data and obtain GOC data
get_xls <- function(eachfile) {
  
  print(paste("Analyzing",eachfile))
  
  df  <-  read_excel(eachfile, sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0)
  indx.sp <- grep("species|Species|Especie|Nombre|especie|nombre|scientificName",colnames(df))
  indx.fuen  <- grep('source|Source|Fuente|fuente|informacion|Base|Inst',colnames(df))
  indx.lon <- grep('Longitude|Longitud|longitud|Lon|longitude',colnames(df))
  indx.lat  <- grep('Latitude|Latitud|latitud|Latutud|Lat|latitude',colnames(df))
  
  xls.data = df[,c(indx.sp,indx.lon,indx.lat,indx.fuen)]
  
  
  xls.data <- xls.data %>% 
    setnames(c("species","lon","lat","source")) %>% 
    mutate(species=trimws(species)) %>%
    mutate(species=tolower(species)) %>%
    distinct(species, lat, lon, .keep_all = TRUE) %>% 
    mutate(source = "xls_files") %>% 
    mutate_at(vars(c(lat,lon)), as.numeric, na.rm=TRUE) %>%
    na.omit
  
  str(xls.data)
  
  return(xls.data)
  
}
# end file


