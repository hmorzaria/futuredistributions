#' Ocurrence record retrieval from CONAPESCA records
#' @author Hem Nalini Morzaria Luna
#' @date March 2016
#' organize data from excel spreadsheets
#' catch data for 2014-106
#' http://datos.gob.mx/

# List of packages for session

devtools::install_github("danlwarren/ENMTools")
install.packages(c("XMLSchema", "SSOAP"), repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"))
if(!require(robis)){devtools::install_github("iobis/robis"); library(robis)}
if(!require(taxizesoap)){devtools::install_github("ropensci/taxizesoap"); library(taxizesoap)}


.packages = c("devtools","dtplyr","rgbif","raster",
              "sp","sperich","spocc","tidyverse",
              "ecoengine", "RCurl","stringi",
              "rvertnet", "httr","tidyr",
              "rbison","rebird","taxize",
              "readr","rgdal","XML", 
              "stringr","rvest","robis","ridigbio",
              "R.utils","taxizesoap","future", "parallel", "doSNOW",
              "SSOAP","XMLSchema","data.table","spThin",
              "maxnet","ENMTools")
.packages <- (c("ENMTools"))

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org', dependencies = TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

installAll() #needed packages for sdm

#' clean up the space

rm(list=ls())

## @knitr organizeconapesca
#' ~/Dropbox/
datafiles='~/Dropbox/Conapesca_species'
workpath='~/Dropbox/Conapesca_species'

#datafiles='E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Data/CONAPESCA/Produccion_Pesquera_2006_2014'
#workpath='E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Data/CONAPESCA'


setwd(workpath)
nombres_validos = fread("nombres_cientificos_sinonimias.csv",header= TRUE, select = 1:3, encoding = "UTF-8") %>% 
  tbl_df %>% setNames(c("NOMBRE_COMUN","NOMBRE_VALIDO","SINONIMIA")) 

nombres_comunes = fread("nombres_cientificos_sinonimias.csv",header= TRUE, select = 1:3, encoding = "UTF-8") %>% 
  tbl_df %>% setNames(c("NOMBRE_COMUN","NOMBRE_VALIDO","SINONIMIA")) %>% .$NOMBRE_COMUN %>% unique

setwd(datafiles)
files <- list.files(pattern = "*.csv$")


conapesca.arrange <- function(x){
  fread(x, header= TRUE) %>% tbl_df %>% 
    setNames(c("ENTIDAD_NO", "ENTIDAD", "OFICINA", "OFICINA_NO", "MES", "DESCRIPCION", "NOMBRE_COMUN", "PESO_VIVO", "PESO_DESEMBARCADO", "VALOR", "NOMBRE_PRINCIPAL", "FAMILIA", "NOMBRE_CIENTIFICO", "YR")) %>% 
    left_join(nombres_validos, by="NOMBRE_COMUN") 
}

conapesca.data <- lapply(files, function (x) conapesca.arrange(x)) %>% 
  rbindlist (fill=TRUE) %>% 
  tbl_df

setwd(workpath)
write.csv(conapesca.data, file="Conapesca_produccion_2006_2014.csv")

####################################

## @knitr checktaxonomy

rm(list=ls())

datafiles="~/Dropbox/Conapesca_species/Data"
datapath="~/Dropbox/Conapesca_species" #put path
workpath="~/Dropbox/Conapesca_species"

setwd(datapath)

valid_species <- fread("nombres_cientificos_sinonimias.csv",header= TRUE, select = 1:3) %>% 
  tbl_df %>% setNames(c("common_name","valid_name","syn_name")) %>% 
  dplyr::select(valid_name) %>% 
  mutate(valid_name=ifelse(valid_name=="Genyonemus l\xadneatus","Genyonemus lineatus",valid_name)) %>% 
  mutate(valid_name=ifelse(valid_name=="Lepidoseus viridis","Lepisosteus viridis",valid_name)) %>% 
  mutate(valid_name=ifelse(valid_name=="Xifias velifer","Xiphias velifer",valid_name)) %>% 
  .$valid_name %>% 
  na.omit %>%  
  trimws() %>% 
  unique() %>% 
  as.character() %>% 
  sort()
  

#first check taxonomic validity of names reviewed manually

name_list <- list()
oldlength <- length(valid_species)
counter <- 1

# gave preference to WORMS but also got names from ITIS

names_function <-  function(){
  
  while(TRUE){
    
    name_match <- valid_species[counter] %>% 
      trimws()
    
    print(paste("Analyzing",name_match,counter,":",oldlength))
    
    worms.value <- future({
      
      
      worms.name <- try(worms_records(scientific= name_match))
      
      
      if(nrow(worms.name)!=0){
        
        worms.name  %>% tbl_df %>% 
          filter(scientificname==name_match) %>% 
          distinct(valid_name)%>% 
          mutate(name = name_match) %>% 
          mutate(match= TRUE) %>% 
          mutate(counter=counter) %>% 
          mutate(source="WORMS")
        
      } else {
        
        matrix(0,1,1) %>% 
          data.frame %>% tbl_df %>% 
          setNames("valid_name") %>% 
          mutate(valid_name = name_match) %>% 
          mutate(name = name_match) %>% 
          mutate(match= FALSE) %>% 
          mutate(counter=counter)%>% 
          mutate(source="WORMS")
      }
    }) %plan% multiprocess
    
    
    itis.value <- future({
      
      
      itis.name <- try(gnr_resolve(names = name_match, canonical=TRUE, best_match_only = TRUE))#, preferred_data_sources = c(3,155,4,9,1,12,167,163,175,173,174,165,163))
      
      
      if(nrow(itis.name)!=0){
        
        itis.name  %>% tbl_df %>% 
          group_by(submitted_name, matched_name2, data_source_title,score) %>% 
          filter(score==max(score)) %>% 
          ungroup %>% 
          distinct(matched_name2)%>% 
          setNames("valid_name") %>% 
          mutate(name = name_match) %>% 
          mutate(match= TRUE) %>% 
          mutate(counter=counter) %>% 
          mutate(source="ITIS")
        
      } else {
        
        matrix(0,1,1) %>% 
          data.frame %>% tbl_df %>% 
          setNames("valid_name") %>% 
          mutate(valid_name = name_match) %>% 
          mutate(name = name_match) %>% 
          mutate(match= FALSE) %>% 
          mutate(counter=counter)%>% 
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
    
    
    accepted.name <-value(worms.value) %>% bind_rows(value(itis.value))
    
    #accepted.name <- accepted.worms.name %>% bind_rows(accepted.itis.name)
    
    print(head(accepted.name))
    name_list[[counter]] <<- accepted.name
    
    if(counter==oldlength) stop("Completed retrieving data")
    if(counter<oldlength) counter <<- counter + 1
  }
}

names_function()

names_goc <- name_list %>% 
  rbindlist %>% 
  tbl_df %>% 
  mutate(wordnum = str_count(valid_name,"\\S+")) %>% 
  mutate(matched_name2 = ifelse(wordnum==1, name, valid_name)) %>% 
  select(name, matched_name2,source, match) %>% 
  setnames(c("old_name","valid_name","source","match")) %>% 
  distinct(old_name,valid_name,source, .keep_all=TRUE)


write.csv(names_goc, file="scientific_names_valid.csv")

itis.species <- fread("scientific_names_valid.csv",header= TRUE, encoding = "UTF-8") %>% tbl_df %>%
  filter(source=="ITIS" & match==TRUE) %>% 
  distinct(old_name, valid_name)

worms.species <- fread("scientific_names_valid.csv",header= TRUE, encoding = "UTF-8") %>% tbl_df %>% 
  filter(source=="WORMS" & match==TRUE) %>% 
  distinct(old_name, valid_name)

only.itis <- anti_join(itis.species,worms.species,by="old_name")

both.taxonomy <- semi_join(worms.species,itis.species, by="old_name")#gives preference to worms
#any database in taxize
only.worms <- anti_join(worms.species,itis.species,by="old_name")

final.taxonomy <-  bind_rows(only.worms,only.itis,both.taxonomy) %>%
  distinct(old_name, .keep_all=TRUE)


old_name2 <- final.taxonomy %>% .$old_name %>% as.character() %>% sort()


write.csv(final.taxonomy, file="final_taxonomy_valid.csv")

print(paste("Original list had",length(valid_species),"species"))
print(paste("Revised list had",nrow(final.taxonomy),"valid species"))


## @knitr getsynonyms

rm(list=ls())

datafiles="~/Dropbox/Conapesca_species/Data"
datapath="~/Dropbox/Conapesca_species" #put path
workpath="~/Dropbox/Conapesca_species"

setwd(datapath)
names_goc <- fread("final_taxonomy_valid.csv") %>% tbl_df()

valid_species_rev <- names_goc %>% 
  distinct(old_name,valid_name) %>% 
  na.omit 


eachdatabase <- "itis"
# other databases c("itis", "tropicos", "col", "nbn")
oldlength <- 1:nrow(valid_species_rev)
# Nov 4/ ITIS synonyms returned an error, then went away
# reported as Github issue


NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

synonym.data <- foreach(eachrow = oldlength, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)

  
    eachspecies <- valid_species_rev[eachrow,] %>% 
      select(valid_name) %>% 
      .$valid_name %>% 
      unique() %>% 
      as.character()
    
    eacholdname <- valid_species_rev[eachrow,] %>% 
      select(old_name) %>% 
      .$old_name %>% 
      unique() %>% 
      as.character()
    
    #use to test
    # check eachspecies <- "Salmo albus"
  
    worms.syn <- try(synonyms_s(get_wormsid(eachspecies, ask=FALSE))) 
          
    test.null.worms <- try(worms.syn %>% unlist() %>% is.null)
    
    if(!inherits(worms.syn, "try-error") & !test.null.worms==TRUE){
      
      test.na.worms <- try(worms.syn %>% unlist() %>% is.na)
      
    if(!test.na.worms==TRUE){
      
      worms.syn <- worms.syn %>% rbindlist %>% tbl_df
      
      if(nrow(worms.syn)!=0){
      
       worms.value <- worms.syn  %>%
        distinct(valid_name,scientificname) %>% 
        mutate(counter=eachrow) %>% 
        mutate(source="WORMS") %>% 
        mutate(old_name=eacholdname) %>%  
        select(old_name,valid_name,scientificname,counter,source) %>% 
        setNames(c("old_name","valid_name","syn_name","counter","source"))
    
      } 
    }
                 }
    
    if (!exists("worms.value")){
      
      worms.value <- matrix(NA,1,5) %>% 
        data.frame %>% tbl_df %>% 
        setNames(c("old_name","valid_name","syn_name","counter","source")) %>% 
        mutate(old_name=eacholdname) %>% 
        mutate(valid_name=eachspecies) %>% 
        mutate(syn_name=eachspecies) %>% 
        mutate(counter=eachrow) %>% 
        mutate(source="WORMS")
      
    
    }
    
    
    itis.syn <- try(taxize::synonyms(get_tsn(eachspecies, ask =FALSE), db = eachdatabase))
    
    test.null.itis <- itis.syn %>% unlist() %>% is.na
    test.na <-  itis.syn[[1]] %>% is.na()
    
    if(!inherits(itis.syn, "try-error") & any(!test.null.itis==TRUE) & any(test.na!=TRUE) & !grepl("no syns found",itis.syn)){
          
          syn.all <- itis.syn %>% rbindlist
          
          if("syn_name" %in% colnames(syn.all)){
            
            if("acc_name" %in% colnames(syn.all)){
              
              itis.value <-  syn.all %>% tbl_df %>%
                dplyr::select(acc_name, syn_name) %>% 
                mutate(counter=eachrow) %>% 
                mutate(source="ITIS") %>% 
                mutate(old_name=eacholdname) %>%  
                select(old_name,acc_name,syn_name,counter,source) %>% 
                setNames(c("old_name","valid_name","syn_name","counter","source"))
              
            } else {
              
              itis.value <- syn.all %>% tbl_df %>% 
                mutate(acc_name=eachspecies) %>% 
                dplyr::select(acc_name, syn_name) %>% 
                mutate(counter=eachrow) %>% 
                mutate(source="ITIS") %>% 
                mutate(old_name=eacholdname) %>%  
                select(old_name,acc_name,syn_name,counter,source) %>% 
                setNames(c("old_name","valid_name","syn_name","counter","source"))
            }
          }
          
    } else {
      
      itis.value <- matrix(1,1,5) %>% 
        data.frame %>% tbl_df %>% 
        setNames(c("old_name","valid_name","syn_name","counter","source")) %>% 
        mutate(old_name=eacholdname) %>% 
        mutate(valid_name=eachspecies) %>% 
        mutate(syn_name=eachspecies) %>% 
        mutate(counter=eachrow) %>% 
        mutate(source="ITIS")
    }
          
      
    syn.tbl <- worms.value %>% bind_rows(itis.value)
    
     #syn.tbl <- itis.syn.name %>% bind_rows(worms.syn.name)
      return(syn.tbl)
 }

stopCluster(cl)

syn_rev <- synonym.data %>% rbindlist %>%   
  na.omit %>% tbl_df %>% 
  distinct(syn_name, valid_name, .keep_all=TRUE) 

print(head(syn_rev))

write.csv (syn_rev, "species_synonyms_all.csv")


####################################
## @knitr getbiodiversity

rm(list=ls())

datafiles="~/Dropbox/Conapesca_species/Data"
datapath="~/Dropbox/Conapesca_species" #put path
workpath="~/Dropbox/Conapesca_species"

#set polygon area for record retrieval
areapolygon <- "POLYGON((-130.00 40.00, -50.00 40.00, -50.00 -10.00, -130.00 -10.00, -130.00 40.00))"

setwd(datapath)

nombres_validos = fread("species_synonyms_all.csv",header= TRUE, encoding = "UTF-8") %>% 
  tbl_df

valid_species <- nombres_validos  %>% 
  tbl_df %>% 
  dplyr::select(valid_name) %>% 
  .$valid_name %>% 
  na.omit %>%  
  unique() %>% 
  as.character()

syn_species <- nombres_validos %>% 
  tbl_df %>% 
  dplyr::select(syn_name)%>% 
  .$syn_name %>% 
  na.omit %>%  
  unique() %>% 
  as.character()

old_species <- nombres_validos %>% 
  tbl_df %>% 
  dplyr::select(old_name)%>% 
  .$old_name %>% 
  na.omit %>%  
  unique() %>% 
  as.character()


species_list <- c(valid_species,syn_species, old_species) %>% unique()

species_list <- species_list[species_list != ""]

#datafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Data/Ocurrencia_especies"
#datapath="E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Analysis" #put path
#workpath="E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Data/CONAPESCA"

site_list = c(165,132,9,239,10,259,122,380,144,145) # 144 Gulf of Mexico, 132 California Current, 145 Caribbean
region_list = c("Gulf of California","California Current", "Pacific Ocean","Pacific Central","Artic","Antartic","Gulf of Alaska","Australian","Gulf of Mexico","Caribbean")
print("Now querying Fishbase")

mirror <- "http://www.fishbase.de" # "http://www.fishbase.org"

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)


fishbase.data <- foreach(this.site = site_list, .verbose = T) %dopar% {
  
.packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
              "fields","data.table","rgbif","raster", "rasterVis",
              "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
              "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
              "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')

# Load packages into session 
lapply(.packages, require, character.only=TRUE)
     
      fishbase.data <- this.site %>% 
      paste(mirror,"/map/EcosystemOccurrencesList.php?e_code=",.,sep="") %>% 
      getURL() %>% readHTMLTable()
    
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

stopCluster(cl)

biodiversity.sp <- fishbase.data %>% 
  rbindlist %>% 
  setnames(c("species","lon","lat","source")) %>% 
  distinct(species, lat, lon, source) %>% 
  mutate_each(funs(as.numeric),lat,lon) %>% 
  na.omit 

setwd(datapath)
print(paste("Fishbase records retreived:", nrow(biodiversity.sp),sep= " "))
write.csv(biodiversity.sp,file="record_queries_fishbase.csv")

#' Robertson
# fish of the Pacific

#start clusters
NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)


species_num <- 1:2500
mirror <- "http://biogeodb.stri.si.edu/sftep/es/thefishes/speciesreport/"


robertson.data <- foreach(this.num = species_num, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)

    url <- paste(mirror,this.num,sep="")
    
    doc_html <- try(read_html(url))
    
    if(!inherits(doc_html, "try-error")){
      sp_name <- doc_html %>% 
        html_nodes("b") %>%
        html_text() %>% 
        strsplit("-") %>% unlist %>% .[2] %>% stri_trim()
      
      lat_lon_table <- doc_html %>% 
        html_nodes("td:nth-child(1) table") %>%
        html_table() %>% 
        as.data.frame() %>% 
        dplyr::select(X3,X2) %>% 
        setNames(c("lon","lat")) %>% 
        filter(!((lon == "Puntos" & lat =="Puntos")|(lon == "Long" & lat == "Lat"))) %>% 
        mutate(species=sp_name)
      
      return(lat_lon_table)
      
    }
}
    
    
stopCluster(cl)
 
#' print records

robertson.data.null <- robertson.data[!sapply(robertson.data, is.null)]

biodiversity.sp <- robertson.data.null %>% 
  rbindlist %>% 
  mutate(source="robertson") %>% 
  select(species,lon,lat,source) %>% 
  distinct(species, lat, lon, source) %>% 
  mutate_each(funs(as.numeric),lat,lon) %>% 
  na.omit 


setwd(datapath)
print(paste("Robertson records retreived:", nrow(biodiversity.sp),sep= " "))
write.csv(biodiversity.sp,file="record_queries_robertson.csv")

#' iDigBio


#start clusters
NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

#counter <- 1
#biodiversity  <-  list()
#oldlength <- length(species_list)

idigbio.data <- foreach(eachspecies = species_list, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
   
    rec_count <- try(idig_count_records(rq=list(scientificname=eachspecies, geopoint=list(type="exists")), fields=c("scientificname", "geopoint")))
    
    
    df1 <- try(idig_search_records(rq=list(scientificname=eachspecies, geopoint=list(type="exists")), fields=c("scientificname", "geopoint")))
    
    
    if(!inherits(df1, "try-error")){
      
      if(nrow(df1)!=0){
        
        biodiversity.sp <- df1 %>% 
          dplyr::select(scientificname,geopoint.lon,geopoint.lat) %>% 
          tbl_df %>% 
          mutate(source = "idigbio") %>% 
          setNames(c("species","lon","lat","source"))%>% 
          mutate(species = capitalize(species))%>% 
          distinct(species, lat, lon, source) %>% 
          na.omit
        
        return(biodiversity.sp)
 
        
      }      
    }
  
}


stopCluster(cl)

#' print records

biodiversity.sp <- idigbio.data %>% 
  rbindlist %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit


print(paste("IDigBio records retreived:", nrow(biodiversity.sp),sep= " "))
write.csv(biodiversity.sp,file="record_queries_idigibio.csv", row.names = FALSE)

#' NOTE ECOBIRDS CURRENTLY COMMENTED OUT BECAUSE I WAS LOOKING FOR ONLY FISH AND INVERTS
#' 
#' species to query instead of all records
#' valid names and synonyms


this.source = 'bison'
NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

#counter <- 1
#biodiversity  <-  list()
#oldlength <- length(species_list)

bison.data <- foreach(eachspecies = species_list, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)

      spocc.data  <-  bison(species = eachspecies, type = "scientific_name", aoi=areapolygon, config=verbose())
     # spocc.data  <-  bison(species = this.species, type = "scientific_name")
      
      spocc.data.source  <-  spocc.data$points %>% 
        data.frame %>% 
        tbl_df 
      
     if(nrow(spocc.data.source)!=0) {
        
        biodiversity.clean <- spocc.data.source %>% tbl_df %>%  
          mutate(source = this.source) %>% 
          dplyr::select(name,decimalLongitude,decimalLatitude,source) %>% 
          na.omit %>% 
          setnames(c("species","lon","lat","source")) %>% 
          distinct(species, lat, lon, .keep_all=TRUE)
          
        
        return(biodiversity.clean)
        
     
      } 
      
    } 
    

stopCluster(cl)

biodiversity.sp <- bison.data %>% 
  rbindlist %>% 
  setnames(c("species","lon","lat","source")) %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit

print(head(biodiversity.sp))

setwd(datapath)
print(paste("Retrieved Bison",nrow(biodiversity.sp)))
write.csv(biodiversity.sp, file="record_queries_bison.csv")

#extract data gbif using their API----
#' gbif needs list of species without the synonyms
#' because it takes species and retrieves key numbers which include synonyms


#gbif finds taxon by key, synonyms are included in each key


NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

gbif.data <- foreach(eachspecies = species_list, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  this.key <- name_backbone(name=eachspecies)$speciesKey %>% unlist
  
    
# gbif.goc = try(occ_search(taxonKey=this.key, return='data',fields=c("name","decimalLatitude","decimalLongitude","datasetName","collectionCode"),limit=200000, callopts=verbose())) 

  gbif.goc <- try(occ_search(taxonKey=this.key, geometry=areapolygon, return='data',fields=c("name","decimalLatitude","decimalLongitude","datasetName","collectionCode"),hasGeospatialIssue=FALSE, limit=200000,callopts=verbose()))
    
  if(!gbif.goc[1]=="no data found, try a different search" & any(grepl("decimalLongitude",colnames(gbif.goc)))==TRUE){
        
        gbif.goc.clean  <-  gbif.goc %>% 
          tbl_df %>% 
          mutate(source = "gbif") %>% 
          dplyr::select(name, decimalLongitude, decimalLatitude,source) %>% 
          setnames(c("species","lon","lat","source")) %>% 
          distinct(species, lat, lon, source) %>% 
          na.omit #eliminate rows with NA
        
       return(gbif.goc.clean)
        
      }
    }
    

stopCluster(cl)

biodiversity.sp <- gbif.data %>% 
  rbindlist %>% 
  setnames(c("species","lon","lat","source")) %>% 
  distinct(species, lat, lon, source) %>%
  na.omit

setwd(datapath)
#write table
print(paste("Retrieved GBIF",nrow(biodiversity.sp)))
write.csv(biodiversity.sp, file="record_queries_gbif.csv")



#extract data from Berkley ecoengine

#add source
this.source = "ecoengine"

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

ecoengine.data <- foreach(eachspecies = species_list, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  ee.data  <-  tryCatch(ee_observations(page_size=10000,scientific_name=eachspecies, georeferenced = TRUE),error=function(e) as.character()) 
  
    if(length(ee.data)!=0){
      
      ee.data.frame  <-  ee.data %>% 
        .$data %>%
        tbl_df %>% 
        mutate(source = this.source) %>% 
        dplyr::select(scientific_name, longitude, latitude, source) %>% 
        setnames(c("species","lon","lat","source")) %>% 
        #remove duplicates based on the combination of latitude, longitude and species
        distinct(species, lat, lon, source) %>% 
        na.omit #eliminate rows with NA
      
      return(ee.data.frame)
     
    }

  }
  

stopCluster(cl)

biodiversity.sp <- ecoengine.data %>% 
  rbindlist %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit

setwd(datapath)
#write table
print(paste("Retrieved Ecoengine",nrow(biodiversity.sp)))
write.csv(biodiversity.sp, file="record_queries_ecoengine.csv")


#extract data from OBIS ecoengine

setwd(datafiles)#switch directory

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

obis.data <- foreach(eachspecies = species_list, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  

    obis.data.table  <- tryCatch(occurrence(eachspecies, geometry =areapolygon, fields=c("species","decimalLongitude", "decimalLatitude"),verbose=TRUE),error=function(e) as.character()) 
    
    
    if(length(obis.data.table)!=0){
      
      obis.data.frame  <- obis.data.table %>% 
        tbl_df %>% 
        mutate(source = "obis") %>% 
        dplyr::select(species, decimalLongitude, decimalLatitude, source) %>% 
        setnames(c("species","lon","lat","source")) %>% 
        #remove duplicates based on the combination of latitude, longitude and species
        distinct(species, lat, lon, source) %>% 
        na.omit #eliminate rows with NA
      
      return(obis.data.frame)
      
    }
  
}

stopCluster(cl)

biodiversity.sp <- obis.data %>% 
  rbindlist %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit

setwd(datapath)
#write table
print(paste("Retrieved OBIS",nrow(biodiversity.sp)))
write.csv(biodiversity.sp, file="record_queries_obis.csv")

#' Retrieve VERTNET files

setwd(datapath)

this.source = "vertnet"
NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

vertnet.data <- foreach(eachspecies = species_list, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  
  genus =  eachspecies %>% 
    strsplit(" ") %>% 
    unlist %>% 
    .[1]
  
  species =  eachspecies %>% 
    strsplit(" ") %>% 
    unlist %>% 
    .[2]
  
  vertnet.call <- try(searchbyterm(genus = genus, specificepithet = species, limit = 100000, mappable=TRUE))
  
  test.res = is.null(vertnet.call)
  
  if(!inherits(vertnet.call, "try-error") & test.res==FALSE) {
    
    vertnet.data <- as.data.frame(vertnet.call$data)
    
    if("decimallongitude" %in% colnames(vertnet.data) & "decimallatitude" %in% colnames(vertnet.data) & "scientificname" %in% colnames(vertnet.data)){
      
      vertnet.records <- vertnet.data %>% dplyr::select(scientificname,decimallatitude,decimallongitude) %>% 
        setNames(c("species", "lat", "lon")) %>% 
        distinct(species, lat, lon) %>% 
        na.omit %>% 
        mutate(source=this.source)
      
      return(vertnet.records)
      
    }
  }
}

stopCluster(cl) 

biodiversity.sp <- vertnet.data %>% rbindlist %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit

print(paste("Retrieved Vertnet",nrow(biodiversity.sp)))
write.csv(biodiversity.sp, file="record_queries_vertnet.csv",row.names=FALSE)

#' Read excel files from UABCS, Pangas, etc.
setwd(datafiles)
xls_files <- list.files(pattern = "\\.xlsx$")# list files
counter <- 1
biodiversity <- list()
oldlength <- length(xls_files)


#loop to read in data and obtain GOC data
xls.function <- function(x) {
  while(TRUE)
    
  {
    
    eachfile <- xls_files[counter]
    
    print(paste("Analyzing"," file",eachfile,counter,"",oldlength))
    
    df  <-  read_excel(eachfile, sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0)
    indx.sp= grep("species|Species|Especie|Nombre|especie|nombre|scientificName",colnames(df))
    indx.fuen  <- grep('source|Source|Fuente|fuente|informacion|Base|Inst',colnames(df))
    indx.lon <- grep('Longitude|Longitud|longitud|Lon|longitude',colnames(df))
    indx.lat  <- grep('Latitude|Latitud|latitud|Latutud|Lat|latitude',colnames(df))
    
    xls.data = df[,c(indx.sp,indx.lon,indx.lat,indx.fuen)]
    
    
    xls.data <- xls.data %>% 
      setnames(c("species","lon","lat","source")) %>% 
      mutate(species=trimws(species)) %>%
      distinct(species, lat, lon, source) %>% 
      na.omit
    
    str(xls.data)
    biodiversity[[eachfile]] <<- xls.data
    
    if(counter==oldlength) stop("Completed retrieving data")
    if(counter<oldlength) counter <<- counter + 1
    
  }
}
# end file

xls.function()

biodiversity.sp <- biodiversity %>% 
  rbindlist %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit

setwd(datapath)
#write table
print(paste("Retrieved XLS",nrow(biodiversity.sp)))
write.csv(biodiversity.sp, file="record_queries_xls.csv")

#' Get shark and seagrass data files from Ulloa et al. 2006
# these were the only groups with species-level data
setwd(datafiles)
csv_files <- list.files(pattern = "^ulloa*")# list files
counter <- 1
biodiversity <- list()
oldlength <- length(csv_files)


#loop to read in data and obtain GOC data
ulloa.function <- function(x) {
  while(TRUE)
    
  {
    eachfile <- csv_files[counter]
    print(paste("Analyzing"," file",eachfile))
    
    ulloa.data  <-  fread(eachfile, header=TRUE) %>% 
      tbl_df %>% 
      dplyr::select(NOM_CIEN, LONGITUD, LATITUD) %>% 
      mutate(source="ulloa") %>% 
      setnames(c("species","lon","lat","source")) %>% 
      mutate(species=trimws(species)) %>%
      distinct(species, lat, lon, source) %>% 
      na.omit
    
    str(ulloa.data)
    
    biodiversity[[eachfile]] <<- ulloa.data
    
    if(counter==oldlength) stop("Completed retrieving data")
    if(counter<oldlength) counter <<- counter + 1
    
  } # end file
}

ulloa.function()

biodiversity.sp <- biodiversity %>% 
  rbindlist %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit

setwd(datapath)
#write table
print(paste("Retrieved CSV files",nrow(biodiversity.sp)))
write.csv(biodiversity.sp, file="record_queries_csv.csv",row.names=FALSE)

#' CONABIO data
# these were the only groups with species-level data
setwd(datafiles)

biodiversity.sp <- list.files(pattern = glob2rx("SNIB*.txt"))%>% 
  fread(header=FALSE) %>% 
  tbl_df %>% 
  dplyr::select(V5, V6, V7, V8) %>% 
  mutate(source="conabio") %>% 
  mutate(species=paste(V5,V6,sep=" ")) %>% 
  select(species,V7,V8,source) %>% 
  setnames(c("species","lon","lat","source")) %>% 
  distinct(species, lon, lat, source) %>% 
  na.omit

setwd(datapath)
#write table
print(paste("Retrieved Conabio",nrow(biodiversity.sp)))
write.csv(biodiversity.sp, file="record_queries_txt.csv",row.names=FALSE)

## @knitr combinebiodiversity
#' combines all record queries in one file
rm(list=ls())    

savepath="~/Dropbox/Conapesca_species" #put path

# workpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/RCode"

setwd(savepath)
record.files <- list.files(pattern = "record_queries*")# list files

counter <- 1
oldlength <- length(record.files)
biodiversity.rec <- list()

record_function <- function(x){
  
  while(TRUE){
    
    eachfile <- record.files[counter]
    
    biodiversity.file <- fread(eachfile, header=TRUE, select=c('species', 'lat', 'lon','source')) %>% 
      tbl_df %>% 
      mutate(species=trimws(species)) %>%
      distinct(species, lat, lon, source) %>%
      na.omit %>%
      mutate(file=eachfile)
    
    print(paste(nrow(biodiversity.file),"records retrieved", eachfile))
    
    str(biodiversity.file)
    
    biodiversity.rec[[counter]] <<- biodiversity.file
    
    if(counter==oldlength) stop("Completed retrieving data")
    if(counter<oldlength) counter <<- counter + 1
    
    
  }
}

record_function()

biodiversity.all <- biodiversity.rec %>% 
  rbindlist %>%   
  distinct(species, lat, lon, .keep_all=TRUE) %>% tbl_df %>% 
  mutate_each(funs(as.numeric),lat,lon) 

print(paste("Total Records retreived:", nrow(biodiversity.all),sep= " "))

setwd(savepath)
write.csv(biodiversity.all,"record_queries_combined.csv")

## @knitr selectspecies
#' subset only target species and changes synonyms to valid names

rm(list=ls())    

recordpath="~/Dropbox/Conapesca_species/species_ocurrence"
workpath="~/Dropbox/Conapesca_species" #put path
shapepath="~/Dropbox/Conapesca_species"

setwd(workpath)

biodiversity.goc <- fread("record_queries_combined.csv", header = TRUE) %>% tbl_df %>% 
  select(species,lat,lon,source) %>% 
  rename(old_name=species)

itis.species <- fread("scientific_names_valid.csv",header= TRUE, encoding = "UTF-8") %>% tbl_df %>%
  filter(source=="ITIS") %>% 
  filter(match==TRUE) %>% 
  distinct(old_name, valid_name)

worms.species <- fread("scientific_names_valid.csv",header= TRUE, encoding = "UTF-8") %>% tbl_df %>% 
  filter(source=="WORMS") %>% 
  filter(match==TRUE) %>% 
  distinct(old_name, valid_name)

only.itis <- anti_join(itis.species,worms.species,by="old_name")

both.taxonomy <- semi_join(itis.species,worms.species,by="old_name")#gives preference to itis,which is really
#any database in taxize
only.worms <- anti_join(worms.species,itis.species,by="old_name")

final.taxonomy <-  bind_rows(only.worms,only.itis,both.taxonomy) %>%
  distinct(old_name, .keep_all=TRUE) %>% 
  mutate(wordnum = str_count(valid_name,"\\S+")) %>% 
  mutate(valid_name = ifelse(wordnum==1, NA, valid_name)) %>% 
  filter(!valid_name=="NA") %>% 
  na.omit
  
syn.species <- fread("species_synonyms_all.csv",header= TRUE, encoding = "UTF-8") %>% tbl_df %>% 
  distinct(old_name,syn_name) %>% select(old_name,syn_name) %>% left_join(final.taxonomy,by="old_name")

temp.1 <- syn.species %>% select(old_name,valid_name)

species.list <- syn.species %>% select(syn_name,valid_name) %>% 
  setNames(c("old_name","valid_name")) %>% 
  bind_rows(temp.1) %>% 
  mutate(valid_name=ifelse(valid_name=="Zostera (Zostera) marina","Zostera marina", valid_name)) %>% 
  mutate(valid_name=ifelse(valid_name=="Rhizophora racemosa","Rhizophora mangle", valid_name)) %>% 
  distinct(old_name,valid_name) 


pacific.records <- biodiversity.goc %>% 
  inner_join(.,species.list,by="old_name") %>% 
  dplyr::select(-old_name) %>% 
  distinct(valid_name, lat, lon, .keep_all=TRUE)

setwd(workpath)

print(str(pacific.records))
write.csv(pacific.records, file="CONAPESCA_ocurrencias_reg.csv")



## @knitr organizebiodiversity
#' subset only data in the Pacific Ocean
#' iterates on sets of data otherwise insufficient memory
#' create new data frame for clean records

rm(list=ls())    

recordpath="~/Dropbox/Conapesca_species/species_ocurrence"
workpath="~/Dropbox/Conapesca_species" #put path
shapepath="~/Dropbox/Conapesca_species"

# workpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/RCode"
# shapepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/SIG_Biodiversity"
# savepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis"
# ulloafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Datos/Ulloa_datos" #put path
# datafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Datos/Ocurrencia_especies"

# projections
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

setwd(shapepath)

#read in Eastern Pacific shapefile
goc.shape <- readOGR(".", "pacific_ocean") 

setwd(workpath)
biodiversity.all <- fread("CONAPESCA_ocurrencias_reg.csv")


biodiversity.row  <-  1:nrow(biodiversity.all)

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

point.data <- foreach(rownum = biodiversity.row, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  
  this.row <- biodiversity.all[rownum,] %>% 
    na.omit %>% 
    mutate_each(funs(as.numeric),lat,lon) %>% 
    as.data.frame
  
  if(nrow(this.row)!=0){
    
    if(this.row$lat==-99 & this.row$lon==-99 | this.row$lat>=180 | this.row$lat<=-180){
      
      new.lon <- 180 
      new.lat <- 90
      
      this.row$lat <- new.lat
      this.row$lon <- new.lon
      
    } 
    
    if(this.row$lat>90 | this.row$lat<=-90){
      
      old.lat <- this.row$lat
      new.lat <- this.row$lon
      
      this.row$lat <- new.lat
      this.row$lon <- old.lat
    } 
    
    if(this.row$lon<=-180) { 
      
     new.lon <- -180 
      this.row$lon <- new.lon
      
    }
    
    
    coordinates(this.row) <- c("lon", "lat")  # set spatial coordinates
    proj4string(this.row) <- crs.geo.wgs  # define projection system of our data
    
    # subset ocurrence points within GOC
    #get table from shapefile
    biodiversity.sec <- this.row[goc.shape, ] %>% as("data.frame")
    test.bio  <- nrow(biodiversity.sec)==0
    
    if (test.bio==FALSE)
    {
      
      biodiversity.clean <- biodiversity.sec %>% 
        distinct(valid_name, lat, lon) %>% 
        na.omit
      
      return(biodiversity.clean)
      
      
    }
   
  } # end if this row !0
  
}
    
stopCluster(cl)

biodiversity.goc <- point.data %>% 
  rbindlist %>%   
  distinct(valid_name, lat, lon) %>% 
  na.omit

qmplot(lon, lat, data = biodiversity.goc, maptype = "satellite", color = I("white"), source = "google",extent = "panel", zoom = 3)+
  #    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  labs(x = 'Longitude', y = 'Latitude')+
  theme(axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))

setwd(workpath)

write.csv(biodiversity.goc,"goc_species_ocurrence.csv")
print(paste("FINAL GOC records retreived:", nrow(biodiversity.goc),sep= " "))


## @knitr countspecies
#'Separate species with more than 20 records for SDMs and generate maps

rm(list=ls())    

recordpath="~/Dropbox/Conapesca_species/species_ocurrence"
workpath="~/Dropbox/Conapesca_species" #put path
savepath="~/Dropbox/Conapesca_species/SDM"

setwd(workpath)
pacific.records <- fread("goc_species_ocurrence.csv") %>% 
  rename(species=valid_name)


print(paste("FINAL GOC records retreived:", nrow(pacific.records),sep= " "))
print(paste("FINAL GOC species:", length(unique(pacific.records$species)),sep= " "))

counter <- 1

species.list <- pacific.records %>% select(species) %>% .$species %>% as.character() %>% unique()

clean.list <- list()
oldlength <- length(species.list)

count.species <- function(x){
  
  while(TRUE){
    
    this.species <- species.list[counter]
    print(paste("Analyzing",this.species,counter,":",oldlength))
    
    these.records <- pacific.records %>% 
      filter(species==this.species)
    
    clean.records <- these.records %>% 
      mutate(num_rows=nrow(these.records)) %>% 
      as.data.frame()
    
    clean.list[[counter]] <<- clean.records 
    
    if(counter==oldlength) stop("Completed retrieving data")
    if(counter<oldlength) counter <<- counter + 1
  }  
}

count.species()


biodiversity.goc <- clean.list %>% 
  rbindlist %>%   
  distinct(species, lat, lon, .keep_all=TRUE) %>% 
  na.omit

goc.records.30sp <-  biodiversity.goc %>% filter(num_rows>=30) %>% 
  select(species,lat,lon, num_rows) %>% 
  mutate(species=as.factor(species)) %>% tbl_df

str(goc.records.30sp)

goc.records.20sp <-  biodiversity.goc %>% filter(num_rows>=20) %>% 
  select(species,lat,lon, num_rows) %>% 
  mutate(species=as.factor(species)) %>% tbl_df

str(goc.records.20sp)

write.csv(goc.records.30sp, file="CONAPESCA_sp_30reg.csv")
write.csv(goc.records.20sp, file="CONAPESCA_sp_20reg.csv")

#set directory path

setwd(savepath)

qmplot(lon, lat, data = pacific.records, maptype = "satellite", color = I("white"), source = "google",extent = "panel", zoom = 3)+
  #    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  labs(x = 'Longitude', y = 'Latitude')+
  theme(axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))


ggsave ("conapesca_points.png", dpi = 300)

setwd(workpath)
unique.species <- goc.records.20sp %>% distinct(species) %>% .$species %>% as.character()
counter <- 1

graph.species <- function(x) {
  
  while(TRUE){
    
    eachspecies <- unique.species[counter]
    
    print(paste("generating map for",eachspecies))
    
    this.data <- goc.records.20sp %>% 
      filter(species==eachspecies)
    
    qmplot(lon, lat, data = this.data, maptype = "satellite", color = I("white"), source = "google",extent = "panel", zoom=3)+
      #    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
      labs(x = 'Longitude', y = 'Latitude')+
      theme(axis.title.x = element_text(size=13),
            axis.title.y = element_text(size=13, angle=90),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10))+
      ggtitle(eachspecies)
    
    setwd(savepath)
    ggsave (paste(eachspecies,"_mapa.png",sep=""), dpi = 300)
    
    if(counter>length(unique.species)) stop("Done") 
    if(counter<=length(unique.species)) counter<<- counter +1 
    
  }
}


graph.species()

###############################

## @knitr generate Maxent SDMs

rm(list=ls())    

datapath="~/Dropbox/Conapesca_species" #put path
savepath="~/Dropbox/Conapesca_species/SDM"
environpath="~/Dropbox/Conapesca_species/environ_var"

setwd(datapath)

crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

goc.20.sp <- fread("CONAPESCA_sp_20reg.csv")

sp.goc.20 <- goc.20.sp %>% 
  dplyr::select(species) %>% 
  distinct(species) %>% 
  .$species %>% as.character()

# thin dataset to correct for sampling bias
# Spatial thinning helps to reduce the effect of uneven, or biased, species occurence collections on spatial
# model outcomes.

#tried to do this parallel but couldn't get it to work

  # Load packages into session 

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

thin.data <- foreach(this.species = sp.goc.20, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "PBSmapping", "fields","dtplyr","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","wesanderson","tidyr","cowplot","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "parallel", "doSNOW",
                "SSOAP","XMLSchema","data.table","spThin","virtualspecies","dismo","sdm")

    if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  


#for(eachnumber in 1:length(sp.goc.20)){
  
 # this.species <- sp.goc.20[eachnumber]
  
  this.data <- goc.20.sp %>% filter(species==this.species) %>% select(species,lat,lon)
  
  print(paste("Analyzing species",this.species, ":",length(sp.goc.20),sep=" "))
  
  setwd(savepath)
  
  thinned_dataset_full <-
    thin( loc.data = this.data, 
          lat.col = "lat", long.col = "lon", 
          spec.col = "species", 
          thin.par = 10, reps = 100, 
          locs.thinned.list.return = TRUE, 
          write.files = TRUE, 
          max.files = 5, 
          out.dir = paste(this.species,"_full","/",sep=""), out.base = paste(this.species,"_thinned",sep=""), 
          write.log.file = TRUE,
          log.file = paste(this.species,"_thinned_full_log_file.txt",sep=""))
  #used code from https://cran.r-project.org/web/packages/MaxentVariableSelection/vignettes/MaxentVariableSelection.pdf
  
  return(thinned_dataset_full)
  
  setwd(paste(savepath,"/",this.species,"_full","/",sep=""))
  png(sprintf('thinplot_%s.png', this.species))
  
  plotThin(thinned_dataset_full, which = 2, ask=FALSE)
  dev.off()
  setwd(savepath)
}

  stopCluster(cl)

######################################################################################
## Load predictor rasters
# make raster "stack" with raster for each predictor
list.rasters<-(list.files("~/Dropbox/Conapesca_species/rasters/pacifico", full.names=T, pattern=".asc"))
list.rasters
rasters.env <- stack(list.rasters)
projection(rasters.env) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

collin.rast <- removeCollinearity(rasters.env, multicollinearity.cutoff = 0.85,
                   select.variables = TRUE, sample.points = FALSE, plot = TRUE)

rasters.selected <- subset(rasters.env, collin.rast)
###############################################################################################


setwd(savepath)

sp.dirs <- list.dirs(path=".",recursive=FALSE)
eachdir <- 1
for(eachdir in 1:length(sp.dirs)){
  
  setwd(savepath)
  setwd(sp.dirs[eachdir])
  sp.file <- list.files() %>% grepl("*5.csv",.)
  last.file <- list.files() %>% .[sp.file]
  spoints <- fread(last.file) %>% dplyr::select(lon,lat)
  this.species <- fread(last.file) %>% distinct(species) %>% .$species %>% as.character()
 
  write.csv(spoints,file=paste(this.species,"_obspoints.csv",sep=""))
  
  ## Extracting values from rasters
  presvals <- raster::extract(rasters.selected, spoints) %>% as.data.frame()
 
  spoints.occ <- rep.int(1,nrow(spoints)) %>% as.numeric
  
  
  #Background points
  
  background.points.buffer(points = ahli$presence.points,
                           radius = 20000, n = 1000, mask = env[[1]])
  
   maxnet(spoints.occ, presvals)
  
# resultado seria el mejor modelo para cada especie
# predicciones para capas futuras (modelos deben empatar capas con observadas)
# mapas de cambio distribucion futura
# comparar cambios en los pixeles del presente y el futuro como una diferencia entre presente - futuro
# obtener todos los registros de especies en la region (comerciales y no comerciales)
# aplicar indice de riqueza para obtener modelo de biodiversidad para todo el area de estudio
# en los sdms seleccionar valor de probabilidad a partir del cual se considera presente
# se normaliza (todo menor a ese valor es 0) y se suman para obtener un indice de riqueza basado en el SDM
# comparar riqueza a partir de los SDMS de especies comerciales con el indice por interpolacion tomando todas las especies
# correlacionar proyecciones a futuro en SDMS con indice de riqueza
# [esto se podria usar en un momento para correlacionar con abundancia]

# este mismo proceso se debe de hacer para el Atlantico



