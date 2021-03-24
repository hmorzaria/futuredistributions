#' Ocurrence record retrieval from CONAPESCA records
#' @author Hem Nalini Morzaria Luna
#' @date March 2016
#' organize data from excel spreadsheets
#' Last reviewed January 2019
#' catch data for 2014-2106
#' http://datos.gob.mx/

# List of packages for session

#install.packages(c("XMLSchema", "SSOAP"), repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"))
if(!require(robis)){devtools::install_github("iobis/robis"); library(robis)}
if(!require(taxizesoap)){devtools::install_github("ropensci/taxizesoap"); library(taxizesoap)}

install.packages("devtools")
library(devtools)
#install_local("/home/atlantis/ENMTools-master")
#library(ENMTools)

remotes::install_github("ropensci/rfishbase")
library("rfishbase")

#if(!require(ENMTools)){devtools::install_github("danlwarren/ENMTools"); library(ENMTools)}

.packages = c("devtools","dtplyr","rgbif","raster","readxl",
              "sp","sperich","spocc","tidyverse",
              "ecoengine", "RCurl","stringi",
              "rvertnet", "httr",
              "rbison","rebird","taxize",
              "readr","rgdal","XML", 
              "stringr","rvest","robis","ridigbio",
              "R.utils","taxizesoap","future", "parallel", "doSNOW",
              "SSOAP","XMLSchema","data.table","spThin",
              "maxnet","rJava", "sdm", "rfishbase","ggmap","virtualspecies")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org', dependencies = TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

installAll() #needed packages for sdm



#' clean up the space

rm(list=ls())


datapath <- "~/conapescaspecies/catchdata"
workpath <- "~/conapescaspecies" #put path
datafiles <- "~/conapescaspecies/Data"

setwd(workpath)

conapesca.sp <- fread("Atlas_CICIMAR_Ramirez_rev.csv",encoding = "Latin-1") %>% 
  as_tibble %>% 
  dplyr::select(common_name, species) %>% 
  setNames(c("nombre_comun","nombre_cientifico")) %>% 
  filter(!is.na(nombre_cientifico)) %>% 
  mutate(nombre_comun=tolower(nombre_comun), nombre_cientifico=tolower(nombre_cientifico)) %>% 
  mutate(nombre_comun=trimws(nombre_comun),nombre_cientifico=trimws(nombre_cientifico)) %>% 
  distinct(nombre_comun,nombre_cientifico)
  


small.scale <- list.files(path = datapath, pattern="CONSULTA *.*", full.names = TRUE)



get_species <- function(this.sheet){
  
  print(this.sheet)
  
  this.data <- fread(this.sheet, header = T, encoding = "Latin-1") %>% 
    as_tibble()
  
 these.names <- this.data %>% 
    dplyr::select(contains("COMUN"),contains("CIENTIFICO")) %>% 
    setNames(c("nombre_comun","nombre_cientifico")) %>% 
    distinct(nombre_comun,nombre_cientifico)
 
 return(these.names)
}


get.species2 <- function(this.sheet){
  
  print(this.sheet)
  
  this.data <- read_xlsx("CONAPESCA_IFAI_Pacifico.xlsx", sheet=this.sheet)
  
  these.names <- this.data %>% 
    dplyr::select(contains("NOMBRE COM")) %>% 
    setNames("nombre_comun") %>% 
    filter(!grepl("DE CULTIVO|RANA ",nombre_comun)) %>% 
    filter(!grepl("DE AGUA DULCE",nombre_comun)) %>% 
    filter(!grepl(" SINTETICO| DE VIVERO",nombre_comun)) %>% 
    mutate(nombre_comun_rev= gsub(" S.C. FCA.| S.C. FCO.| DESV. FCO.| DESV. FCA| FILETE DE FCO.| DESV. Y DESC. FCO.| DESV. Y DESC. FCO| DESC. FCO.| ENT. COCIDA| PULPA FCA. DE| ALETA FCA.| CARNE SECA| CARNE FCA.| ENT. SECO| ALETA DE FCA.| ENT. COCIDO| DESC. Y| PIEL DE| FCO.| CONCHA DE| ALETA DE| FILETE SECO| SECA|[.]| SECO| DESC| COLAS DE FCA| MANTODE| MANTODE| FILETE DE SECO| COLAS DE| DESV DESC FCA| DESV Y DESC FC| TRIPA DE| DESV Y DESC| CARNE DE| VIVO| S/MANTO ","",nombre_comun)) %>% 
    mutate(nombre_comun_rev= gsub("TENTACULOS DE| DESC PTO| COCIDO| CARNE DE SECA| MANOS DE| DESV. DESC. FCO.| DESV. Y DESC. FCA.| DESC. FCO.| CARNE DE FCA.| DESC. SECO| ENT. FCO.| ENT. FCA.| ENTFCA| ENTFCO| VIVITA CARNADA| COCIDAS| PULPA DE FCA| DESV, DESC FCO| DESC COCIDO| CABEZA DE| PULPA FCO.| ALETA DE SECA| S/C FCO.| FILETE FCO.| PIEL DE| HUEVA DE| ENT FCA| P/CARNADA| MANTODE| DESV FCA| DESV Y FC| DESV Y| ENTFCO| DESV| S/C| FILETE| ENT| ALETA CARTON DESV. Y DESC.| TENTACULOS DE| DESV, FCO| DESV| ALETA DE| COCIDO| TUBO DE| FCO| POLVO DE| FCA| DESC PTO| FRESCO| COCIDA| AVISOS|","",nombre_comun_rev)) %>% 
    mutate(nombre_comun_rev= gsub("AA","A",nombre_comun_rev)) %>% 
    mutate(nombre_comun_rev= gsub("ROJOFCO","ROJO",nombre_comun_rev)) %>% 
    mutate(nombre_comun_rev= gsub("MERLUZAFCA","MERLUZA",nombre_comun_rev)) %>% 
    mutate(nombre_comun_rev= gsub("MANTARRAYASECA","MANTARRAYA",nombre_comun_rev)) %>% 
    #mutate(nombre_comun_rev= gsub(" \\(.*\\)", "",nombre_comun_rev)) %>% 
    distinct(nombre_comun_rev) %>% 
    setNames("nombre_comun") %>% 
    arrange(nombre_comun) 
  
  print(paste("Number of records",nrow(these.names)))
  
  return(these.names)
  
}

conapesca.species.anuario <- lapply(small.scale,get_species) %>% 
  bind_rows %>% 
  mutate(nombre_comun=trimws(nombre_comun),nombre_cientifico=trimws(nombre_cientifico)) %>% 
  distinct(nombre_comun,nombre_cientifico) %>% 
  mutate(nombre_comun=tolower(nombre_comun), nombre_cientifico=tolower(nombre_cientifico))
  
ifai.sheets <- excel_sheets("CONAPESCA_IFAI_Pacifico.xlsx") %>% 
  length %>% 
  1:.

conapesca.2016.species <- lapply(ifai.sheets,get.species2) %>% 
  bind_rows

conapesca.species.list <- conapesca.2016.species %>% 
  distinct(nombre_comun) %>% 
  left_join(conapesca.species.anuario, by="nombre_comun") %>% 
  bind_rows(conapesca.species.anuario)  %>% 
  mutate(nombre_comun= gsub("\\(","",nombre_comun)) %>%
  mutate(nombre_comun= gsub("\\)","",nombre_comun)) %>%
  mutate(nombre_comun= gsub(" ind$","",nombre_comun)) %>%
  mutate(nombre_comun= gsub(" se$","",nombre_comun)) %>%
  mutate(nombre_cientifico=if_else(nombre_cientifico %in% c(0,"#n/a"),"",nombre_cientifico)) %>% 
  mutate(nombre_cientifico=gsub(" sp$","",nombre_cientifico), nombre_cientifico=gsub(" spp$","",nombre_cientifico)) %>% 
  mutate(nombre_cientifico=gsub(" sp\\.$","",nombre_cientifico), nombre_cientifico=gsub(" spp\\.$","",nombre_cientifico)) %>%
  mutate(word_count = stri_count(nombre_cientifico,regex="\\S+")) %>% 
  mutate(nombre_cientifico=if_else(word_count == 1, NA_character_, nombre_cientifico)) %>% 
  mutate(nombre_comun=trimws(nombre_comun), nombre_comun=tolower(nombre_comun)) %>%
  distinct(nombre_comun,nombre_cientifico) 

conapesca.species.taxa <- conapesca.species.list %>%
  filter(is.na(nombre_cientifico)) %>% 
  distinct(nombre_comun) %>% 
  left_join(conapesca.species.list, by="nombre_comun") %>% 
   bind_rows(conapesca.species.list) 
  
atlas.sp <- conapesca.species.taxa %>% 
  filter(is.na(nombre_cientifico)) %>% 
  dplyr::select(nombre_comun) %>% 
  left_join(conapesca.sp, by="nombre_comun") %>% 
  distinct(nombre_comun,nombre_cientifico) 

conapesca.identified <- conapesca.species.taxa %>% 
  bind_rows(atlas.sp) %>% 
  distinct(nombre_comun,nombre_cientifico) %>% 
  filter(!is.na(nombre_cientifico))
  
missing.sp <- conapesca.species.list %>% 
  filter(is.na(nombre_cientifico)) %>% 
  distinct(nombre_comun) %>% 
  left_join(conapesca.identified, by="nombre_comun") %>% 
filter(is.na(nombre_cientifico))

list.missing <- missing.sp %>% 
  pull(nombre_comun)

find_missing <- function(eachmissing, conapesca.identified){
  
  print(eachmissing)
  
  matched.table <- conapesca.identified %>% 
    filter(grepl(eachmissing, nombre_comun)) %>% 
    filter(!is.na(nombre_cientifico)) %>% 
    mutate(nombre_original = eachmissing) %>% 
    dplyr::select(nombre_original, everything())
  
  matched.table.atlas <- conapesca.sp %>% 
    filter(grepl(eachmissing, nombre_comun)) %>% 
    filter(!is.na(nombre_cientifico)) %>% 
    mutate(nombre_original = eachmissing) %>% 
    dplyr::select(nombre_original, everything())
  
  all.matched <- matched.table %>% 
    bind_rows(matched.table.atlas) %>% 
    distinct(nombre_original,nombre_comun,nombre_cientifico)
    
  return(all.matched)
    
}

missing.id <- lapply(list.missing, find_missing, conapesca.identified) %>% 
  bind_rows
  

not.found <- missing.sp %>% 
  distinct(nombre_comun) %>% 
  dplyr::rename(nombre_original=nombre_comun) %>% 
  left_join(missing.id, by="nombre_original") %>% 
  filter(is.na(nombre_comun)) %>% 
  distinct(nombre_original) 
  
#this didn't work, responses are too general
# find_fishbase <- function(eachnotfound){
#   
#   print(eachnotfound)
#   
# sci.fishbase <- common_to_sci(eachnotfound, Language = "Spanish") %>% 
#   mutate(nombre_original=eachnotfound)
# 
# print(sci.fishbase)
# return(sci.fishbase)
# 
# }
# 
# fishbase.res <- lapply(not.found, find_fishbase) %>% 
#   bind_rows

setwd(workpath)
#this file was saved and checked manually
write_csv(missing.id,"conapesca_especies_faltantes.csv")

write_csv(conapesca.identified,"conapesca_identified.csv")

not.found  
write_csv(not.found, "conapesca_no_encontradas.csv")

#Commented Nov/2020 check to see no data is missing
# conapesca.species <- read_csv("conapesca_especies_revisar.csv") %>% 
#   mutate(nombre_cientifico = if_else(is.na(nombre_cientifico),"NO ENCONTRADO",nombre_cientifico)) %>% 
#   bind_rows(existing.sp) %>% 
#   mutate(nombre_comun = tolower(nombre_comun), nombre_cientifico = tolower(nombre_cientifico))
# 
# #this is detailed data to 2016 but has no scientific names
# ifai.sheets <- excel_sheets("CONAPESCA_IFAI_Pacifico.xlsx") %>% 
#   grep("MEN",.,value = TRUE)
# 
# conapesca.list.species <- conapesca.full.species %>% 
#   mutate(nombre_comun=tolower(nombre_comun)) %>% 
#   left_join(conapesca.species, by="nombre_comun") %>%
#   filter(!is.na(nombre_cientifico)) %>% 
#   distinct(nombre_comun,nombre_cientifico) 
# 
# write_csv(conapesca.list.species,"conapesca_lista_especies.csv")
# 
# conapesca.missing <- conapesca.full.species %>% 
#   mutate(nombre_comun=tolower(nombre_comun)) %>% 
#   left_join(conapesca.species, by="nombre_comun") %>%
#   filter(is.na(nombre_cientifico)) %>% 
#   mutate(nombre_comun= gsub(" asado| ind$","",nombre_comun)) %>% 
#   mutate(nombre_comun= gsub("alta mar","altamar",nombre_comun)) %>%
#   distinct(nombre_comun,nombre_cientifico) %>% 
#   arrange(nombre_comun)
# 
# write_csv(conapesca.missing,"conapesca_faltantes_especies.csv")
# 
# #this file has been checked manually
# 
# conapesca.list.species2 <- read_csv("conapesca_especies_revisadas.csv")
# 
# species.list <- conapesca.list.species %>% 
#   bind_rows(conapesca.list.species2) %>% 
#   filter(!is.na(nombre_cientifico)) %>% 
#   filter(nombre_cientifico!="no encontrado") %>% 
#   filter(nombre_cientifico!="") %>% 
#   filter(nombre_cientifico!="#n/a") %>% 
#   mutate(nombre_cientifico = tolower(nombre_cientifico)) %>% 
#   distinct(nombre_cientifico) %>% 
#   arrange(nombre_cientifico) %>% 
#   filter(!grepl(" spp$",nombre_cientifico)) %>% 
#   filter(!grepl(" sp.$",nombre_cientifico))
# 
# write_csv(species.list,"final_species_list_conapesca.csv")
#  
# #used to erase some strange characters manually
# 
# species.list <- read_csv("final_species_list_conapesca.csv") %>% 
#   filter(!grepl(" spp$",nombre_cientifico)) %>% 
#   filter(!grepl(" spp.$",nombre_cientifico)) %>% 
#   filter(!grepl(" sp.$",nombre_cientifico)) %>% 
#   filter(!grepl(" sp$",nombre_cientifico)) %>% 
#   mutate(wordnum = str_count(nombre_cientifico,"\\S+")) %>% 
#   filter(wordnum != 1) %>% 
#   distinct(nombre_cientifico) %>% 
#   arrange(nombre_cientifico) %>% .$nombre_cientifico



#first check taxonomic validity of names reviewed manually

source("check_taxonomy.R")

names_goc_list <- lapply(species.list,names_function)

names_goc <- names_goc_list %>% 
  bind_rows 

setwd(workpath)
write_csv(names_goc, file="scientific_names_valid.csv")

## @knitr getsynonyms

rm(list=ls())

datapath <- "~/conapescaspecies/catchdata"
workpath <- "~/conapescaspecies" #put path

setwd(workpath)

names_goc <- fread("scientific_names_valid.csv") %>% tbl_df()

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
                "stringr","rvest","robis","ridigbio","R.utils","taxizesoap")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)

  
    eachvalidanme <- valid_species_rev[eachrow,] %>% 
      select(valid_name) %>% 
      .$valid_name %>% 
      unique() %>% 
      as.character() %>% tolower()
    
    eacholdname <- valid_species_rev[eachrow,] %>% 
      select(old_name) %>% 
      .$old_name %>% 
      unique() %>% 
      as.character() %>% tolower()
    
    
    name.list <- c(eachvalidanme, eacholdname) %>% unique()
    
    #use to test
    # check eachspecies <- "abudefduf concolor"
    
    result.list <- list()
    
   for(eachname in 1:length(name.list)) {
     
     eachspecies <- name.list[eachname]
    
    worms.syn <- try(synonyms_s(get_wormsid(eachspecies, ask=FALSE))) 
          
    test.null.worms <- try(worms.syn %>% unlist() %>% is.null)
    
    if(!inherits(worms.syn, "try-error") & !test.null.worms==TRUE){
      
      test.na.worms <- try(worms.syn %>% unlist() %>% is.na)
      
    if(!any(test.na.worms==TRUE)){
      
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
    
    result.list[[eachname]] <- syn.tbl
   }
    
    synonym.results <- result.list %>% bind_rows()
    
     #syn.tbl <- itis.syn.name %>% bind_rows(worms.syn.name)
      return(synonym.results)
 }

stopCluster(cl)

syn_rev <- synonym.data %>% rbindlist %>%   
  na.omit %>% tbl_df %>% 
  mutate(valid_name=tolower(valid_name), syn_name=tolower(syn_name)) %>% 
  distinct(old_name, syn_name, valid_name, .keep_all=TRUE) 

write_csv (syn_rev, "species_synonyms_all.csv")


####################################
## @knitr getbiodiversity

rm(list=ls())

datapath <- "~/conapescaspecies/biodiverdata"
workpath <- "~/conapescaspecies" #put path

setwd(workpath)


#set polygon area for record retrieval
areapolygon <- "POLYGON((-130.00 40.00, -50.00 40.00, -50.00 -10.00, -130.00 -10.00, -130.00 40.00))"



valid.names  <- read_csv("species_synonyms_all.csv") 

valid_species <- valid.names  %>% 
  .$valid_name %>% 
  na.omit %>%  
  unique() %>% 
  as.character()

syn_species <- valid.names %>% 
  .$syn_name %>% 
  na.omit %>%  
  unique() %>% 
  as.character()

old_species <- valid.names %>% 
  .$old_name %>% 
  na.omit %>%  
  unique() %>% 
  as.character()


species_list <- c(valid_species,syn_species, old_species) %>% unique()

species_list <- species_list[species_list != ""]

species_list %>% 
  as_tibble %>% write_csv("full_species_searched.csv")

site_list = c(165,132,9,239,10,259,122,380,144,145) # 144 Gulf of Mexico, 132 California Current, 145 Caribbean
region_list = c("Gulf of California","California Current", "Pacific Ocean","Pacific Central","Artic","Antartic","Gulf of Alaska","Australian","Gulf of Mexico","Caribbean")
print("Now querying Fishbase")

mirror <- "https://www.fishbase.de" # "http://www.fishbase.org"

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

stopCluster(cl)

fishbase.sp <- fishbase.data %>% 
  bind_rows %>% 
  setnames(c("species","lon","lat","source")) %>% 
  distinct(species, lat, lon, source) %>% 
  mutate_at(vars(c(lat,lon)), as.numeric, na.rm=TRUE) %>% 
  mutate(species = tolower(species), submitted_name = species) %>% 
  dplyr::select(submitted_name, species, lat, lon, source) %>% 
  na.omit %>% 
  filter(species %in% species_list)

setwd(datapath)
print(paste("Fishbase records retreived:", nrow(fishbase.sp),sep= " "))
write_csv(fishbase.sp,file="record_queries_fishbase.csv")

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

robertson.sp <- robertson.data.null %>% 
  bind_rows %>% tbl_df %>% 
  mutate(source="robertson") %>% 
  dplyr::select(species,lon,lat,source) %>% 
  distinct(species, lat, lon, source) %>% 
  mutate_each(funs(as.numeric),lat,lon) %>% 
  mutate(species = tolower(species)) %>% 
  na.omit %>% 
#  filter(species %in% species_list) %>% 
  mutate(submitted_name = species) %>% 
  dplyr::select(submitted_name, species, lat, lon, source)


setwd(datapath)
print(paste("Robertson records retreived:", nrow(robertson.sp),sep= " "))
write_csv(robertson.sp,file="record_queries_robertson.csv")

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
          na.omit %>% 
          mutate(submitted_name = eachspecies)
        
        return(biodiversity.sp)
 
        
      }      
    }
  
}


stopCluster(cl)

#' print records

idigbio.sp <- idigbio.data %>% 
  rbindlist %>% 
  distinct(submitted_name, species, lat, lon, source) %>% 
  na.omit %>% 
  dplyr::select(submitted_name, species, lat, lon, source)


setwd(datapath)
print(paste("IDigBio records retreived:", nrow(idigbio.sp),sep= " "))
write_csv(idigbio.sp,file="record_queries_idigibio.csv", row.names = FALSE)

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

      spocc.data  <-  try(bison(species = eachspecies, type = "scientific_name", aoi=areapolygon, config=verbose()))
     # spocc.data  <-  bison(species = this.species, type = "scientific_name")
      
      if(!inherits(spocc.data, "try-error")){
        
      spocc.data.source  <-  spocc.data$points %>% 
        data.frame %>% 
        tbl_df 
      
     if(any(spocc.data.source$geo=="Yes")) {
        
        biodiversity.clean <- spocc.data.source %>% tbl_df %>%  
          mutate(source = this.source) %>% 
          dplyr::select(name,decimalLongitude,decimalLatitude,source) %>% 
          na.omit %>% 
          setnames(c("species","lon","lat","source")) %>% 
          distinct(species, lat, lon, .keep_all=TRUE) %>% 
          mutate(submitted_name = eachspecies)
          
        
        return(biodiversity.clean)
        
     
      } 
      
    } }
    

stopCluster(cl)

bison.data.null <- bison.data[!sapply(bison.data, is.null)]


bison.sp <- bison.data.null %>% 
  rbindlist %>% 
  setnames(c("species","lon","lat","source", "submitted_name")) %>% 
  distinct(submitted_name, species, lat, lon, source) %>% 
  na.omit %>% 
  dplyr::select(submitted_name, species, lat, lon, source)

setwd(datapath)
print(paste("Retrieved Bison",nrow(bison.sp)))
write_csv(bison.sp, file="record_queries_bison.csv")

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
  
  if(!is.null(this.key)) {
    
# gbif.goc = try(occ_search(taxonKey=this.key, return='data',fields=c("name","decimalLatitude","decimalLongitude","datasetName","collectionCode"),limit=200000, callopts=verbose())) 

  gbif.goc <- try(occ_search(taxonKey=this.key, geometry=areapolygon, return='data',fields=c("name","decimalLatitude","decimalLongitude","datasetName","collectionCode"),hasGeospatialIssue=FALSE, limit=200000,callopts=verbose()))
    
  if(!gbif.goc[1]=="no data found, try a different search" & any(grepl("decimalLongitude",colnames(gbif.goc)))==TRUE){
        
        gbif.goc.clean  <-  gbif.goc %>% 
          tbl_df %>% 
          mutate(source = "gbif") %>% 
          dplyr::select(name, decimalLongitude, decimalLatitude,source) %>% 
          setnames(c("species","lon","lat","source")) %>% 
          distinct(species, lat, lon, source) %>% 
          na.omit %>%  #eliminate rows with NA
          mutate(submitted_name = eachspecies)
        
       return(gbif.goc.clean)
        
      }
    }
}

stopCluster(cl)

gbif.sp <- gbif.data %>% 
  bind_rows %>% 
  distinct(submitted_name, species, lat, lon, source) %>%
  na.omit %>% 
  dplyr::select(submitted_name, species, lat, lon, source)


setwd(datapath)
#write table
print(paste("Retrieved GBIF",nrow(gbif.sp)))
write_csv(gbif.sp, file="record_queries_gbif.csv")



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
  
  ee.data  <-  tryCatch(ee_observations(page_size=10000,scientific_name=eachspecies, bbox = '-50,-10,-130,40', georeferenced = TRUE),error=function(e) as.character()) 
  
    if(length(ee.data)!=0){
      
      ee.data.frame  <-  ee.data %>% 
        .$data %>%
        tbl_df %>% 
        mutate(source = this.source) %>% 
        dplyr::select(scientific_name, longitude, latitude, source) %>% 
        setnames(c("species","lon","lat","source")) %>% 
        #remove duplicates based on the combination of latitude, longitude and species
        distinct(species, lat, lon, source) %>% 
        na.omit %>% #eliminate rows with NA 
        mutate(submitted_name = eachspecies)
      
      return(ee.data.frame)
     
    }

  }
  

stopCluster(cl)

ecoengine.sp <- ecoengine.data %>% 
  rbindlist %>% 
  distinct(submitted_name, species, lat, lon, source) %>% 
  na.omit %>% 
  dplyr::select(submitted_name, species, lat, lon, source)


setwd(datapath)
#write table
print(paste("Retrieved Ecoengine",nrow(ecoengine.sp)))
write_csv(ecoengine.sp, file="record_queries_ecoengine.csv")


#extract data from OBIS ecoengine

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)


species_list2 <- paste0(toupper(substr(species_list, 1, 1)), substr(species_list, 2, nchar(species_list)))

obis.data <- foreach(eachspecies = species_list2, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  

    obis.data.table  <- tryCatch(robis::occurrence(scientificname=eachspecies, geometry =areapolygon, fields=c("species","decimalLongitude", "decimalLatitude"),verbose=TRUE),error=function(e) as.character()) 
    
    
    if(length(obis.data.table)!=0){
      
      obis.data.frame  <- obis.data.table %>% 
        tbl_df %>% 
        mutate(source = "obis") %>% 
        dplyr::select(species, decimalLongitude, decimalLatitude, source) %>% 
        setnames(c("species","lon","lat","source")) %>% 
        #remove duplicates based on the combination of latitude, longitude and species
        distinct(species, lat, lon, source) %>% 
        na.omit %>% 
        mutate(submitted_name = eachspecies)#eliminate rows with NA
      
      return(obis.data.frame)
      
    }
  
}

stopCluster(cl)

obis.sp <- obis.data %>% 
  bind_rows %>% 
  distinct(submitted_name, species, lat, lon, source) %>% 
  na.omit %>% 
  dplyr::select(submitted_name, species, lat, lon, source)


setwd(datapath)
#write table
print(paste("Retrieved OBIS",nrow(obis.sp)))
write_csv(obis.sp, file="record_queries_obis.csv")

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
        mutate(source=this.source, submitted_name=eachspecies)
      
      return(vertnet.records)
      
    }
  }
}

stopCluster(cl) 


vertnet.sp <- vertnet.data %>% rbindlist %>% 
  distinct(submitted_name, species, lat, lon, source) %>% 
  na.omit %>% 
  dplyr::select(submitted_name, species, lat, lon, source)


setwd(datapath)
print(paste("Retrieved Vertnet",nrow(vertnet.sp)))
write_csv(vertnet.sp, file="record_queries_vertnet.csv",row.names=FALSE)

#' Read excel files from UABCS, Pangas, etc.
setwd(datafiles)
xls_files <- list.files(pattern = "\\.xlsx$")# list files


#loop to read in data and obtain GOC data
xls.function <- function(eachfile) {
    
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

biodiversity.xls <- lapply(xls_files, xls.function)

xls.sp <- biodiversity.xls %>% 
  bind_rows %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit %>% 
#  filter(species %in% species_list) %>% 
  mutate(submitted_name= species) %>% 
  dplyr::select(submitted_name, species, lat, lon, source)

setwd(datapath)
#write table
print(paste("Retrieved XLS",nrow(xls.sp)))
write_csv(xls.sp, path="record_queries_xls.csv")

#' Get shark and seagrass data files from Ulloa et al. 2006
# these were the only groups with species-level data
setwd(datafiles)
csv_files <- list.files(pattern = "^ulloa*")# list files
counter <- 1
biodiversity.ulloa <- list()
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
      mutate(species=tolower(species)) %>%
      distinct(species, lat, lon, source) %>% 
      na.omit
    
    str(ulloa.data)
    
    biodiversity.ulloa[[eachfile]] <<- ulloa.data
    
    if(counter==oldlength) stop("Completed retrieving data")
    if(counter<oldlength) counter <<- counter + 1
    
  } # end file
}

ulloa.function()

ulloa.sp <- biodiversity.ulloa %>% 
  rbindlist %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit %>% 
  mutate(species = tolower(species)) %>% 
#  filter(species %in% species_list) %>% 
  mutate(submitted_name= species) %>% 
  dplyr::select(submitted_name, species, lat, lon, source)


setwd(datapath)
#write table
print(paste("Retrieved CSV files",nrow(ulloa.sp)))
write_csv(ulloa.sp, "record_queries_csv.csv")

#' CONABIO data
# these were the only groups with species-level data
setwd(datafiles)

conabio.sp <- list.files(pattern = glob2rx("SNIB*.txt"))%>% 
  fread(header=FALSE) %>% 
  tbl_df %>% 
  dplyr::select(V5, V6, V7, V8) %>% 
  mutate(source="conabio") %>% 
  mutate(species=paste(V5,V6,sep=" ")) %>% 
  dplyr::select(species,V7,V8,source) %>% 
  setnames(c("species","lon","lat","source")) %>% 
  distinct(species, lon, lat, source) %>% 
  na.omit %>% 
  mutate(species=if_else(species=="Hydrolithon samo\xebnse","Hydrolithon samoense",species)) %>% 
  mutate(species=if_else(species=="Perissono\xeb cruciata","Perissono cruciata",species)) %>% 
  mutate(species=if_else(species=="Terpsino\xeb americana", "Terpsinoe americana",species)) %>% 
  mutate(species=if_else(species=="Terpsino\xeb musica", "Terpsinoe musica",species)) %>% 
  mutate(species = tolower(species)) %>% 
  filter(species %in% species_list) %>% 
  mutate(submitted_name= species) %>% 
  dplyr::select(submitted_name, species, lat, lon, source)

setwd(datapath)
#write table
print(paste("Retrieved Conabio",nrow(conabio.sp)))
write_csv(conabio.sp, file="record_queries_conabio.csv",row.names=FALSE)

## @knitr combinebiodiversity
#' combines all record queries in one file

datapath <- "~/conapescaspecies/biodiverdata"
workpath <- "~/conapescaspecies" #put path

setwd(datapath)
record.files <- list.files(pattern = "record_queries*")# list files

record_function <- function(eachfile){
  
   print(eachfile)
    
    biodiversity.file <- read_csv(eachfile) %>% 
      mutate(species=trimws(species)) %>%
      mutate(species=tolower(species), submitted_name=tolower(submitted_name)) %>%
      distinct(submitted_name, species, lat, lon, source) %>%
      mutate_at(vars(c(lat,lon)), as.numeric, na.rm=TRUE) %>% 
      na.omit %>%
      mutate(file=eachfile)
    
    print(head(biodiversity.file))
    
    print(paste(nrow(biodiversity.file),"records retrieved", eachfile))
    
    str(biodiversity.file)
   return(biodiversity.file)
}

biodiversity.rec <- lapply(record.files, record_function)

biodiversity.all <- biodiversity.rec %>% 
  bind_rows %>%   
  distinct(submitted_name, species, lat, lon, .keep_all=TRUE) %>% tbl_df %>% 
  mutate_at(vars(c(lat,lon)), as.numeric, na.rm=TRUE)

print(paste("Total Records retreived:", nrow(biodiversity.all),sep= " "))

setwd(workpath)
write_csv(biodiversity.all,"biodiversity_record_queries_combined.csv")

## @knitr selectspecies
#' subset only target species and changes synonyms to valid names


datapath <- "~/conapescaspecies/biodiverdata"
workpath <- "~/conapescaspecies" #put path

setwd(workpath)

names_goc <- read_csv("species_synonyms_all.csv") 


biodiversity.goc <- read_csv("biodiversity_record_queries_combined.csv") %>% 
  distinct(submitted_name, species,lat,lon) %>% 
  rename(old_name=submitted_name) %>% 
  mutate(old_name = tolower(old_name))


syn.name.list <- names_goc %>% dplyr::select(syn_name, valid_name) %>% 
  setNames(c("old_name","valid_name")) %>% 
  distinct(old_name,valid_name)

valid.name.list <- names_goc %>% dplyr::select(valid_name) %>% 
  bind_cols(.,.) %>% 
  setNames(c("old_name","valid_name")) %>% 
  distinct(old_name,valid_name)

name.list <- names_goc %>% dplyr::select(old_name, valid_name) %>% 
  setNames(c("old_name","valid_name")) %>% 
  bind_rows(syn.name.list, valid.name.list) %>% 
  distinct(old_name,valid_name) 

#Nov 2020 check here because there are species missing valid names and that should not be happening anymore
species.records <- biodiversity.goc %>% 
  left_join(name.list,by="old_name") %>% 
  dplyr::select(-old_name) %>% 
  mutate(valid_name=if_else(is.na(valid_name),species,valid_name)) %>% 
  distinct(valid_name, lat, lon)

setwd(workpath)

print(str(species.records))
write_csv(species.records, "CONAPESCA_ocurrencias_reg.csv")


#' subset only data in the Pacific Ocean
#' iterates on sets of data otherwise insufficient memory

# projections
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

setwd(workpath)
biodiversity.all <- fread("CONAPESCA_ocurrencias_reg.csv")

pacific.shape <- readOGR(".", "pacific_ocean") 
atlantic.shape <- readOGR(".", "atlantic_ocean") 

biodiversity.row  <-  1:nrow(biodiversity.all)

point_func <- function(rownum, goc.shape) {
  
  print(rownum)
  #read in shapefile
 
  
  this.row <- biodiversity.all[rownum,] %>% 
    na.omit %>% 
   as.data.frame
  
  print(this.row)
  
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
    
point.pacific <- mclapply(biodiversity.row,point_func, goc.shape = pacific.shape)

biodiversity.pacific <- point.pacific %>% 
  bind_rows %>%   tbl_df %>% 
  distinct(valid_name, lat, lon) %>% 
  na.omit

setwd(workpath)

write_csv(biodiversity.pacific,"pacific_species_ocurrence.csv")
print(paste("FINAL Pacific records retreived:", nrow(biodiversity.pacific),sep= " "))

# biodiversity.pacific %>% 
#   filter(valid_name=="megapitaria squalida"| valid_name=="megapitaria aurantiaca") %>% 
#   write_csv("almeja_chocolata_pacifico.csv")



point.atlantic <- mclapply(biodiversity.row,point_func, goc.shape = atlantic.shape)

biodiversity.atlantic <- point.atlantic %>% 
  bind_rows %>%   tbl_df %>% 
  distinct(valid_name, lat, lon) %>% 
  na.omit

setwd(workpath)

write_csv(biodiversity.atlantic,"atlantic_species_ocurrence.csv")
print(paste("FINAL Atlantic records retreived:", nrow(biodiversity.atlantic),sep= " "))

# biodiversity.atlantic %>% 
#   filter(valid_name=="megapitaria squalida"| valid_name=="megapitaria aurantiaca") %>% 
#   write_csv("almeja_chocolata_atlantico.csv")

#Google has recently changed its API requirements, and ggmap users are now required to provide an API key and enable billing. 
#https://github.com/dkahle/ggmap

# qmplot(lon, lat, data = biodiversity.goc, maptype = "satellite", color = I("white"), source = "google",extent = "panel", zoom = 3)+
#   #    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
#   labs(x = 'Longitude', y = 'Latitude')+
#   theme(axis.title.x = element_text(size=13),
#         axis.title.y = element_text(size=13, angle=90),
#         axis.text.x = element_text(size=10),
#         axis.text.y = element_text(size=10))


#'Separate species with more than 20 records for SDMs and generate maps

setwd(workpath)

subset_data <- function(this.file, this.region){
  
this.data <- fread(this.file) %>% 
    rename(species=valid_name) %>% 
    mutate(species=gsub(" l$","",species))
  
region.records <- this.data %>% 
  mutate(record=1) %>% 
  group_by(species) %>%
  summarise(tot_count=sum(record))

species.list.20 <- region.records %>% 
  filter(tot_count>20) %>% 
  .$species

this.data %>% 
  filter(species %in% species.list.20) %>% 
  write_csv(paste(this.region,"_CONAPESCA_sp_20reg.csv"))

species.list.30 <- region.records %>% 
  filter(tot_count>30) %>% 
  .$species

this.data %>% 
  filter(species %in% species.list.30) %>% 
  write_csv(paste(this.region,"_CONAPESCA_sp_30reg.csv"))

this.data %>% 
  filter(species %in% species.list.20) %>%
qmplot(lon, lat, data = ., maptype = "toner-lite", size =I(0.5), color = I("black"), extent = "panel", zoom = 3)+
  #    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  labs(x = 'Longitude', y = 'Latitude')+
  theme(axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))


ggsave(paste(this.region,"_CONAPESCA_sp_20_points.png"), dpi = 300)


}

subset_data(this.file="atlantic_species_ocurrence.csv",this.region = "atlantic") 

subset_data(this.file="pacific_species_ocurrence.csv",this.region = "pacific")

#set directory path

save_species <- function(this.file, this.region){
  
  species.list <- read_csv(this.file) %>% 
    distinct(valid_name) %>% 
    mutate(region = this.region) 
  
  species.list %>% 
    write_csv(paste0(this.region,"_specieslist.csv"))
  
  fish <- species.list %>% 
    mutate(valid_name=str_to_sentence(valid_name)) %>% 
    dplyr::select(valid_name) %>% 
    .$valid_name %>% 
    validate_names()
  }

save_species(this.file="atlantic_species_ocurrence.csv",this.region = "atlantis")
save_species(this.file="pacific_species_ocurrence.csv",this.region = "pacific")


region.files <- list.files(pattern = "*.*20reg.csv$")






lapply(region.files,graph.species)
