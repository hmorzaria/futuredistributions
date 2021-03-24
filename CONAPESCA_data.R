#' Ocurrence record retrieval from CONAPESCA records
#' @author Hem Nalini Morzaria Luna
#' @date March 2016
#' organize data from excel spreadsheets
#' Last reviewed January 2019
#' catch data for 2014-106
#' http://datos.gob.mx/

# List of packages for session

#install.packages(c("XMLSchema", "SSOAP"), repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"))
#if(!require(robis)){devtools::install_github("iobis/robis"); library(robis)}
#if(!require(taxizesoap)){devtools::install_github("ropensci/taxizesoap"); library(taxizesoap)}

install.packages("devtools")
library(devtools)
#install_local("/home/atlantis/ENMTools-master")
library(ENMTools)


#if(!require(ENMTools)){devtools::install_github("danlwarren/ENMTools"); library(ENMTools)}

.packages = c("devtools","tidyverse","rgbif","raster","readxl",
              "stringr","rvest","R.utils","future", "parallel", "doSNOW",
              "ggmap","janitor")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org', dependencies = TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#installAll() #needed packages for sdm

#' clean up the space

rm(list=ls())


datapath <- "~/conapescaspecies/catchdata"
workpath <- "~/conapescaspecies" #put path
datafiles <- "~/conapescaspecies/Data"

setwd(workpath)

#this is detailed data to 2016 but has no scientific names
ifai.sheets <- excel_sheets("CONAPESCA_IFAI_Pacifico.xlsx") %>% 
  grep("MEN",.,value = TRUE)

get.species2 <- function (this.sheet){
  
  print(this.sheet)
  
  this.data <- read_xlsx("CONAPESCA_IFAI_Pacifico.xlsx", sheet=this.sheet)
  
  these.names <- this.data %>% 
    dplyr::select(starts_with("PUERTO"),starts_with("UNIDAD"),starts_with("OFICINA"),starts_with("FECHA"),`ZONA DE PESCA`,ESPECIE,contains("PESO DESEMBARCADO")) %>% 
    setNames(c("port","economic_unit","date","fishing_area","nombre_comun","landed_weight")) %>% 
    mutate(date = as.character(date)) %>% 
    filter(!grepl("DE CULTIVO|RANA ",nombre_comun)) %>% 
    filter(!grepl("DE AGUA DULCE",nombre_comun)) %>% 
    filter(!grepl(" SINTETICO| DE VIVERO",nombre_comun)) %>% 
    mutate(nombre_comun_rev= gsub(" S.C. FCA.| S.C. FCO.| DESV. FCO.| DESV. FCA| FILETE DE FCO.| DESV. Y DESC. FCO.| DESV. Y DESC. FCO| DESC. FCO.| ENT. COCIDA| PULPA FCA. DE| ALETA FCA.| CARNE SECA| CARNE FCA.| ENT. SECO| ALETA DE FCA.| ENT. COCIDO| DESC. Y| PIEL DE| FCO.| CONCHA DE| ALETA DE| FILETE SECO| SECA|[.]| SECO| DESC| COLAS DE FCA| MANTODE| MANTODE| FILETE DE SECO| COLAS DE| DESV DESC FCA| DESV Y DESC FC| TRIPA DE| DESV Y DESC| CARNE DE| VIVO| S/MANTO ","",nombre_comun)) %>% 
    mutate(nombre_comun_rev= gsub("TENTACULOS DE| DESC PTO| COCIDO| CARNE DE SECA| MANOS DE| DESV. DESC. FCO.| DESV. Y DESC. FCA.| DESC. FCO.| CARNE DE FCA.| DESC. SECO| ENT. FCO.| ENT. FCA.| ENTFCA| ENTFCO| VIVITA CARNADA| COCIDAS| PULPA DE FCA| DESV, DESC FCO| DESC COCIDO| CABEZA DE| PULPA FCO.| ALETA DE SECA| S/C FCO.| FILETE FCO.| PIEL DE| HUEVA DE| ENT FCA| P/CARNADA| MANTODE| DESV FCA| DESV Y FC| DESV Y| ENTFCO| DESV| S/C| FILETE| ENT| ALETA CARTON DESV. Y DESC.| TENTACULOS DE| DESV, FCO| DESV| ALETA DE| COCIDO| TUBO DE| FCO| POLVO DE| FCA| DESC PTO| FRESCO| COCIDA| AVISOS|","",nombre_comun_rev)) %>% 
    mutate(nombre_comun_rev= gsub("AA","A",nombre_comun_rev)) %>% 
    mutate(nombre_comun_rev= gsub("ROJOFCO","ROJO",nombre_comun_rev)) %>% 
    mutate(nombre_comun_rev= gsub("MERLUZAFCA","MERLUZA",nombre_comun_rev)) %>% 
    mutate(nombre_comun_rev= gsub("MANTARRAYASECA","MANTARRAYA",nombre_comun_rev)) 
  
  print(paste("Number of records",nrow(these.names)))
  
  print(these.names %>% distinct(date))
  
  return(these.names)
  
}


conapesca.full.species <- lapply(ifai.sheets,get.species2) %>% 
  bind_rows

conapesca.date <- conapesca.full.species %>% 
  mutate(date_num = as.numeric(date), date_no = excel_numeric_to_date(date_num)) %>% 
  separate(date,c("yr","mo","day"),sep="-",remove=FALSE)

w.graph <- conapesca.date %>% 
  filter(yr>1990) %>% 
  filter(nombre_comun %in% c("ABULON","CALAMAR","JAIBA","ERIZO","PULPO","OSTION","LANGOSTA","LENGUADO","LISA")) %>% 
  mutate(date_new = as.Date(date)) %>% 
  ggplot(aes(date, landed_weight, colour = nombre_comun))+
           geom_line()+
  facet_wrap(~nombre_comun, scales="free")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("CONAPESCA_sp_time.png", w.graph, dpi = 300)
  

conapesca.list.species <- conapesca.full.species %>% 
  mutate(nombre_comun=tolower(nombre_comun)) %>% 
  left_join(conapesca.species, by="nombre_comun") %>%
  filter(!is.na(nombre_cientifico)) %>% 
  distinct(nombre_comun,nombre_cientifico) 

write_csv(conapesca.list.species,"conapesca_lista_especies.csv")

conapesca.missing <- conapesca.full.species %>% 
  mutate(nombre_comun=tolower(nombre_comun)) %>% 
  left_join(conapesca.species, by="nombre_comun") %>%
  filter(is.na(nombre_cientifico)) %>% 
  mutate(nombre_comun= gsub(" asado| ind$","",nombre_comun)) %>% 
  mutate(nombre_comun= gsub("alta mar","altamar",nombre_comun)) %>%
  distinct(nombre_comun,nombre_cientifico) %>% 
  arrange(nombre_comun)

write_csv(conapesca.missing,"conapesca_faltantes_especies.csv")

#this file has been checked manually

conapesca.list.species2 <- read_csv("conapesca_especies_revisadas.csv")

species.list <- conapesca.list.species %>% 
  bind_rows(conapesca.list.species2) %>% 
  filter(!is.na(nombre_cientifico)) %>% 
  filter(nombre_cientifico!="no encontrado") %>% 
  filter(nombre_cientifico!="") %>% 
  filter(nombre_cientifico!="#n/a") %>% 
  mutate(nombre_cientifico = tolower(nombre_cientifico)) %>% 
  distinct(nombre_cientifico) %>% 
  arrange(nombre_cientifico) %>% 
  filter(!grepl(" spp$",nombre_cientifico)) %>% 
  filter(!grepl(" sp.$",nombre_cientifico))

write_csv(species.list,"final_species_list_conapesca.csv")

#used to erase some strange characters manually

species.list <- read_csv("final_species_list_conapesca.csv") %>% 
  filter(!grepl(" spp$",nombre_cientifico)) %>% 
  filter(!grepl(" spp.$",nombre_cientifico)) %>% 
  filter(!grepl(" sp.$",nombre_cientifico)) %>% 
  filter(!grepl(" sp$",nombre_cientifico)) %>% 
  mutate(wordnum = str_count(nombre_cientifico,"\\S+")) %>% 
  filter(wordnum != 1) %>% 
  distinct(nombre_cientifico) %>% 
  arrange(nombre_cientifico) %>% .$nombre_cientifico


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