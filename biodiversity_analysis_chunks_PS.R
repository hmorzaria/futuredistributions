#' Gather species ocurrence records and generate richness model
#' 
#' chunks that feed into biodiversity_article.Rmd
#' @author Hem Nalini Morzaria Luna , \email{hmorzarialuna@gmail.com}
#' @keywords biodiversity, richness
#' Created December 2015
#' last reviewed October 2016

# setpreferences: library prefs
# pointgrid: develop point grid 
# getbiodiversity: retrieve and clean up biodiversity files
# organizebiodiversity: Merge all biodiversity files, check taxonomy
# checktaxonomy: check using taxize
# updatetaxonomy: update taxonomy
# recbiodiversity: update biodiversity file
# mapbiodiversity: Reads in the species records and maps them
# richnessmodel: Calculates a richness model using sperich
# corridorpoints: Cut out points in corridor and map
# richnessmodelcorridor: model in corridor


## @knitr setpreferences

rm(list=ls())    

if(!require(ridigbio)){devtools::install.github("iDigBio/ridigbio"); library(ridigbio)}
if(!require(robis)){devtools::install_github("iobis/robis"); library(robis)}
install.packages(c("XMLSchema", "SSOAP"), repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"))
if(!require(taxizesoap)){devtools::install_github("ropensci/taxizesoap"); library(taxizesoap)}


# List of packages for session
.packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
              "PBSmapping", "fields","data.table","rgbif","raster", "rasterVis",
              "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
              "rvertnet", "httr","wesanderson","tidyr","cowplot","rbison","rebird","taxize","readr","rgdal","XML", 
              "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","plyr","geosphere","magrittr",
              "taxizesoap","future", "parallel", "doSNOW")

.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])



# Load packages into session 
lapply(.packages, require, character.only=TRUE)

if(!require(rmarkdown)){devtools::install_github("rstudio/rmarkdown"); library (rmarkdown)}
if(!require(raster)){install.packages('raster', repos = 'http://r-forge.r-project.org/', type = 'source'); library(raster)}
if(!require(rgdal)){install.packages('rgdal', type='source'); library(rgdal)}
if(!require(proj4)){install.packages('proj4', type='source'); library(proj4)}

## @knitr getbiodiversity
rm(list=ls())  
workpath = "~/Dropbox/Puget_sound"
shapepath = "~/Dropbox/Puget_sound"
savepath = "~/Dropbox/Puget_sound"
datafiles="~/Dropbox/Puget_sound"

# workpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/RCode"
# shapepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/SIG_Biodiversity"
# savepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis"
# ulloafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Datos/Ulloa_datos" #put path
# datafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Datos/Ocurrencia_especies"

# projections
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
crs.lcc <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

setwd(workpath)

# coordinates for Puget Sound Atlantis Model

maxlat <- 49.5
minlat <- 46.5
maxlon <- -124
minlon <- -122

lat <- c(maxlat,minlat,minlat,maxlat)
lon <- c(maxlon,maxlon,minlon,minlon)

#bounding box for ecoengine
# uses paging so one big bounding box can retrieve all records
bbox.data.eco <- paste(maxlon,minlat,minlon,maxlat,sep=",")

#' points to polygons
goc.points = data.frame(lon,lat) 
coordinates(goc.points) <- c("lon", "lat")
goc.points = goc.points %>% rbind(.,goc.points[1,]) 
proj4string(goc.points) <- crs.geo.wgs # define projection

goc.pol = SpatialPolygons(list(Polygons(list(Polygon(goc.points)), 1)))
proj4string(goc.pol) <- crs.geo.wgs # define projection

# Create a dataframe and display default rownames
goc.pol.df <- data.frame( ID=1:length(goc.pol))
rownames(goc.pol.df)

# Extract polygon ID's
pid <- sapply(slot(goc.pol, "polygons"), function(x) slot(x, "ID"))

# Create dataframe with correct rownames
goc.pol.df <- data.frame( ID=1:length(goc.pol), row.names = pid)    

# Try coersion again and check class
goc.pol <- SpatialPolygonsDataFrame(goc.pol, goc.pol.df)
proj4string(goc.pol) <- crs.geo.wgs
plot(goc.pol)
#write polygon as kml; used to estimate distance on Google Earth
writeOGR(goc.pol, dsn="pscpolygon.kml", layer= "goc.pol", driver="KML", dataset_options=c("NameField=name"))
writeOGR(goc.pol, dsn="pspolygon.shp", layer= "goc.pol", driver="ESRI Shapefile", overwrite_layer=TRUE)


#' create point grid for Puget Sound
#' this grid will be used as vertices to search for species records
#' keep in geographic
#' this returns a spatial point object
#' otherwise couldn't find difference from makegrid
#' goc.sample.points = spsample(goc.pol, n = 4000, "regular")
#' bison is slow change to 500 points - also not that many records


pointsingrid <- 5000
goc.point.grid  <- makegrid(goc.pol, n = pointsingrid, pretty=FALSE)
#creates a spatial points object
#goc.point.grid  <- spsample(goc.pol, n = 4000, "regular")
plot(goc.point.grid)

latrows  <-  length(unique(goc.point.grid$x1))
lonrows  <-  nrow(goc.point.grid) / length(unique(goc.point.grid$x1))

# Used instructions here to get polygon grid 
#http://neondataskills.org/working-with-field-data/Field-Data-Polygons-From-Centroids
goc.point.grid.id <- goc.point.grid %>% tbl_df %>% mutate(ID=seq(1:nrow(goc.point.grid)))

ID <- goc.point.grid.id$ID

#set the radius for the plots

radius <- abs(goc.point.grid[1,1]) - abs(goc.point.grid[2,1])

#define the plot boundaries based upon the plot radius. 
#NOTE: this assumes that plots are oriented North and are not rotated. 
#If the plots are rotated, you'd need to do additional math to find 
#the corners.
yPlus <- goc.point.grid.id$x1+radius
xPlus <- goc.point.grid.id$x2+radius
yMinus <- goc.point.grid.id$x1-radius
xMinus <- goc.point.grid.id$x2-radius

#calculate polygon coordinates for each plot centroid. 
square <- cbind(xMinus,yPlus, xPlus,yPlus, xPlus,yMinus, xMinus,yMinus,xMinus,yPlus,xMinus,yPlus)

polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(square, row(square)), ID),proj4string=crs.geo.wgs)

polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
plot(polys.df, col=rainbow(50, alpha=0.5))

#' create list of WKT polygons

poly.data.frame <- cbind(xMinus,yPlus,yMinus,xPlus) %>% tbl_df %>% setNames(c("bottomlat","bottomlon","toplon","toplat"))

poly.data <- paste("POLYGON((",yMinus," ",xMinus,",",yPlus," ",xMinus,",",yPlus," ",xPlus,",",yMinus," ",xPlus,",",yMinus," ",xMinus,"))",sep="") %>% unlist()

#this is distance between points
distancepoints <- distm(c(goc.point.grid[1,1], goc.point.grid[1,2]), c(goc.point.grid[2,1], goc.point.grid[2,2]), fun = distHaversine)%>% divide_by(2)
# distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)


  
## @knitr getbiodiversity

#' Record retrieval
#' First retrieve from data bases that don't take bounding boxes
#' this is FishBase 
#' retrieve all records for an ecosystem
#' Gulf of California = 165

 
site_list = c(132,9,239,10,259,122) 
region_list = c("California Current", "Pacific Ocean","Pacific Central","Artic","Antartic","Gulf of Alaska")
print("Now querying Fishbase")

counter <- 1
biodiversity <- list()
oldlength <- length(site_list)
mirror <- "http://www.fishbase.de" # "http://www.fishbase.org"

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)


fishbase.data <- foreach(this.site = site_list) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
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


robertson.data <- foreach(this.num = species_num) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
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

#' Retrieve iDigBio
# create original polygon grid
# Oct 2016, new limits on search records return error if >100000 per cell

print("Now querying iDigBio")
oldlength <- 1:nrow(poly.data.frame)


#start clusters
NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

#counter <- 1
#biodiversity  <-  list()
#oldlength <- length(species_list)

idigbio.data <- foreach(counter = oldlength, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
      bottom_right_lon  <-  poly.data.frame[counter,] %>% tbl_df %>% select(bottomlon) %>% as.numeric()
      bottom_right_lat  <- poly.data.frame[counter,] %>% tbl_df %>% select(bottomlat)%>% as.numeric()
      top_left_lon  <-  poly.data.frame[counter,] %>% tbl_df %>% select(toplon)%>% as.numeric()
      top_left_lat  <-  poly.data.frame[counter,] %>% tbl_df %>% select(toplat)%>% as.numeric()
      
      print(paste(top_left_lat, top_left_lon, bottom_right_lat, bottom_right_lon))
      
      #countrecords
      
      rec_count <- try(idig_count_records(rq=list(geopoint=list(type="geo_bounding_box", top_left=list(lat=top_left_lat, lon=top_left_lon), bottom_right= list(lat=bottom_right_lat, lon= bottom_right_lon))), fields=c("scientificname", "geopoint"), limit=100000))
      
      print(paste("Records available",rec_count))
      
      if(inherits(rec_count, "try-error")) {
      
        print("Error")
        
        }
      
      if(!inherits(rec_count, "try-error")){
        
       df1 <- try(idig_search_records(rq=list(geopoint=list(type="geo_bounding_box", top_left=list(lat=top_left_lat, lon=top_left_lon), bottom_right= list(lat=bottom_right_lat, lon= bottom_right_lon))), fields=c("scientificname", "geopoint"), limit=100000))
        
  
        #print(head(df1))
        
        if(!nrow(df1)==0){
          
          if(any(is.na(df1$scientificname))){
            
            biodiversity.idigbio <- df1 %>% 
              dplyr::select(scientificname,geopoint.lat,geopoint.lon) %>% 
              tbl_df %>% 
              mutate(source = "idigbio") %>% 
              setNames(c("species","lat","lon","source"))%>% 
              mutate(species = capitalize(species)) %>% 
              mutate(polnum=counter) %>% 
              mutate(gridsize = nrow(poly.data.frame)) %>% 
              mutate(rownum=nrow(df1)) %>% 
              as.data.frame
            
            print(paste("Retrieved records",nrow(biodiversity.idigbio)))
            
           return(biodiversity.idigbio)
           #   print(str(biodiversity))
          }
        }
      }
    
}

stopCluster(cl)

biodiversity.sp <- idigbio.data %>% 
  rbindlist %>% 
  na.omit %>% tbl_df

setwd(savepath)  

biodiversity.clean <- biodiversity.sp %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit

print(paste("IDigBio records retreived:", nrow(biodiversity.sp),sep= " "))
print(paste("Clean IDigBio records retreived:", nrow(biodiversity.clean),sep= " "))

#use this to save the complete data file
setwd(savepath)
write.csv(biodiversity.clean,file="record_queries_idigbio.csv")

#use this if reading the output file
#biodiversity.sp <- fread("record_queries_idigbio.csv") %>% tbl_df

#' Retrieve BISON
# create original grid

# create frame for results
oldlength <- 1:length(poly.data)

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

counter <- 1

bison.data <- foreach(counter = oldlength, .verbose = T) %dopar% {
  
 
  packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
               "fields","data.table","rgbif","raster", "rasterVis",
               "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
               "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
               "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  

    #select polygons or bounding box    
    
    thispolygon <- poly.data[counter]
    

    print(paste("Analyzing record",thispolygon, "is point",counter,":",length(poly.data)))
    
    #ERROR HANDLING
    # obtain bison data
    bison.data <- try(bison(aoi=thispolygon,count=10000))
    
    if(!inherits(bison.data, "try-error")){
      
      bison.data.tbl  <- bison.data%>% 
        .$points %>% 
        data.frame %>% 
        tbl_df
      
      if(nrow(bison.data.tbl)!=0){
        
        bison.data.clean <- bison.data.tbl %>% 
          mutate(source = "bison") %>% 
          dplyr::select(name,decimalLatitude,decimalLongitude,source) %>% 
          setNames(c("species","lat","lon","source")) %>% 
          as.data.frame() %>% 
          mutate(polnum=counter) %>% 
          mutate(gridsize = length(poly.data)) %>% 
          mutate(rownum=nrow(bison.data.tbl)) %>% 
          as.data.frame
        
       
        return(bison.data.clean)
        
          }
      
      
    }
    
   
}

stopCluster(cl)

biodiversity.sp <- bison.data %>% 
  rbindlist %>% 
  na.omit


test.2000 <- biodiversity.sp %>% filter(rownum>10000) %>% distinct(polnum)


biodiversity.clean <- biodiversity.sp %>% 
  distinct(species, lat, lon, source) 

setwd(savepath)
write.csv(biodiversity.clean,file="record_queries_bison.csv")

print(paste("Bison Records retreived:", nrow(biodiversity.sp),sep= " "))
print(paste("Clean Bison Records retreived:", nrow(biodiversity.clean),sep= " "))


#' Retrieve GBIF

#GBIF has many more records available so using a larger grid
# Using a very large grid to avoid reaching limit, because parallel processing has a problem
# accesing Global variables

oldlength <- 1:length(poly.data)
gbif.row.num <- 0


NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

gbif.data <- foreach(counter = oldlength, .verbose = T) %dopar% {
  
  packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
               "fields","data.table","rgbif","raster", "rasterVis",
               "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
               "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
               "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  
  #ERROR HANDLING
  # get gbif records
  
  thispolygon <- poly.data[counter]
  
  print(paste("GBIF Analyzing record",thispolygon, "is point",counter,":",length(poly.data)))
  
  #gbif.data <- try(occ_search(geometry=thispolygon,return='data',fields=c("name","decimalLatitude","decimalLongitude","datasetName","collectionCode"),limit=200000,config=verbose()))
  gbif.data <- try(occ_search(geometry=thispolygon,return='data',fields=c("name","decimalLatitude","decimalLongitude","datasetName","collectionCode"),hasGeospatialIssue=FALSE, limit=200000))
  
  withRestarts({ 
    
    if(inherits(gbif.data, "try-error")) {
      
      "Network error, restarting..."
    }
    #' arrange data frame only if records exist
    #' otherwise returns empty frame
    
  }, restartLoop = function() { 
    message("restarting") 
    NULL 
  })
  
  if(!gbif.data[1]=="no data found, try a different search" | any(grepl("decimalLongitude",colnames(gbif.data)))==TRUE){ 
    
    gbif.clean  <-  gbif.data %>% 
      tbl_df() %>% 
      mutate(source = "gbif") %>% 
      dplyr::select(name, decimalLatitude,decimalLongitude, source) %>% 
      setNames(c("species","lat","lon","source")) %>% 
      mutate(polnum=counter) %>% 
      mutate(gridsize = length(poly.data)) %>% 
      mutate(rownum=nrow(gbif.data)) %>% 
      as.data.frame
    
    
    print(paste("Clean GBIF has", nrow(gbif.clean)))
    print(head(gbif.clean))
    
    return(gbif.clean)
    
  }
  
}

  
stopCluster(cl)

biodiversity.sp <- gbif.data %>% 
  rbindlist %>% 
  na.omit %>% 
  tbl_df

test.2000 <- biodiversity.sp %>% filter(rownum>199000) %>% distinct(polnum)

setwd(savepath)  

biodiversity.clean <- biodiversity.sp %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit

write.csv(biodiversity.clean,file="record_queries_gbif.csv")

print(paste("GBIF Records retreived:", nrow(biodiversity.sp),sep= " "))
print(paste("Clean GBIF Records retreived:", nrow(biodiversity.clean),sep= " "))


#' Query Ecoengine

print("Now querying ecoengine")

biodiversity <- list()

eachbox <- 1
pagesize <- 1000

# uses paging so one big bounding box can retrieve all records

thisbox = bbox.data.eco[eachbox]
print(paste("Analyzing record",thisbox, "is point",eachbox,":",length(bbox.data.eco)))

ee.data.frame <- try(ee_observations(page_size=pagesize, georeferenced = TRUE,bbox = thisbox))

total_pages <- ee_pages(ee.data.frame)

total_pages <- ceiling(ee.data.frame$results / pagesize)

if(total_pages > 0){
  
  grouped_pages <- split(1:total_pages, ceiling(1:total_pages/pagesize))
  
  all_data <- llply(grouped_pages[1], function(x) {
    ee_observations(page_size=1000, georeferenced = TRUE,bbox = thisbox, page = x, progress = FALSE, quiet = TRUE)
  }, .progress = "text")
  
  ee.data.frame <- ee_cbind(all_data)
  
}

biodiversity.sp  <-  ee.data.frame %>% .$data %>% tbl_df() %>% 
  select(scientific_name, latitude, longitude) %>% 
  setNames(c("species","lat","lon")) %>% 
  mutate(source = "ecoengine") %>% 
  na.omit

biodiversity.clean  <-  biodiversity.sp %>% 
  distinct(species, lat, lon, .keep_all=TRUE) %>% 
  na.omit

#use this to save the complete data file
setwd(savepath)
write.csv(biodiversity.clean,file="record_queries_ecoengine.csv")
print(paste("Ecoengine Records retreived:", nrow(biodiversity.sp),sep= " "))
print(paste("Clean ecoengine Records retreived:", nrow(biodiversity.clean),sep= " "))


#'Vertnet
print("Now querying vertnet")
#' query Vertnet using spatial points  

# make larger grid

#this is distance between points
distancepoints <- distm(c(goc.point.grid[1,1], goc.point.grid[1,2]), c(goc.point.grid[2,1], goc.point.grid[2,2]), fun = distHaversine)%>% divide_by(2)
print(paste("Radius of point grid is",distancepoints,"meters"))

counter <- 1
oldlength <- 1:nrow(goc.point.grid)

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

vertnet.data <- foreach(counter = oldlength, .verbose = T) %dopar% {
  
  packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
               "fields","data.table","rgbif","raster", "rasterVis",
               "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
               "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
               "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  
  #ERROR HANDLING
  
  this.point <- goc.point.grid[counter,]
  
  print(this.point)
  point.lat = this.point[2]
  point.lon = this.point[1]
  
  
  #ERROR HANDLING
  vertnet.call <- tryCatch(spatialsearch(lat = point.lat, lon = point.lon, radius = distancepoints, limit = 10000, verbose= TRUE), error=function(e) as.character())
  
  if(!is.null(vertnet.call)){
    
    if((length(vertnet.call) == 0) && (typeof(vertnet.call) == "character")){
      
      print("Empty data frame")
    } else {
      
      vertnet.data  <-  vertnet.call %>% .$data %>% data.frame 
      
    }
    
    if(!length(vertnet.data)==0){
      
      names.class <- c("specificepithet","scientificname")
      
      if(any(grepl(paste(names.class, collapse = "|"), colnames(vertnet.data)))){
        
        if (any(grepl("scientificname",colnames(vertnet.data)))==TRUE){
          
          vertnet.data.rec  <-  vertnet.data %>%
            tbl_df %>%
            dplyr::select(scientificname, decimallatitude,decimallongitude) %>%
            mutate(source = "vertnet") %>%
            setNames(c('species', 'lat', 'lon',"source")) %>% # rename columns
            mutate_each(funs(as.character),lat:lon)%>%
            mutate_each(funs(as.numeric),lat:lon) %>% 
            mutate(polnum=counter) %>% 
            mutate(gridsize = nrow(goc.point.grid)) %>% 
            mutate(rownum=nrow(vertnet.data)) %>% 
            as.data.frame
        }
        
        if (any(grepl("specificepithet",colnames(vertnet.data)))){
          #create scientificname column when species and genus are separate
          
          vertnet.data.rec  <-  vertnet.data %>% tbl_df %>%
            mutate(scientificname = paste(genus,specificepithet, sep= " "))  %>%
            dplyr::select(scientificname, decimallatitude,decimallongitude) %>%
            mutate(source = "vertnet") %>%
            setNames(c('species', 'lat', 'lon',"source")) %>% # rename columns
            mutate_each(funs(as.character),lat:lon)%>%
            mutate_each(funs(as.numeric),lat:lon) %>% 
            mutate(polnum=counter) %>% 
            mutate(gridsize = nrow(goc.point.grid)) %>% 
            mutate(rownum=nrow(vertnet.data)) %>% 
            as.data.frame
          
        }
        
        
        return(vertnet.data.rec)
        
      }
    }
  } 
}

stopCluster(cl)

biodiversity.sp <- vertnet.data %>% 
  rbindlist %>% 
  na.omit

test.2000 <- biodiversity.sp %>% filter(rownum>10000) %>% distinct(polnum)


biodiversity.clean <- biodiversity.sp %>% 
  distinct(species, lat, lon, .keep_all=TRUE) %>% 
  na.omit

setwd(savepath)
write.csv(biodiversity.clean,file="record_queries_vertnet.csv")

print(paste("Vertnet Records retreived:", nrow(biodiversity.sp),sep= " "))
print(paste("Clean Vertnet Records retreived:", nrow(biodiversity.clean),sep= " "))

# get ebird records
print("Now querying ebird")

biodiversity.sp <-  list()

# get regions from https://confluence.cornell.edu/display/CLOISAPI/eBird-1.1-HotSpotsByRegion

regions.ebird = c('CA-BC-CP','CA-BC-CV','CA-BC-FV','CA-BC-GV','CA-BC-NA','CA-BC-SC','US-WA','US-WA-045','US-WA-031','US-WA-035','US-WA-073','US-WA-067','US-WA-061','US-WA-057','US-WA-055','US-WA-053','US-WA-049','US-WA-041','US-WA-033','US-WA-029','US-WA-027','US-WA-009')
# regions are British Columbia, 
counter <- 1
oldlength <- 1:length(regions.ebird)

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

ebird.data <- foreach(counter = oldlength, .verbose = T) %dopar% {
  
  packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
               "fields","data.table","rgbif","raster", "rasterVis",
               "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
               "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
               "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  this.region <- regions.ebird[counter]
  ebird.data <- ebirdregion(this.region, max=10000)
  
  if(nrow(ebird.data)!=0)  {
    
    ebird.data = ebird.data %>% 
      dplyr::select(sciName,lat,lng) %>% 
      setNames(c('species', 'lat', 'lon')) %>% 
      mutate(source = "ebird") %>% 
      mutate_each(funs(as.character),lat:lon)%>% 
      mutate_each(funs(as.numeric),lat:lon)#make sure lon and lat are numeric
    
    
    return(ebird.data)
    
  }
}

stopCluster(cl)

biodiversity.sp <- ebird.data %>% 
  rbindlist %>% 
  na.omit

biodiversity.clean <- biodiversity.sp %>% 
  distinct(species, lat, lon, .keep_all=TRUE) %>% 
  na.omit

# print records
print(paste("Ebird Records retreived:", nrow(biodiversity.sp),sep= " "))
print(paste("Clean ebird Records retreived:", nrow(biodiversity.clean),sep= " "))

#save output
setwd(savepath)
write.csv(biodiversity.clean,"record_queries_ebird.csv")


#' retrieve OBIS


counter <- 1
oldlength <- 1:length(poly.data)

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

obis.data <- foreach(counter = oldlength, .verbose = T) %dopar% {
  
  packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
               "fields","data.table","rgbif","raster", "rasterVis",
               "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
               "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
               "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  
  this_polygon  <- poly.data[counter]
  
  print(paste("Analyzing obis", counter,":",oldlength))
  
  
  obis.data  <- tryCatch(occurrence(geometry = this_polygon, fields=c("species","decimalLongitude", "decimalLatitude")),error=function(e) as.character())
  
  
  if(!inherits(obis.data, "try-error")){
    
    if(length(obis.data)!=0){
      
      obis.na <- obis.data %>% 
        tbl_df %>% 
        mutate(source = "obis") %>% 
        na.omit
      
      if(nrow(obis.na)!=0){
        
        if (any(grepl("species",colnames(obis.data)))){
          
          obis.data.frame  <- obis.data %>% 
            tbl_df %>% 
            mutate(source = "obis") %>% 
            dplyr::select(species, decimalLongitude, decimalLatitude, source) %>% 
            setnames(c("species","lon","lat","source")) %>% 
            #remove duplicates based on the combination of latitude, longitude and species
            na.omit %>% 
            mutate(polnum=counter) %>% 
            mutate(gridsize = length(poly.data)) %>% 
            mutate(rownum=nrow(obis.data)) %>% 
            as.data.frame()
          
          print(head(obis.data.frame))
          #return(obis.data.frame)
          
          return(obis.data.frame)
          
        }
      }
    }
    
  }
  
}
stopCluster(cl)

biodiversity.sp <- obis.data %>% 
  rbindlist %>% 
  na.omit

biodiversity.clean <- biodiversity.sp %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit

setwd(datapath)
#write table
print(paste("Retrieved OBIS",nrow(biodiversity.sp)))
print(paste("Clean retrieved OBIS",nrow(biodiversity.clean)))

write.csv(biodiversity.clean, file="record_queries_obis.csv")


## @knitr combinebiodiversity
#' combines all record queries in one file
rm(list=ls())    

shapepath = "~/Dropbox/Puget_sound"
savepath = "~/Dropbox/Puget_sound"
datafiles="~/Dropbox/Puget_sound"
workpath="~/Dropbox/Puget_sound"
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
  distinct(species, lat, lon, source) %>% tbl_df %>% 
  mutate_each(funs(as.numeric),lat,lon) 

print(paste("Total Records retreived:", nrow(biodiversity.all),sep= " "))

setwd(savepath)
write.csv(biodiversity.all,"record_queries_combined.csv")

## @knitr organizebiodiversity
#' subset only data in the study area
#' iterates on each row

rm(list=ls())    

shapepath = "~/Dropbox/Puget_sound"
savepath = "~/Dropbox/Puget_sound"
datafiles="~/Dropbox/Puget_sound"
workpath="~/Dropbox/Puget_sound"

# projections
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

setwd(shapepath)

#read in shapefile
goc.shape <- readOGR(".", "Puget_sound")

setwd(workpath)

biodiversity <- fread("record_queries_combined.csv")# list files

rownum <- 1
oldlength <- 1:nrow(biodiversity)
NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

point.data <- foreach(rownum = oldlength, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  
#for(rownum in oldlength){
  
    this.row <- biodiversity[rownum,] %>% 
    na.omit %>% 
    mutate_each(funs(as.numeric),lat,lon) %>% 
    as.data.frame
  
    print(this.row)
    
  if(nrow(this.row)!=0){
    
    if(this.row$lat==-99 & this.row$lon==-99 | this.row$lat>=180 | this.row$lat<=-180){
      
      new.lon <- 180 
      new.lat <- 90
      
      this.row$lat <- new.lat
      this.row$lon <- new.lon
      
    } 
    
    if(this.row$lat>=90 | this.row$lat<=-90){
      
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
    
    print(this.row)
    
    # subset ocurrence points within GOC
    #get table from shapefile
    biodiversity.sec <- this.row[goc.shape, ] %>% as("data.frame")
    test.bio  <- nrow(biodiversity.sec)==0
    
    print(biodiversity.sec)
    
    if (test.bio==FALSE)
    {
      
      biodiversity.clean <- biodiversity.sec %>% 
        distinct(species, lat, lon, .keep_all= TRUE) %>% 
        na.omit
      
      return(biodiversity.clean)
      
     # list.biodiv[[rownum]] <- biodiversity.clean
    }
    
  } # end if this row !0
  
  }


stopCluster(cl)

biodiversity.goc <- point.data %>% 
  rbindlist %>%   
  distinct(species, lat, lon,.keep_all= TRUE) %>% 
  na.omit

setwd(workpath)

write.csv(biodiversity.goc,"point_species_ocurrence.csv")
print(paste("FINAL records retreived:", nrow(biodiversity.goc),sep= " "))


## @knitr checktaxonomy    

rm(list=ls())    

savepath = "~/Dropbox/Puget_sound"


setwd(savepath)

#biodiversity.goc <- fread("point_species_ocurrence.csv", header=TRUE, select=c("species","lat","lon","source"))

biodiversity.goc <- fread("point_species_ocurrence.csv", header=TRUE, select=c("species","lat","lon"))

qmplot(lon, lat, data = biodiversity.goc, maptype = "satellite", color = I("white"), source = "google",extent = "panel", zoom = 7)+
  #    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  labs(x = 'Longitude', y = 'Latitude')+
  theme(axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))

#eliminate sp. 
sppatterns <- c("->","sp.$","sp.1$","sp.2$","sp1$","sp2$","spp$","/","SPP$"," sp$","sp. 2$","sp. 1$","sp. 3$","sp. 4$"," species a$"," volume "," spp.$"," sp. ")

# obtain list of unique species from ocurrence list, eliminate extra white space
# assign NA as new specie sname for records not identified to species
unique.species <- biodiversity.goc %>%  tbl_df %>% 
  #setNames(c("original_name","lat","lon","source")) %>% 
  setNames(c("old_name","lat","lon")) %>% 
  mutate(old_name=trimws(old_name)) %>% # trim white spaces
  filter(!old_name=="NA") %>% 
  mutate(old_name=str_replace(old_name,"NA","")) %>% 
  mutate(old_name=str_replace(old_name,"\\?","")) %>% 
  mutate(wordnum = str_count(old_name,"\\S+")) %>% 
  filter(!wordnum==1) %>% 
  mutate(old_name = case_when(grepl(paste(sppatterns,collapse="|"), old_name)~ "NA", TRUE ~ as.character(old_name))) %>% 
  filter(!old_name=="NA") %>% 
  #known misspellings
  mutate(old_name = case_when(old_name=="Squalus sucklei"~ "Squalus suckleyi", 
                            old_name=="Hydrolagus collici"~ "Hydrolagus colliei",
                                    old_name=="Pleuronectes Pleuronectes isolepis"~ "Pleuronectes isolepis", 
                            old_name=="Eopsetta Eopsetta exilis"~ "Eopsetta exilis",
                            old_name=="Errex Errex zachirus"~ "Errex zachirus",
                            old_name=="Lycodopsis Lycodopsis pacifica"~ "Lycodopsis pacifica",
                            old_name=="Pleuronectes Pleuronectes vetulus"~ "Pleuronectes vetulus",
                            TRUE~old_name)) %>% 
  filter(!old_name=="") %>% 
  distinct(old_name, lat, lon,.keep_all= TRUE) %>% 
  filter(!old_name=="Unpublished name") %>% 
  filter(!old_name=="California gull") %>% 
  filter(!old_name=="Anaxeocythere edentata")

  
write.csv(unique.species, file="clean_species_ocurrence.csv")

# make string of species names to check for validity and correct spelling
valid_species <- unique.species %>% 
  distinct(old_name) %>% 
.$old_name %>% 
  as.character() %>% trimws() 

valid_species <- unique(valid_species[valid_species != ""])


#first check taxonomic validity of names reviewed manually

# if(nrow(itis.class.sp)==0){
#   
#   itis.name <- try(gnr_resolve(names = name_match, canonical=TRUE, best_match_only=TRUE))#preferred_data_sources = c(3,11,1,5,6,52,53,59,60,62,70,83)))#85,91,92,93,95,97,98,99)))
#   
#   if(!nrow(itis.name)==0){
#     valid_name <- itis.name %>% select(matched_name2) %>% .$matched_name2 %>% as.character
#     itis.class.sp <- itis.class.sp %>% mutate(species=valid_name)
#   }
# }

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
        
        worms.clean <- worms.name  %>% tbl_df %>% 
          filter(grepl(name_match,scientificname)) 
        
        if(nrow(worms.clean)>0 && str_count(name_match,"\\S+")==2){
          worms.clean <- worms.clean %>% filter(rank=="Species")
        } 
        
        if(nrow(worms.clean)>0 && str_count(name_match,"\\S+")==2){
          worms.clean <- worms.clean %>% filter(scientificname==name_match)
        }
        
        if(nrow(worms.clean)!=0){
          
          worms.clean %>% distinct(valid_name, .keep_all=TRUE)%>% 
            mutate(old_name = name_match) %>% 
            select(old_name,kingdom,class,order,family,genus,valid_name) %>% 
            mutate(match= TRUE) %>% 
            mutate(counter=counter) %>% 
            mutate(source="WORMS")
        } else {
          
          matrix("NA",1,6) %>% 
            data.frame %>% tbl_df %>% 
            setNames(c("old_name","kingdom","class","order","family","genus")) %>% 
            mutate_each(as.character()) %>% 
            mutate(old_name=name_match) %>% 
            mutate(valid_name = name_match) %>% 
            mutate(match= FALSE) %>% 
            mutate(counter=counter)%>% 
            mutate(source="WORMS")
        }
        
        
      } else {
        
        matrix("NA",1,6) %>% 
          data.frame %>% tbl_df %>% 
          setNames(c("old_name","kingdom","class","order","family","genus")) %>% 
          mutate_each(as.character()) %>% 
          mutate(old_name=name_match) %>% 
          mutate(valid_name = name_match) %>% 
          mutate(match= FALSE) %>% 
          mutate(counter=counter)%>% 
          mutate(source="WORMS")
      }
    }) %plan% multiprocess
    
    
    itis.value <- future({
      
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
              
              itis.class.sp <- itis.class %>% filter(matchtype=="HIGHERRANK") %>% mutate(species=name_match)
              
            }
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
              mutate(counter=counter) %>% 
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
            mutate(counter=counter)%>% 
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
    
    
    accepted.name <- value(worms.value) %>% bind_rows(value(itis.value))
    
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
  select(-counter) 


print(head(names_goc))
print(str(names_goc))

write.csv(names_goc, file="scientific_names_valid.csv")


## @knitr selectspecies
#' changes synonyms to valid names
#' preference to ITIS

rm(list=ls())    

workpath = "~/Dropbox/Puget_sound"

setwd(workpath)

biodiversity.goc <- fread("clean_species_ocurrence.csv", select=c("old_name","lon","lat"), header = TRUE) %>% tbl_df %>% 
  setNames(c("old_name","lon","lat"))

old_species <- biodiversity.goc %>% 
  distinct(old_name, .keep_all=TRUE) %>% 
  # filter(old_name=="Jatropha canescens (benth.) müll. arg.") %>% 
  mutate(old_name=enc2utf8(old_name))

classif.taxa <- fread("scientific_names_valid.csv",header= TRUE, encoding = "UTF-8") %>% tbl_df %>%
  filter(match==TRUE) %>% 
  distinct(old_name, .keep_all=TRUE) %>% 
  dplyr::select(-source, -V1, -valid_name) %>% 
  #  filter(old_name=="Jatropha canescens (benth.) müll. arg.") %>% 
  mutate(old_name=enc2utf8(old_name))

missing.class <- anti_join(old_species,classif.taxa,by="old_name")

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
  distinct(old_name, .keep_all=TRUE) %>% left_join(classif.taxa,by="old_name") %>% 
  filter(!is.na(kingdom))
  


clean.records <- biodiversity.goc %>% 
 left_join(.,final.taxonomy,by="old_name") %>% 
  dplyr::select(-old_name, -match) %>% 
  distinct(valid_name, lat, lon, .keep_all=TRUE)

setwd(workpath)
write.csv(clean.records, file="biodiversity_recs_Puget_sound.csv")

#make summary table
clean.records %>% 
  distinct(order)
 