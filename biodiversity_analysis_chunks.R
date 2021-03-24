#' Gather species ocurrence records and generate richness model
#' 
#' chunks that feed into biodiversity_article.Rmd
#' @author Hem Nalini Morzaria Luna , \email{hmorzarialuna@gmail.com}
#' @keywords biodiversity, richness
#' last reviewed March 2016

# setpreferences: library prefs
# pointgrid: develop point grid for the Mexican Pacific
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
              "rvertnet", "httr","wesanderson","cowplot","rbison","rebird","taxize","readr","rgdal","XML", 
              "stringr","MaxentVariableSelection","rvest","robis","ridigbio","R.utils","plyr","geosphere","magrittr",
              "taxizesoap","future", "parallel", "doSNOW","RODBC","rredlist")

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
workpath = "~/Dropbox/Biodiversity"
shapepath = "~/Dropbox/Biodiversity/Analysis/SIG_Biodiversity"
savepath = "~/Dropbox/Biodiversity/Analysis"
ulloafiles="~/Dropbox/Biodiversity/Datos/Ulloa_datos" #put path
datafiles="~/Dropbox/Biodiversity/Datos/Ocurrencia_especies"

# workpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/RCode"
# shapepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/SIG_Biodiversity"
# savepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis"
# ulloafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Datos/Ulloa_datos" #put path
# datafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Datos/Ocurrencia_especies"

# projections
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
crs.lcc <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

setwd(shapepath)
#read in Gulf of California shapefile
#goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")#without wetlands
goc.shape <- readOGR(".", "Golfo_california_wetland_poly_WGS84")#with wetlands

setwd(workpath)

#'create bounding polygon for the Gulf of California
#'upper.left = c(32.139900, -115.142516)
#'lower.left = c(20.164036, -115.142516)
#'lower.right = c(20.164036, -104.95342)
#'upper.right = c(32.139900, -104.95342)

#lat = c(32.139900,20.164036,20.164036,32.139900)
#lon = c(-115.142516, -115.142516, -104.95342, -104.95342)


maxlat <- 32.139900
minlat <- 20.164036
maxlon <- -115.142516
minlon <- -104.95342

lat <- c(maxlat,minlat,minlat,maxlat)
lon <- c(maxlon,maxlon,minlon,minlon)

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
file.remove("/home/atlantis/biodiversity/Analysis/gocpolygon.kml")

writeOGR(goc.pol, dsn="/home/atlantis/biodiversity/Analysis/gocpolygon.kml", layer= "goc.pol", driver="KML", dataset_options=c("NameField=name"))

goc.pol.lcc <- goc.pol %>% spTransform(.,crs.lcc)

#' create point grid 
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

 
#' Record retrieval
#' First retrieve from data bases that don't take bounding boxes
#' this is FishBase 
#' retrieve all records for an ecosystem

site_list = c(165,132,9,239) # 144 Gulf of Mexico, 132 California Current, 145 Caribbean
region_list = c("Gulf of California","California Current", "Pacific Ocean","Pacific Central")
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
                "stringr","rredlist", "MaxentVariableSelection","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
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

setwd(savepath)
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


setwd(savepath)
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

counter <- 1
oldlength <- 1:length(poly.data)

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

#if there are polygons with >200 k records, but they are at the edges on land, leave
test.2000 <- biodiversity.sp %>% filter(rownum>199000) %>% distinct(polnum)

setwd(savepath)  

biodiversity.clean <- biodiversity.sp %>% 
  distinct(species, lat, lon, source) %>% 
  na.omit

write.csv(biodiversity.clean,file="record_queries_gbif.csv")

print(paste("GBIF Records retreived:", nrow(biodiversity.sp),sep= " "))
print(paste("Clean GBIF Records retreived:", nrow(biodiversity.clean),sep= " "))



#' Query ecoengine

print("Now querying ecoengine")

biodiversity <- list()

eachbox <- 1
pagesize <- 1000

# uses paging so one big bounding box can retrieve all records
thisbox = bbox.data.eco[eachbox]
print(paste("Analyzing record",thisbox, "is point",eachbox,":",length(bbox.data.eco)))

ee.data.frame <- try(ee_observations(page_size=pagesize, georeferenced = TRUE,  bbox = thisbox))

total_pages <- ee_pages(ee.data.frame)

total_pages <- ceiling(ee.data.frame$results / pagesize)

if(total_pages > 0){
  
  grouped_pages <- split(1:total_pages, ceiling(1:total_pages/pagesize))
  
  all_data <- llply(grouped_pages[1], function(x) {
    ee_observations(page_size=1000, georeferenced = TRUE, bbox = thisbox, page = x, progress = FALSE, quiet = FALSE)
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
    distinct(species, lat, lon, .keep_all=TRUE) %>% 
    na.omit
  
  test.2000 <- biodiversity.sp %>% filter(rownum>10000) %>% distinct(polnum)
  
  
  biodiversity.clean <- biodiversity.sp %>% 
    rbindlist %>% 
    distinct(species, lat, lon, .keep_all=TRUE) %>% 
    na.omit

setwd(savepath)
write.csv(biodiversity.clean,file="record_queries_vertnet.csv")

print(paste("Vertnet Records retreived:", nrow(biodiversity.sp),sep= " "))
print(paste("Clean Vertnet Records retreived:", nrow(biodiversity.clean),sep= " "))


# get ebird records
print("Now querying ebird")

biodiversity.sp <-  list()

regions.ebird = c('MX-SON','MX-BCN','MX-SIN','MX-BCS','MX-NAY','MX-JAL','MX-COL','MX-CAM')
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

setwd(savepath)
#write table
print(paste("Retrieved OBIS",nrow(biodiversity.sp)))
print(paste("Clean retrieved OBIS",nrow(biodiversity.clean)))

write.csv(biodiversity.clean, file="record_queries_obis.csv")

#' gather preexisting data

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

setwd(savepath)
#write table
print(paste("Retrieved CSV files",nrow(biodiversity.sp)))
write.csv(biodiversity.sp, file="record_queries_csv.csv",row.names=FALSE)

#' read in xls files

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

setwd(savepath)
#write table
print(paste("Retrieved XLS",nrow(biodiversity.sp)))
write.csv(biodiversity.sp, file="record_queries_xls.csv")

#' CONABIO data
# these were the only groups with species-level data
setwd(datafiles)

biodiversity.sp <- list.files(pattern = "*.txt")%>% 
  fread(header=FALSE) %>% 
  tbl_df %>% 
  dplyr::select(V5, V6, V7, V8) %>% 
  mutate(source="conabio") %>% 
  mutate(species=paste(V5,V6,sep=" ")) %>% 
  select(species,V7,V8,source) %>% 
  setnames(c("species","lon","lat","source")) %>% 
  distinct(species, lon, lat, source) %>% 
  na.omit
    
setwd(savepath)
#write table
print(paste("Retrieved Conabio",nrow(biodiversity.sp)))
write.csv(biodiversity.sp, file="record_queries_txt.csv",row.names=FALSE)



## @knitr combinebiodiversity
#' combine results into one file

rm(list=ls())    

datafiles="~/Dropbox/Biodiversity/Datos/Ocurrencia_especies"
savepath = "~/Dropbox/Biodiversity/Analysis"
# savepath ="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
shapepath = "~/Dropbox/Biodiversity/Analysis/SIG_Biodiversity"

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
  rbindlist %>% tbl_df %>% 
  mutate_each(funs(as.numeric),lat,lon) %>% 
  distinct(species, lat, lon, .keep_all=TRUE)
 

print(paste("Total Records retreived:", nrow(biodiversity.all),sep= " "))

setwd(savepath)
write.csv(biodiversity.all,"record_queries_combined.csv")

## @knitr organizebiodiversity
#' subset only data in the Gulf
#' iterates on sets of data otherwise insufficient memory
#' create new data frame for clean records

rm(list=ls())    

datafiles="~/Dropbox/Biodiversity/Datos/Ocurrencia_especies"
savepath = "~/Dropbox/Biodiversity/Analysis"
# savepath ="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
shapepath = "~/Dropbox/Biodiversity/Analysis/SIG_Biodiversity"

setwd(savepath)


# projections
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

setwd(shapepath)
#read in Gulf of California shapefile
#goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")#without wetlands
goc.shape <- readOGR(".", "Golfo_california_wetland_poly_WGS84")#with wetlands

setwd(savepath)
biodiversity.all <- fread("record_queries_combined.csv")

biodiversity.row  <-  1:nrow(biodiversity.all)

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

point.data <- foreach(rownum = biodiversity.row, .verbose = T) %dopar% {
  
  .packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
                "fields","data.table","rgbif","raster", "rasterVis",
                "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
                "rvertnet", "httr","tidyr","rbison","rebird","taxize","readr","rgdal","XML", 
                "stringr","rvest","robis","ridigbio","R.utils","taxizesoap","future", "doSNOW")
  
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
        distinct(species, lat, lon, .keep_all=TRUE) %>% 
        na.omit
      
      return(biodiversity.clean)
      
      
    }
    
  } # end if this row !0
  
}

stopCluster(cl)

biodiversity.goc <- point.data %>% 
  rbindlist %>%   
  distinct(species, lat, lon, .keep_all=TRUE) %>% 
  na.omit

qmplot(lon, lat, data = biodiversity.goc, maptype = "satellite", color = I("white"), source = "google",extent = "panel", zoom = 3)+
  #    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  labs(x = 'Longitude', y = 'Latitude')+
  theme(axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))

setwd(savepath)

write.csv(biodiversity.goc,"point_species_ocurrence.csv")
print(paste("FINAL GOC records retreived:", nrow(biodiversity.goc),sep= " "))


## @knitr checktaxonomy    

rm(list=ls())    

datafiles="~/Dropbox/Biodiversity/Datos/Ocurrencia_especies"
savepath = "~/Dropbox/Biodiversity/Analysis"
# savepath ="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
shapepath = "~/Dropbox/Biodiversity/Analysis/SIG_Biodiversity"


setwd(savepath)

biodiversity.goc <- fread("point_species_ocurrence.csv", header=TRUE, select=c("species","lat","lon","source"))

# biodiversity.goc <- fread("point_species_ocurrence.csv", header=TRUE, select=c("species","lat","lon"))

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
  setNames(c("old_name","lat","lon","source")) %>% 
  mutate(old_name=trimws(old_name)) %>% # trim white spaces
  mutate(old_name=str_replace(old_name,"NA","")) %>% 
  mutate(old_name=str_replace(old_name,"\\?","")) %>% 
  mutate(wordnum = str_count(old_name,"\\S+")) %>% 
  filter(!wordnum==1) %>% 
  mutate(old_name = case_when(grepl(paste(sppatterns,collapse="|"), old_name)~ "NA", TRUE ~ as.character(old_name))) %>% 
  filter(!old_name=="NA") %>% 
  filter(!grepl("Unknown", old_name)) %>% 
  filter(!grepl("Unpublished", old_name)) %>% 
  filter(!grepl("Otros ", old_name)) %>% 
  filter(!grepl("Unidentified", old_name)) %>% 
  #known misspellings
  filter(!old_name=="") %>% 
  filter(!old_name=="Colonial diatoms") %>% 
  mutate(old_name = case_when(old_name=="Salicornia pacÃfica"~ "Salicornia pacifica", 
                              old_name=="Hepatus lineatus"~ "Hepatus lineatus",
                              old_name=="Acthrocnemun subterminalis"~ "Arthrocnemum subterminale", 
                              TRUE~old_name)) %>% 
  distinct(old_name, lat, lon,.keep_all= TRUE) %>% 
  na.omit


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

oldlength <- length(valid_species)

# gave preference to WORMS but also got names from ITIS

names_function <-  function(species){
  
    name_match <- species %>% 
      trimws()
    
    print(paste("Analyzing",name_match))
    
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
            mutate(source="WORMS")
        } else {
          
          matrix("NA",1,6) %>% 
            data.frame %>% tbl_df %>% 
            setNames(c("old_name","kingdom","class","order","family","genus")) %>% 
            mutate_each(as.character()) %>% 
            mutate(old_name=name_match) %>% 
            mutate(valid_name = name_match) %>% 
            mutate(match= FALSE) %>% 
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

names_goc_list <- mclapply(valid_species,names_function)


names_goc <- names_goc_list %>% 
  rbindlist %>% 
  tbl_df 

#these are names that could not be matched and have to be checked manually
#identification errors and some fossil taxa

missing.taxa <- names_goc %>% 
  filter(match==FALSE)

write_csv(missing.taxa,"classif_missing_taxa.csv")

print(lenght(missing.taxa))
  
print(head(names_goc))
print(str(names_goc))

write.csv(names_goc, file="scientific_names_valid.csv")



## @knitr selectspecies
#' changes synonyms to valid names
#' preference to ITIS

rm(list=ls())    

datafiles="~/biodiversity/Datos/Ocurrencia_especies"
savepath = "~/biodiversity/Analysis"
# savepath ="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
shapepath = "~/biodiversity/Analysis/SIG_Biodiversity"

setwd(savepath)

biodiversity.goc <- fread("clean_species_ocurrence.csv", select=c("old_name","lon","lat"), header = TRUE) %>% tbl_df %>% 
  setNames(c("old_name","lon","lat"))

old_species <- biodiversity.goc %>% 
  distinct(old_name) %>% 
 # filter(old_name=="Jatropha canescens (benth.) müll. arg.") %>% 
  mutate(old_name=enc2utf8(old_name)) 

missing.species <- read_csv("classif_missing_taxa_rev.csv") %>% 
  filter(match==TRUE) %>% 
  dplyr::select(-match,-source)

classif.taxa <- fread("scientific_names_valid.csv",header= TRUE, encoding = "UTF-8") %>% tbl_df %>%
#  filter(kingdom!="NA") %>% 
  distinct(old_name,valid_name,.keep_all=TRUE) %>% 
  dplyr::select(-source, -V1, -match) %>% 
  mutate(old_name=enc2utf8(old_name)) %>% 
  bind_rows(missing.species)

invert.taxonomy <- read_excel("sinonimias_Macroinvertebrados-RCB.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0) %>% 
  tbl_df() %>% 
  dplyr::rename(old_name=Synonym) %>% 
  inner_join(classif.taxa, by="old_name") %>% 
  dplyr::select(-valid_name) %>% 
  dplyr::rename(valid_name = Species) %>% 
  distinct(old_name, valid_name, .keep_all=TRUE)


write_csv(final.taxonomy,"final_species_list.csv")

#rows in final.taxonomy should match rows in update.inverts
#originally there were inconsistencies in the invert synonym database

# these are invert species that could be assigned to two different valid names
# eliminate because we can't tell what the valid name is
problem.species <- read_csv("conflict_species.csv") %>% 
  dplyr::select(old_name) %>% 
  .$old_name

#used this to find misspelled names, those still with authorities
# final.taxonomy <- anti_join(classif.taxa,invert.taxonomy, by="old_name") %>%
#   bind_rows(invert.taxonomy) %>%
#   distinct(old_name,valid_name, .keep_all = TRUE) %>%
#   mutate(valid_name=ifelse(valid_name=="Zostera (Zostera) marina","Zostera marina", valid_name)) %>%
#   mutate(valid_name=ifelse(valid_name=="Rhizophora racemosa","Rhizophora mangle", valid_name)) %>%
#   filter(!old_name %in% problem.species) %>%
#   arrange(kingdom,class,order,family,genus,valid_name) %>%
#   filter(old_name!="Pareques species A") %>%
#   mutate(wordnum = as.numeric(str_count(valid_name,"\\S+"))) %>%
#   filter(wordnum==1) %>%
#   dplyr::select(valid_name,everything()) %>%
#   write_csv("misspelled_species.csv")
  
ms.species <- read_csv("mispelled_species.csv") %>% tbl_df %>% 
  dplyr::select(nombre_original,nombre_valido) %>% 
  dplyr::rename(old_name=nombre_original) %>% 
  arrange(old_name) %>% 
  distinct(old_name, nombre_valido, .keep_all=TRUE)

ms.taxonomy <- anti_join(classif.taxa,invert.taxonomy, by="old_name") %>%
  bind_rows(invert.taxonomy) %>%
  distinct(old_name,valid_name, .keep_all = TRUE) %>%
  mutate(valid_name=ifelse(valid_name=="Zostera (Zostera) marina","Zostera marina", valid_name)) %>%
  mutate(valid_name=ifelse(valid_name=="Rhizophora racemosa","Rhizophora mangle", valid_name)) %>%
  filter(!old_name %in% problem.species) %>%
  arrange(kingdom,class,order,family,genus,valid_name) %>%
  filter(old_name!="Pareques species A") %>%
  mutate(wordnum = as.numeric(str_count(valid_name,"\\S+"))) %>% 
  right_join(ms.species, by="old_name") %>% 
  dplyr::select(-valid_name, -wordnum) %>% 
  dplyr::rename(valid_name=nombre_valido) %>% 
  distinct(valid_name,old_name, .keep_all=TRUE) %>% 
  filter(!is.na(valid_name))

final.taxonomy <- anti_join(classif.taxa,invert.taxonomy, by="old_name") %>%
  bind_rows(invert.taxonomy) %>%
  distinct(old_name,valid_name, .keep_all = TRUE) %>%
  mutate(valid_name=ifelse(valid_name=="Zostera (Zostera) marina","Zostera marina", valid_name)) %>%
  mutate(valid_name=ifelse(valid_name=="Rhizophora racemosa","Rhizophora mangle", valid_name)) %>%
  filter(!old_name %in% problem.species) %>%
  arrange(kingdom,class,order,family,genus,valid_name) %>%
  filter(old_name!="Pareques species A") %>%
  anti_join(ms.species, by="old_name") %>% 
  bind_rows(ms.taxonomy) %>% 
  distinct(valid_name,old_name, .keep_all=TRUE) %>% 
  filter(!is.na(valid_name))


distinct.species <- final.taxonomy%>% 
  distinct(valid_name, .keep_all=TRUE)

distinct.class <- final.taxonomy%>% 
  distinct(valid_name, .keep_all=TRUE) %>% 
  distinct(family, .keep_all=TRUE)


clean.records <- biodiversity.goc %>% 
  left_join(final.taxonomy,by="old_name") %>% 
  filter(!is.na(valid_name)) %>% 
  dplyr::select(-old_name) %>% 
  mutate(wordnum = str_count(valid_name,"\\S+")) %>% 
  filter(wordnum!=1) %>% 
 # mutate(valid_name=trimws(valid_name)) %>% 
  distinct(valid_name, lat, lon, .keep_all=TRUE) %>% 
  separate(valid_name,c("one","two"),sep=" ", remove=FALSE, extra="drop") %>% 
  mutate(genus=if_else(is.na(genus),one,genus)) %>% 
  dplyr::select(-one,-two) %>% 
  mutate(index=1:nrow(.)) %>% 
  distinct(valid_name, lat, lon, .keep_all=TRUE)
  
  
#saved classification that was missing.Ari checked bny hand
# clean.records %>%
# filter(is.na(family) | is.na(class) | is.na(kingdom)) %>%
# distinct(kingdom,class,order,family,genus,valid_name) %>%
# as.data.frame() %>%
# write.csv("fix_taxonomy.csv")

new.taxonomy <- read_csv("fix_taxonomy.csv") %>% dplyr::select(-X1)

fixed.taxonomy <- clean.records %>%
  filter(is.na(family) | is.na(class) | is.na(kingdom)) %>%
  distinct(lon,lat,kingdom,class,order,family,genus,valid_name) %>%
  dplyr::select(lon,lat,valid_name) %>% 
  left_join(new.taxonomy, by="valid_name")
 
updated.records <- clean.records %>% 
  filter(!is.na(family) & !is.na(class) & !is.na(kingdom)) %>%
  dplyr::select(-wordnum, -index) %>% 
  dplyr::select(lon,lat,valid_name,everything()) %>% 
  bind_rows(fixed.taxonomy) %>% 
  mutate(class=trimws(class),kingdom=trimws(kingdom)) %>% 
  mutate(kingdom=if_else(kingdom=="Aninalia","Animalia",kingdom))

records.kingdom <-table(updated.records$kingdom) %>% as.data.frame() %>% tbl_df %>% 
  setNames(c("kingdom","no_records"))

taxa.table <- updated.records %>% 
  distinct(kingdom,class,order,family,valid_name)

table(updated.records$valid_name) %>% as.data.frame() %>% tbl_df %>% 
  setNames(c("valid_name","no_records")) %>% 
  arrange(desc(no_records)) %>% 
  mutate(cumfre=cumsum(no_records),cumprop=cumfre/286533) %>% 
  filter(cumprop<=0.1) %>% 
  left_join(taxa.table, by="valid_name") %>% 
  write_csv("high_freq_species.csv")

updated.records %>% 
  ggplot(aes(x = kingdom)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# save final taxonomy table
species.kingdom <- updated.records %>% 
   distinct(kingdom,class,order,family,genus,valid_name) %>% 
   mutate(index=1) %>% 
  group_by(kingdom) %>% 
  summarise(no_species=sum(index))

family.kingdom <- updated.records %>% 
  distinct(kingdom,class,order,family) %>% 
  mutate(index=1) %>% 
  group_by(kingdom) %>% 
  summarise(no_family=sum(index))

order.kingdom <- updated.records %>% 
  distinct(kingdom,class,order) %>% 
  mutate(index=1) %>% 
  group_by(kingdom) %>% 
  summarise(no_order=sum(index))

class.kingdom <- updated.records %>% 
  distinct(kingdom,class) %>% 
  mutate(index=1) %>% 
  group_by(kingdom) %>% 
  summarise(no_class=sum(index))

taxa.summary <- records.kingdom %>% left_join(species.kingdom,by="kingdom") %>% 
  left_join(family.kingdom,by="kingdom") %>% 
  left_join(order.kingdom,by="kingdom") %>% 
  left_join(class.kingdom,by="kingdom") 

write_csv(taxa.summary,"final_taxonomy_table.csv")

setwd(savepath)

write.csv(updated.records, file="biodiversity_recs.csv")



## @knitr richnessmodel
# interpolation model

rm(list=ls())  

# List of packages for session
.packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
              "PBSmapping", "fields","data.table","rgbif","raster", "rasterVis",
              "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
              "rvertnet", "httr","wesanderson","cowplot","rbison","rebird","taxize","rgdal","XML", 
              "MaxentVariableSelection","rvest","robis","ridigbio","R.utils","geosphere",
              "taxizesoap","future", "parallel", "doSNOW","RODBC","rredlist","stringr")

.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])


# Load packages into session 
lapply(.packages, require, character.only=TRUE)


datafiles="~/biodiversity/Datos/Ocurrencia_especies"
savepath = "~/biodiversity/Analysis"
# savepath ="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
shapepath = "~/biodiversity/Analysis/SIG_Biodiversity"

setwd(shapepath)
#read in Gulf of California shapefile
#goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")
goc.shape <- readOGR(".", "Golfo_california_wetland_poly_WGS84")#with wetlands

setwd(savepath)

biodiversity <- fread("biodiversity_recs.csv") %>% dplyr::select(lon,lat,valid_name) %>% 
  setNames(c("long","lat","speciesID")) %>% na.omit

full.records <- fread("biodiversity_recs.csv") 

biodi.shp <- biodiversity

coordinates(biodi.shp) <- c("long","lat")

crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
proj4string(biodi.shp) <- crs.geo.wgs # define projection

file.remove("/home/atlantis/biodiversity/Analysis/occurrence_points.shp")
writeOGR(biodi.shp, dsn="/home/atlantis/biodiversity/Analysis/occurrence_points.shp", layer= "biodi.shp", driver="ESRI Shapefile")

myLocation<-c(-115.142516, 20.164036, -104.95342, 32.139900)
map <- get_map(myLocation, zoom = 6, maptype = "terrain", source = "google")


mapPoints <- ggmap(map) +
  geom_point(aes(x = long, y = lat), size = 0.25, color = "royalblue4",data = biodiversity, alpha = .5)


# chart <- ggplot(data=ct_mod,aes(long,lat,group=group)) # the group is the issue, should not be used here as the hosp.list will also be looked for group, which does not exist
chart <- ggplot(data=biodiversity,aes(long,lat))

final.species <- biodiversity %>% 
  mutate(speciesID=as.factor(speciesID)) %>% 
  distinct(speciesID) %>% 
  arrange(speciesID)

sp.richness.list <- biodiversity %>% 
  distinct(speciesID) %>% .$speciesID


# use this code to check entries for individual species 
# species.list <- biodiversity %>% distinct(speciesID) %>% .$speciesID %>% as.character()
# counter <- 4089


# this.species <- species.list[counter]
# print(this.species)
# print(biodiversity %>% filter(speciesID==this.species))
# counter <- counter+1

#set extent
resolution= 0.08
shift = biodiversity %>%  getShift(.)
dimension = biodiversity %>%  getDimension(., resolution)

#create null masks
landwatermask = createLandwatermask(NULL,dimension,shift,resolution)
heightwatermask = createHeightmask(NULL,dimension,shift,resolution)

narrow.rare.species.nrs <- getNarrowEndemics(biodiversity,
                                             all.species=-1,
                                             narrow.endemic.limit = 5,
                                             dimension,
                                             shift,
                                             resolution)

rare.sp <- biodiversity %>% 
  filter(speciesID %in% narrow.rare.species.nrs) %>% 
  tbl_df

rare.sp %>% distinct(speciesID)

write_csv(rare.sp, "rare_species.csv")

rare.shp <- fread("rare_species.csv") %>% tbl_df %>%  na.omit

coordinates(rare.shp) <- c("long","lat")

crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
proj4string(rare.shp) <- crs.geo.wgs # define projection

file.remove("/home/atlantis/biodiversity/Analysis/rare_occurrence_points.shp")
writeOGR(rare.shp, dsn="/home/atlantis/biodiversity/Analysis/rare_occurrence_points.shp", layer= "rare.shp", driver="ESRI Shapefile")

# Threat

# Runs search for threathened species on IUCN
# run again if needed

# #get IUCN Red List
#
#   getredlist <- function(taxa){
#  
#     iucn.cat <- c("VU","EN","CR")
#     print(taxa)
#     iucn.res <- rl_search(taxa, parse = TRUE, key="cb6904ea802695a639b3bc2b8b53b7be189cd97f896ac912ac1acf959b607556")
#     iucn.frame <- iucn.res$result
#  
#     if(length(iucn.frame!=0)){
#       iucn.frame %>%
#         mutate(iucn = if_else(category %in% iucn.cat, 1,0))
#     }
#   }
#  
#   iucn.species <- biodiversity %>%
#      distinct(speciesID) %>%
#     .$speciesID %>%
#     lapply(.,getredlist)
#   
#   iucn.data <- iucn.species[!sapply(iucn.species, is.null)] %>%
#     bind_rows %>%
#     tbl_df %>%
#     filter(iucn==1) %>%
#     distinct(scientific_name) %>%
#     .$scientific_name
#  
#   write.csv(iucn.data,"iucn_data.csv")
#   
# 
#   names_function <-  function(name_match){
#  
#       print(paste("Analyzing",name_match))
#  
#       worms.value <- future({
#  
#  
#         worms.name <- try(worms_records(scientific= name_match))
#  
#  
#         if(nrow(worms.name)!=0){
#  
#           worms.clean <- worms.name  %>% tbl_df %>%
#             filter(grepl(name_match,scientificname))
#  
#           if(nrow(worms.clean)>0 && str_count(name_match,"\\S+")==2){
#             worms.clean <- worms.clean %>% filter(rank=="Species")
#           }
#  
#           if(nrow(worms.clean)>0 && str_count(name_match,"\\S+")==2){
#             worms.clean <- worms.clean %>% filter(scientificname==name_match)
#           }
#  
#           if(nrow(worms.clean)!=0){
#  
#             worms.clean %>% distinct(valid_name, .keep_all=TRUE)%>%
#               mutate(old_name = name_match) %>%
#               select(old_name,kingdom,class,order,family,genus,valid_name) %>%
#               mutate(match= TRUE) %>%
#               mutate(source="WORMS")
#           } else {
#  
#             matrix("NA",1,6) %>%
#               data.frame %>% tbl_df %>%
#               setNames(c("old_name","kingdom","class","order","family","genus")) %>%
#               mutate_each(as.character()) %>%
#               mutate(old_name=name_match) %>%
#               mutate(valid_name = name_match) %>%
#               mutate(match= FALSE) %>%
#               mutate(source="WORMS")
#           }
#  
#  
#         } else {
#  
#           matrix("NA",1,6) %>%
#             data.frame %>% tbl_df %>%
#             setNames(c("old_name","kingdom","class","order","family","genus")) %>%
#             mutate_each(as.character()) %>%
#             mutate(old_name=name_match) %>%
#             mutate(valid_name = name_match) %>%
#             mutate(match= FALSE) %>%
#             mutate(source="WORMS")
#         }
#       }) %plan% multiprocess
#  
#  
#       itis.value <- future({
#  
#         itis.class <- try(name_match %>% get_gbifid_(.) %>% rbindlist() %>% tbl_df)
#  
#  
#         if(!inherits(itis.class, "try-error")){
#  
#           if(!nrow(itis.class)==0){
#  
#             itis.class.sp <- itis.class %>% filter(status ==  "ACCEPTED" & matchtype=="EXACT")
#  
#             if(nrow(itis.class)!=0 && nrow(itis.class.sp)==0){
#  
#               itis.class.sp <- itis.class %>% filter(matchtype=="EXACT"| matchtype=="FUZZY")  %>% filter(canonicalname==name_match)
#  
#               if(nrow(itis.class.sp)==0){
#  
#                 itis.class.sp <- itis.class %>% filter(status ==  "ACCEPTED" & matchtype=="FUZZY")
#  
#               }
#  
#               if(nrow(itis.class.sp)==0){
#  
#                 itis.class.sp <- itis.class %>% filter(status ==  "SYNONYM" & matchtype=="FUZZY")
#  
#               }
#  
#               if(nrow(itis.class.sp)==0){
#  
#                 itis.class.sp <- itis.class %>% filter(status ==  "SYNONYM" & matchtype=="EXACT")
#  
#               }
#  
#               if(nrow(itis.class.sp)==0){
#  
#                 itis.class.sp <- itis.class %>% filter(matchtype=="EXACT")
#  
#               }
#  
#               if(nrow(itis.class.sp)==0){
#  
#                 itis.class.sp <- itis.class %>% filter(matchtype=="FUZZY")
#  
#               }
#  
#               if(nrow(itis.class.sp)==0){
#  
#                 itis.class.sp <- itis.class %>% filter(matchtype=="HIGHERRANK")
#  
#                 if(!any(colnames(itis.class.sp)=="species")){
#  
#                   itis.class.sp <- itis.class.sp %>% mutate(species=name_match)
#                 }
#  
#               }
#             }
#  
#  
#             if(!any(colnames(itis.class.sp)=="kingdom")){
#  
#               itis.class.sp <- itis.class.sp %>% mutate(kingdom="NA")
#             }
#  
#             if(!any(colnames(itis.class.sp)=="order")){
#  
#               itis.class.sp <- itis.class.sp %>% mutate(order="NA")
#             }
#  
#             if(!any(colnames(itis.class.sp)=="class")){
#  
#               itis.class.sp <- itis.class.sp %>% mutate(class="NA")
#             }
#  
#             if(!any(colnames(itis.class.sp)=="family")){
#  
#               itis.class.sp <- itis.class.sp %>% mutate(family="NA")
#             }
#  
#             if(!any(colnames(itis.class.sp)=="genus")){
#               this.genus <- strsplit(name_match," ") %>% unlist %>% .[1]
#               itis.class.sp <- itis.class.sp %>% mutate(genus=this.genus)
#             }
#  
#  
#             if(!any(colnames(itis.class.sp)=="species")){
#  
#               itis.class.sp <- itis.class.sp %>% mutate(species=canonicalname)
#             }
#  
#             if(nrow(itis.class.sp)>1) itis.class.sp <- itis.class.sp[1,]
#  
#             if(nrow(itis.class.sp)!=0){
#  
#               itis.class.sp %>%
#                 mutate(old_name = name_match) %>%
#                 select(old_name,kingdom,class,order,family,genus,species) %>%
#                 setNames(c("old_name","kingdom","class","order","family","genus","valid_name")) %>%
#                 mutate(match= TRUE) %>%
#                 mutate(source="ITIS")
#  
#  
#             }
#           } else {
#  
#             genus.sp <-  name_match %>% strsplit(" ") %>% unlist %>% .[1]
#  
#             itis.class <- try(genus.sp %>% get_gbifid_(.) %>% rbindlist() %>% tbl_df) %>%
#               filter(status ==  "ACCEPTED" & matchtype=="EXACT")
#  
#             if(nrow(itis.class)==0){
#  
#               itis.class <- matrix("NA",1,5) %>% tbl_df() %>%
#                 setNames(c("kingdom","class","order","family","genus"))
#             }
#  
#             itis.class %>% .[1,] %>%
#               mutate(old_name = name_match) %>%
#               select(old_name,kingdom,class,order,family,genus) %>%
#               mutate(valid_name = name_match) %>%
#               mutate(match= FALSE) %>%
#               mutate(source="ITIS")
#           }
#  
#  
#         } else {
#  
#           genus.sp <-  name_match %>% strsplit(" ") %>% unlist %>% .[1]
#  
#           itis.class <- try(genus.sp %>% get_gbifid_(.) %>% rbindlist() %>% tbl_df) %>%
#             filter(status ==  "ACCEPTED" & matchtype=="EXACT")
#  
#           itis.class %>% .[1,] %>%
#             mutate(old_name = name_match) %>%
#             select(old_name,kingdom,class,order,family,genus) %>%
#             mutate(valid_name = name_match) %>%
#             mutate(match= FALSE) %>%
#             mutate(source="ITIS")
#         }
#  
#       }) %plan% multiprocess
#  
#       while (!resolved(itis.value)) {
#         print(cat("Resolving ITIS ...\n"))
#         Sys.sleep(0.2)
#       }
#  
#       while (!resolved(worms.value)) {
#         print(cat("Resolving WORMS ...\n"))
#         Sys.sleep(0.2)
#       }
#  
#  
#       accepted.name <- value(worms.value) %>%
#         bind_rows(value(itis.value))
#  
#      print(head(accepted.name))
#  
#     return(accepted.name)
#  
#   }
#  
#   
#   iucn.data <-  read_csv("iucn_data.csv") %>% 
#     dplyr::select(2) %>%
#     na.omit %>% 
#     setNames("species")
#   
#   NOM species
nom.species <- read_csv("nom.csv") %>%
  na.omit %>%
  setNames("species")

iucn.data <- read_csv("iucn_data.csv")
#   
#   threat.sp <- bind_rows(nom.species,iucn.data) %>% 
#     na.omit %>% .$species
# 
#   
#   nom.taxa <- lapply(threat.sp,names_function) %>%
#     bind_rows
#   
#   write_csv(nom.taxa,"threat_taxonomy.csv")

nom.taxa <- read_csv("threat_taxonomy.csv")

itis.species <- nom.taxa %>%
  filter(source=="ITIS") %>%
  filter(match==TRUE) %>%
  distinct(old_name, valid_name)

worms.species <-  nom.taxa %>%
  filter(source=="WORMS") %>%
  filter(match==TRUE) %>%
  distinct(old_name, valid_name)

only.itis <- anti_join(itis.species,worms.species,by="old_name")

both.taxonomy <- semi_join(itis.species,worms.species,by="old_name") #gives preference to itis,which is really
#  any database in taxize
only.worms <- anti_join(worms.species,itis.species,by="old_name")

classif.taxa <- nom.taxa %>%
  filter(match==TRUE) %>%
  distinct(old_name, .keep_all=TRUE) %>%
  dplyr::select(-source, -valid_name) %>%
  mutate(old_name=enc2utf8(old_name))

final.taxonomy <-  bind_rows(only.worms,only.itis,both.taxonomy) %>%
  distinct(old_name, .keep_all=TRUE) %>% left_join(classif.taxa,by="old_name") %>%
  filter(!is.na(kingdom)) %>%
  mutate(valid_name=ifelse(valid_name=="Zostera (Zostera) marina","Zostera marina", valid_name)) %>%
  mutate(valid_name=ifelse(valid_name=="Rhizophora racemosa","Rhizophora mangle", valid_name))

threat.clean.records <- nom.species %>%
  tbl_df %>% setNames("old_name") %>%
  left_join(.,final.taxonomy,by="old_name") %>%
  dplyr::select(-old_name, -match) %>%
  distinct(valid_name) %>%
  na.omit() %>%
  .$valid_name

#  count records species under threat

threat <- biodiversity %>%
  filter(speciesID %in% threat.clean.records) %>%
  tbl_df

write_csv(threat, "threatened_species.csv")

threat.shp <- fread("threatened_species.csv") %>% tbl_df %>%  na.omit

threat.sp <- threat.shp %>% distinct(speciesID) %>% .$speciesID

coordinates(threat.shp) <- c("long","lat")

crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
proj4string(threat.shp) <- crs.geo.wgs # define projection

file.remove("/home/atlantis/biodiversity/Analysis/threat_occurrence_points.shp")
writeOGR(threat.shp, dsn="/home/atlantis/biodiversity/Analysis/threat_occurrence_points.shp", layer= "threat.shp", driver="ESRI Shapefile")

# estimate endemic species

#count records species under threat

endemic.sp <- read_csv("especies_endemicas_final.csv") %>% 
  dplyr::select(ESPECIE) %>% 
  dplyr::rename(old_name=ESPECIE) %>% 
  mutate(old_name=trimws(old_name)) %>% 
  distinct(old_name) 

endemic.taxonomy <- read_csv("final_species_list.csv") %>% 
  right_join(endemic.sp,by="old_name") %>% 
  mutate(new_name=if_else(is.na(valid_name),old_name,valid_name)) %>% 
  dplyr::select(new_name) %>% 
  dplyr::rename(speciesID = new_name) %>% 
  .$speciesID

endemic <- biodiversity %>% 
  filter(speciesID %in% endemic.taxonomy) %>% 
  tbl_df

write_csv(endemic, "endemic_species.csv")

endemic.shp <- fread("endemic_species.csv") %>% tbl_df %>%  na.omit

endemic.sp <- endemic.shp %>% 
  distinct(speciesID) %>% 
  .$speciesID

coordinates(endemic.shp) <- c("long","lat")

crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
proj4string(endemic.shp) <- crs.geo.wgs # define projection

file.remove("/home/atlantis/biodiversity/Analysis/endemic_occurrence_points.shp")
writeOGR(endemic.shp, dsn="/home/atlantis/biodiversity/Analysis/endemic_occurrence_points.shp", layer= "rare.shp", driver="ESRI Shapefile")



#calculate inverse-distance weighted richness

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)
clusterEvalQ(cl, library("sperich"))

#center.list<- c("richness","threat","endemic", "rare")
center.list<- c("threat")


for(this.center in center.list){
  #get.hotspot <-function(species.list, this.distance, this.quantile, this.center){
  
   print(paste("Analyzing", this.center))
  
  if(this.center=="endemic") {
species.list=endemic.sp
this.distance=1:10
this.quantile=0.9
  }
  
  if(this.center=="rare") {
    species.list=narrow.rare.species.nrs
    this.distance=1:5
    this.quantile=0.9
  }

  if(this.center=="threat") {
    species.list=threat.sp
    this.distance=1:10
    this.quantile=0.9
  }
  
  if(this.center=="richness") {
    species.list=sp.richness.list
    this.distance=1:10
    this.quantile=0.9
  }
 
  print(paste("Analyzing", this.center, "these species:",length(species.list)))
  
 
  
  
  #map species ocurrence
  
  print("Analyzing noninterpolated grid")
  noninterpolatedgrid = createNonInterpolatedGrid(dataset.all.species = biodiversity,
                                                  dimension,shift,resolution,
                                                  all.species=species.list)
  
  save.image(file = paste("Routput_",this.center,".RData"))
  
  exportAsGDAL(noninterpolatedgrid, shift, resolution,
               directory=getwd(), filename=paste("pointgrid_",this.center,"_goc.tif",sep=""), drivername="GTiff")
  
  createImage(grid=noninterpolatedgrid,landwatermask,
              image.title = "GOC richness",
              directory=getwd(),
              filename=paste("pointgrid_",this.center,"_goc.png",sep=""),
              shift,
              part=15,
              resolution)
  
  non.data = raster(paste("pointgrid_",this.center,"_goc.tif",sep="")) %>%
    reclassify(cbind(0, NA)) %>%
    rasterToPoints %>%
    data.frame %>%
    setnames(c('Longitude', 'Latitude', 'Richness'))
  
  pal <- wes_palette(10, name = "FantasticFox1", type = "continuous")
  
  #plot richness model and robustness
  non.plot = ggplot(data=non.data, aes(y=Latitude, x=Longitude)) +
    geom_raster(aes(fill=Richness)) +
    theme_bw() +
    coord_equal() +
    scale_fill_gradientn(colours = pal)+
    theme(axis.title.x = element_text(size=13),
          axis.title.y = element_text(size=13, angle=90),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'right',
          legend.text = element_text(size=10),
          legend.title = element_text(size=12))
  
  print("Analyzing weights")
  
  species.richness.weighted = sperich::species.richness(biodiversity,
                                                        landwatermask=landwatermask,
                                                        distances=this.distance,
                                                        weight=0.5,
                                                        dimension=dimension,
                                                        shift=shift,
                                                        resolution,
                                                        upperbound=3000,
                                                        all.species=species.list,
                                                        silent=FALSE,
                                                        do.parallel=TRUE)

  save.image(file = paste("Routput_",this.center,".RData"))
  
  exportAsGDAL(species.richness.weighted, shift, resolution,
               directory=getwd(), filename=paste("weighted_",this.center,"_goc.tif",sep=""), drivername="GTiff")
  
  sperich::evaluate(result.grid.one=species.richness.weighted,
                    result.grid.two=NULL,
                    title.one="Histogram of species richness",
                    title.two=NULL,
                    xmax=400,
                    ymax=1000,
                    directory=getwd(),
                    filename=paste("histogram_weighted",this.center,"_goc.png",sep=""))
  
  
  print("Determining clusters")
  
  # adjust the result for sampling effort
  # determine clusters based on the 90 quantile
  clusterlimit <- species.richness.weighted %>% quantile(probs = c(this.quantile))
  
  
  # set clusterlimit to prepare clusterlist
  # clusterlimit is the number of species, below which clusters are not created.
  clusterlist=searchClusters(species.richness.weighted,
                             dimension,
                             shift,
                             resolution,
                             clusterlimit)
  
  save.image(file = paste("Routput_",this.center,".RData"))
  
  #adjust inverse-weighted species
  #richness grid for sampling effort
  
  print("Adjusting data for unequal sampling")
  
  species.richness.adjusted = adjustment(species.richness.weighted,
                                         noninterpolatedgrid,
                                         clusterlist)
  
  save.image(file = paste("Routput_",this.center,".RData"))
  
  #create a histogram of the result grid and calculates statistical scores like percentiles, median, and mean value.
  
  sperich::evaluate(result.grid.one=species.richness.adjusted,
                    result.grid.two=NULL,
                    title.one="Histogram of adjusted species richness",
                    title.two=NULL,
                    xmax=400,
                    ymax=1000,
                    directory=getwd(),
                    filename=paste("histogram_adjusted",this.center,"_goc.png",sep=""))
  
  exportAsGDAL(species.richness.adjusted, shift, resolution,
               directory=getwd(), filename=paste("adjusted_",this.center,"_goc.tif",sep=""), drivername="GTiff")
  
  
  createImage(grid=species.richness.adjusted,landwatermask,
              image.title = "GOC richness",
              directory=getwd(),
              filename=paste("adjusted_",this.center,"_goc.png",sep=""),
              shift,
              part=15,
              resolution)
  
  sp.data = raster(paste("adjusted_",this.center,"_goc.tif",sep="")) %>%
    raster::mask(goc.shape) %>%
    reclassify(cbind(0, NA))
  
  sp.data2 = sp.data %>%
    rasterToPoints %>%
    data.frame %>%
    setnames(c('Longitude', 'Latitude', 'Richness'))
  
  print("Cross validate")
  
  #cross validate
  species.richness.cv = species.richness.cv(biodiversity,
                                            landwatermask, 
                                            fold=5, 
                                            loocv.limit=10, 
                                            distances=2:5,
                                            weight=0.5, 
                                            dimension, 
                                            shift, 
                                            resolution, 
                                            upperbound=5,
                                            all.species=-1,
                                            silent=FALSE,
                                            do.parallel = TRUE)
  
  #extract centers of richness based on the interpolation model and the limit
  #set in clusterlimit parameter for function cluster list
  
  print("Extract cluster centers")
  
  min.size <- 5
  centers <- matrix(0,dimension[1],dimension[2])
  
  for(i in 1:length(clusterlist)){
    if(length(clusterlist[[i]])>=min.size){
      print(paste("Analyzing i =",i))
      
      cluster <- matrix(0,dimension[1],dimension[2])
      
      cluster[clusterlist[[i]]] <- rep(1, length(clusterlist[[i]]))
      
      centers <- centers+cluster
      
      exportAsGDAL(cluster,shift,resolution, directory=getwd(),
                   filename=paste("cluster",i,this.center,".tif",sep=""), drivername = "GTiff")
      
      
      createImage(grid=cluster,landwatermask,
                  image.title = "Clusters",
                  directory=getwd(),
                  filename="clustergoc.png",
                  shift,
                  part=15,
                  resolution)
      
    }
  }
  
  #extract SRE values for centers
  centers[which(centers>0)] <- species.richness.weighted[which(centers>0)]
  
  print("Estimate robustness")
  
  
  robust <- species.richness.cv/species.richness.adjusted
  robust[which(is.na(robust)==TRUE)] <- 0
  robust[which(is.infinite(robust)==TRUE)] <- 0
  
  save.image(file = paste("Routput_",this.center,".RData"))
   
  #calculate robustness for centers
  #robust.center  <- species.richness.cv/centers
  #reset Na values and Inf to Zero
  #robust[which(is.na(robust)==TRUE)] <- 0
  #robust[which(is.infinite(robust)==TRUE)] <- 0
  
  #export as grid
 
  exportAsGDAL(robust, shift, resolution,
               directory=getwd(), filename=paste("robust_",this.center,"_goc.tif",sep=""), drivername="GTiff")
  
  robust.data <- raster(paste("robust_",this.center,"_goc.tif",sep="")) %>%
    raster::mask(goc.shape) %>%
    reclassify(cbind(0, NA)) %>% 
    rasterToPoints %>%
    data.frame %>%
    setnames(c('Longitude', 'Latitude', 'Robustness'))
  
  
  pal <- wes_palette(20, name = "Cavalcanti1", type = "continuous")
  
  robust.plot = ggplot(data=robust.data, aes(y=Latitude, x=Longitude)) +
    geom_raster(aes(fill=Robustness)) +
    theme_bw() +
    coord_equal() +
    scale_fill_gradientn(colours = pal) +
    theme(axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12, angle=90),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'bottom',
          legend.text = element_text(size=10),
          legend.title = element_text(size=12))
  
  sp.plot  <-  ggplot(data=sp.data2, aes(y=Latitude, x=Longitude)) +
    geom_raster(aes(fill=Richness)) +
    theme_bw() +
    coord_equal() +
    scale_fill_gradientn(colours = pal) +
    theme(axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12, angle=90),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'bottom',
          legend.text = element_text(size=10),
          legend.title = element_text(size=12))
  
  #export as png
  png(paste(this.center,"results_goc.png",sep=""))
  plot_grid(robust.plot,sp.plot, align='h',labels = c('A','B'),
            label_size = 16,hjust = -1,vjust = 3)
  dev.off()
  
  print(paste("DONE",this.center))
  
  
  save.image(file = paste("Routput_",this.center,".RData"))
}

stopCluster(cl)
#load(paste("my_work_",this.center,".RData"))

# get.hotspot(species.list=endemic.sp, this.distance=1:10, this.quantile=0.9, this.center="endemic")
# get.hotspot(species.list=narrow.rare.species.nrs, this.distance=1:5, this.quantile=0.9, this.center="rare")
# get.hotspot(species.list=threat.sp, this.distance=1:10, this.quantile=0.9, this.center="threat")
# get.hotspot(species.list=sp.richness.list, this.distance=1:10, this.quantile=0.9, this.center="richness")



#create raster files for hotspots

.packages = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
              "fields","data.table","rgbif","raster", "rasterVis",
              "sp","sperich","spocc","tidyverse","SDMTools","ggmap", "ecoengine", "RCurl","stringi",
              "readr","rgdal","XML", "stringr","R.utils","future", "doSNOW", "gdalUtils")

if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org')

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

rm(list=ls())  
workpath = "~/biodiversity/Analysis"
shapepath = "~/biodiversity/Analysis/SIG_Biodiversity"

setwd(shapepath)
#read in Gulf of California shapefile
#goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")#without wetlands
goc.shape <- readOGR(".", "Golfo_california_wetland_poly_WGS84")
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

NumberOfCluster <- detectCores() - 1
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

setwd(workpath)

hotspots <- c("richness", "threat", "rare", "endemic")

#create raster files for clusters combined 

get.cluster <- function(eachhotspot){
  
sp.data = raster(paste("adjusted_",eachhotspot,"_goc.tif",sep="")) %>% 
    raster::mask(goc.shape) %>% 
    reclassify(cbind(0, NA))

raster.goc <- sp.data

raster.goc[raster.goc] <- NA
  
#delete if file exists
test.file <- file.exists(pattern=paste("cluster_",eachhotspot,".asc",sep="")) 

if(test.file==TRUE) list.files(pattern=paste("cluster_",eachhotspot,".asc",sep=""))%>% file.remove


cluster.list <- list.files(pattern=paste("cluster*.*",eachhotspot,"*.tif", sep=""))

for(eachcluster in 1:length(cluster.list) ) {

cluster.data = raster(cluster.list[eachcluster]) %>% 
  reclassify(cbind(0, NA)) 

if(eachcluster==1){

 merged.cluster <- merge(raster.goc, cluster.data, tolerance = 1)
  
} else {
  
  merged.cluster <- merge(merged.cluster, cluster.data, tolerance = 1)
  
}

}


writeRaster(merged.cluster, file=paste("cluster_",eachhotspot,sep=""),format="ascii", overwrite=TRUE)

plot(merged.cluster)

}

mclapply(hotspots, get.cluster)

#get 10% higher richness values and define as hotspots

get.hotspot <- function(this.center, this.quantile){
  
  sp.data  <-  raster(paste("adjusted_",this.center,"_goc.tif",sep="")) %>% 
    raster::mask(goc.shape) %>% 
    reclassify(cbind(0, NA))
  
  plot(sp.data)
  
  this.cutoff <- sp.data
  
  cutoff <- quantile(this.cutoff, probs=this.quantile)
  
  this.cutoff[this.cutoff<= cutoff] <- NA
  this.cutoff[this.cutoff>=0] <- 1
  
  this.hotspot <- this.cutoff %>% 
    reclassify(cbind(NA,0))
  
  #delete if file exists
  test.file <- file.exists(pattern=paste("hotspot_",this.center,".tif",sep="")) 
  
  if(test.file==TRUE) list.files(pattern=paste("hotspot_",this.center,".tif",sep=""))%>% file.remove

  writeRaster(this.hotspot, file=paste("hotspot_",this.center,sep=""),format="ascii", overwrite=TRUE)
  
  plot(this.hotspot)
  return(this.hotspot)
  
}

hotspots.models <- mclapply(hotspots, get.hotspot, this.quantile=0.9)

#get stats for richness and robustness

get.statistics <- function(this.center){
  
  hotspot.raster <- raster(paste("hotspot_",this.center,".asc",sep=""))%>%
    raster::mask(goc.shape) %>% 
    reclassify(cbind(0, NA))

  weighted.raster <- raster(paste("weighted_",this.center,"_goc.tif",sep="")) %>%
    raster::mask(goc.shape) %>% 
    reclassify(cbind(0, NA))
  
  adjusted.raster <- raster(paste("adjusted_",this.center,"_goc.tif",sep="")) %>%
    raster::mask(goc.shape) %>% 
    reclassify(cbind(0, NA))
  
 pointgrid.raster <- raster(paste("pointgrid_",this.center,"_goc.tif",sep="")) %>%
    raster::mask(goc.shape)%>% 
    reclassify(cbind(0, NA))
  
 robust.raster <- raster(paste("robust_",this.center,"_goc.tif",sep="")) %>%
    raster::mask(goc.shape) %>% 
    reclassify(cbind(0, NA))
  
robust.vals <- mask(robust.raster,hotspot.raster) 
  adjusted.vals <- mask(adjusted.raster,hotspot.raster) 
  pointgrid.vals <- mask(pointgrid.raster,hotspot.raster) 
  
 
  richness.frame <- tibble(hotspot=this.center) %>% 
    mutate(cells.hotspot=sum(!is.na(hotspot.raster[]))) %>% 
    mutate(mean.pointgrid=cellStats(pointgrid.vals,"mean"), sd.pointgrid=cellStats(pointgrid.vals,sd)) %>% 
    mutate(mean.adjusted=cellStats(adjusted.vals,"mean"), sd.adjusted=cellStats(adjusted.vals,sd)) %>% 
    mutate(mean.robust=cellStats(robust.vals,"mean"), sd.robust=cellStats(robust.vals,sd))
 
return(richness.frame)
  
  }

richness.statistics <- mclapply(hotspots, get.statistics)

richness.statistics %>% 
  bind_rows() %>% 
  write_csv("richness_statistics.csv")

#Use this to transform polygons of MPAs into ASCII for 
portfolio.path <- "~/biodiversity/Analysis/SIG_Biodiversity/Portfolios/shapefiles"

file.shp <- list.files(path=portfolio.path,pattern="*full.shp$")
file.shp <- c(file.shp,"GC_MPA_diss.shp")

poly.to.raster <- function (thisfile){

  setwd(portfolio.path)
  eachfile <- thisfile %>% str_split("[.]") %>% unlist %>% .[1]
  print(eachfile)
  this.field <- eachfile %>% str_split("_") %>% unlist %>% .[1]
  print(this.field)
  this.shape <- readOGR(".", eachfile) %>% 
    spTransform(.,crs.geo.wgs)
  
  if(this.field=="GC"){
    this.shape[[this.field]]=1
  }
  
  raster.goc <- raster(ncol=126, nrow=148)
  proj4string(raster.goc) <- crs.geo.wgs
  
  this.raster <- raster::rasterize(this.shape,richness.hotspot,field=this.field, mask=TRUE)
  
  this.raster[this.raster>0] <- 1
  proj4string(this.raster) <- crs.geo.wgs
  
  this.raster <- this.raster %>% reclassify(cbind(NA, 0))

  setwd(workpath)
  writeRaster(this.raster, paste(this.field,"_raster"), format = "ascii", overwrite = TRUE)
}


mclapply(file.shp,poly.to.raster)

stopCluster(cl)
# 
# 
# raster.plot <- function(thisraster,thistitle){
#   
# #convert the raster to points for plotting
# map.p <- rasterToPoints(thisraster) %>% 
#   tbl_df %>% 
#   setNames(c("Longitude", "Latitude","value"))
# 
# ggplot(data=map.p, aes(y=Latitude, x=Longitude)) +
#   geom_raster(aes(fill=value)) +
#   theme_bw() +
#   coord_equal() +
#   geom_polygon(data=goc.shape, aes(x=Longitude, y = Latitude))+
#   scale_fill_gradient(thistitle, limits=c(0,1)) +
#   theme(axis.title.x = element_text(size=16),
#         axis.title.y = element_text(size=16, angle=90),
#         axis.text.x = element_text(size=12),
#         axis.text.y = element_text(size=12, angle=90),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position = "right",
#         legend.key = element_blank()
#   )
# }
# 
# richness.plot <- raster.plot(richness.hotspot,"Richness")
