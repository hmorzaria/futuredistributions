
#devtools::install_github("danlwarren/ENMTools")

#system("sudo apt-get install libxml2-dev")
# system("wget http://www.omegahat.net/SSOAP/SSOAP_0.9-0.tar.gz")
# system("wget http://www.omegahat.net/XMLSchema/XMLSchema_0.7-0.tar.gz")
# 
# install.packages("~/conapescaspecies/XMLSchema_0.7-0.tar.gz", repos = NULL, type = "source")
# install.packages("~/conapescaspecies/SSOAP_0.9-0.tar.gz", repos = NULL, type = "source")

#options(java.parameters = "-Xmx8g")
#gc()

#javafolder <- system.file("java",package="dismo")
#rJava requires openjdk 8
#system("sudo apt-get install openjdk-8-jdk", wait = TRUE)
# if need to uninstall java completey see 
#https://askubuntu.com/questions/84483/how-to-completely-uninstall-java
# system("sudo apt-get install unzip", wait = TRUE)
# system("unzip maxent.zip -d maxent", wait=TRUE)
# system(paste("sudo cp -a ~/maxent/maxent.jar ", javafolder,"/", sep=""), wait = TRUE)
# system("sudo R CMD javareconf", wait=TRUE)
#then run this from a ssh session
#sudo rstudio-server stop
#export LD_LIBRARY_PATH=/usr/lib/jvm/jre/lib/amd64:/usr/lib/jvm/jre/lib/amd64/default
#sudo rstudio-server start
#devtools::install_github("sylvainschmitt/SSDM")
#https://cran.r-project.org/src/contrib/Archive/SDMTools/


.packages <- (c("tidyverse","dplyr","future", "parallel", "doSNOW",
                "data.table","raster","rgdal", "maps", "mapdata", 
                "dismo","maptools","jsonlite","rasterVis",
                "rnaturalearth","tools","gridExtra","Hmisc","SSDM",
                "sf", "rnaturalearth", "rnaturalearthdata","rnaturalearthhires",
                "ggmap", "spThin","dismo","virtualspecies","ggspatial","sdm","SDMTools",
                "tiff","RStoolbox","spatialEco", "readxl","cowplot"))#,"LightSSDM"



# Install CRAN packages (if not already installed)

if(!require(SSDM)){
  devtools::install_github("sylvainschmitt/SSDM")
}

#devtools::install_github('hugocalcad/LightSSDM')


.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org', dependencies = TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)



#if these packages are not installed uncomment and add
#devtools::install_github("ropenscilabs/rnaturalearth")
#devtools::install_github("ropenscilabs/rnaturalearthdata")
#install.packages("rnaturalearthhires",
#                 repos = "http://packages.ropensci.org",
#                 type = "source")
