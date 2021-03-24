#to install rJava
#https://github.com/hannarud/r-best-practices/wiki/Installing-RJava-(Ubuntu)
#sudo add-apt-repository ppa:marutter/c2d4u3.5
#sudo apt-get update
#sudo apt-get install default-jdk
#sudo R CMD javareconf
#sudo apt-get install r-cran-rjava
#sudo apt-get install libgdal-dev libproj-dev
#Then in Rstudio install.packages("rJava")


#system("wget http://www.omegahat.net/XMLSchema/XMLSchema_0.7-0.tar.gz")
#install.packages("~/XMLSchema_0.7-0.tar.gz", repos = NULL, type = "source")

install.packages("devtools")
library(devtools)
#install_local("/home/atlantis/ENMTools-master")
#library(ENMTools)

remotes::install_github("ropensci/rfishbase")
remotes::install_github("sckott/SSOAP")
remotes::install_github('ropensci/ecoengine')
remotes::install_github('ropensci/taxize')

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
              "maxnet","sdm", "rfishbase","ggmap","virtualspecies") #"rJava"

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos='http://cran.us.r-project.org', dependencies = FALSE)
#install.packages(c("XMLSchema", "SSOAP"), repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"))
if(!require(robis)){devtools::install_github("iobis/robis"); library(robis)}
if(!require(taxizesoap)){devtools::install_github("ropensci/taxizesoap"); library(taxizesoap)}


# Load packages into session 
lapply(.packages, require, character.only=TRUE)

installAll() #needed packages for sdm

