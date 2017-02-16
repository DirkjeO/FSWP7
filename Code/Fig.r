#'========================================================================================================================================
#' Project:  FSWP7
#' Subject:  Plots to illustrate food security in 2050
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
#p_load("WDI", "countrycode")

### DOWNLOAD RADAR DIAGRAM CODE
devtools::install_github("ricardo-bion/ggradar", 
                         dependencies=TRUE)

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### SET DATAPATH
dataPath <- "C:\\Users\\vandijkm\\Dropbox\\FOODSECURE Scenarios\\Results" 

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD DATA CUBE
Total <- read_csv(file.path(dataPath, "TOTAL_2017-02-16.csv"))

### SELECT RELEVANT INDICATORS
FNS <- Total %>%
  filter(model %in% c("MAGNET", "GLOBIOM"), variable %in% 
           c("CALO", "PROT", "XPRP", "NETT", "PCAL", "SHRF", "GDPC", "XFPI", "XPRM"))



### AREA GRAPHS