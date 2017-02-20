#'========================================================================================================================================
#' Project:  FOODSECURE WP7
#' Subject:  Script to create tables
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
p_load("WDI", "countrycode", "stargazer")

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### DATAPATH


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### ANOVA
aov_db <- filter(FNS_db, model != "Average", FNS == "CALO_TOT")
summary(aov_db)
FNS_aov <- aov(index ~factor(year) + scenario + region + model, data = aov_db)
summary(FNS_aov)

# Create data for map
p_load("proxy")

### CORRELATIONs
cor_db <- filter(FNS_db, model != "Average") %>%
  spread(model, index)

cor(cor_db[c("MAGNET", "GLOBIOM")], method = "pearson")
cor(cor_db[c("MAGNET", "GLOBIOM")], method = "spearman")
cor(cor_db[c("MAGNET", "GLOBIOM")], method = "kendall")


# Write data
write_csv(file.path(root, "Produced_data/"))

# Create table