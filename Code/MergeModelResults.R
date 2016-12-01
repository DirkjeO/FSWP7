# PROJECT: FOODSECURE WP7
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# Merge resuls from different models
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# PACKAGES
BasePackages <- c("foreign", "stringr", "gdata", "car", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2", "openxlsx")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <-  c("WDI", "countrycode")
lapply(AdditionalPackages, library, character.only = TRUE)

# SET PATHS
wdPath<-"C:\\Users\\vandijkm\\Dropbox\\FOODSECURE Scenarios"
#wdPath<-"D:\\Dropbox\\FOODSECURE Scenarios"
setwd(wdPath)


# R SETTINGS
options(scipen=99) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=2)

# FUNCTIONS
# Plus function that ensures NA + NA is NA not 0 as in sum
plus <- function(x) {
  if(all(is.na(x))){
    c(x[0],NA)} else {
      sum(x,na.rm = TRUE)}
}

# ma function to calculate moving average
ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}

## READ DATA

# GLOBIOM
# Process
GLOBIOM <- read.csv("./Results/GLOBIOM_FoodSecure_8oct16.csv") %>% 
  rename(variable = Var, sector = Item, scenario = Scen, year = Year, value = Val, FSregion = Reg, unit = Unit) %>% 
  mutate(model = "GLOBIOM", 
         variable =toupper(variable), 
         sector=toupper(sector),
         FSregion = revalue(FSregion, c("1_SubSaharanAfrica" = "SSA",
                                      "1_LatinAmerica" = "LAC",
                                      "1_MENA" = "MENA",
                                      "1_EastAsia" = "EASIA",
                                      "1_EU28" = "EU",
                                      "1_SouthAsia" = "SASIA",
                                      "1_Other" = "ROW",
                                      "WLD" = "WLD"))) %>%
  filter(FSregion %in% c("SSA", "LAC", "MENA", "EASIA", "EU", "SASIA", "ROW", "WLD")) 
xtabs(~GLOBIOM$sector + GLOBIOM$variable)

# Check if there are variables with missing information for 2010
# There are a few combination in GLOBIOM that lack 2010 data
check <- GLOBIOM %>%
  arrange(model, scenario, FSregion, sector, variable, unit, year) %>%
  group_by(model, scenario, FSregion, sector, unit, variable) %>%
  filter(!any(year==2010))
#write.csv(check, file = "./Results/GLOBIOMmiss.csv", row.names = F)

GLOBIOM <- GLOBIOM %>%
  arrange(model, scenario, FSregion, sector, variable, unit, year) %>%
  group_by(model, scenario, FSregion, sector, variable, unit) %>%
  filter(any(year==2010)) # to remove values with missing 2010 
xtabs(~sector + variable, data = GLOBIOM)
xtabs(~unit + variable, data = GLOBIOM)


# IMAGE
IMAGE2FSRegion <- read.csv("./Mappings/IMAGE2FSRegion.csv")

# Process
IMAGE <- read.xlsx("./Results/AllScen_IMAGE_FSregions.xlsx") %>% 
  gather(year, value, -Model:-Unit) %>%
  rename(sector = Variable, scenario = Scenario, model = Model, unit = Unit, region = Region) %>%
  filter(sector %in% c("AREA|OILSEEDS", "AREA|RICE", "AREA|WHT", "FRTN", "LAND|Built-up Area",
                       "LAND|Cropland", "LAND|Cropland|Energy Crops", "LAND|Forest", "LAND|Other Arable Land",                     
                       "LAND|Other Land", "LAND|Pasture")) %>%
  mutate(year = as.numeric(year),
         scenario = revalue(scenario, c("FFNF" = "FFANF")),
         sector = toupper(revalue(sector, c("LAND|Cropland|Energy Crops" = "LAND|EneCrp",
                                            "LAND|Cropland" = "LAND|CrpLnd",
                                            "LAND|Pasture" = "LAND|GrsLnd")))) %>%
  separate(sector, c("variable", "sector"), sep = "\\|", fill = "right") %>%
  mutate(value = ifelse(sector %in% c("WHT", "RICE", "OILSEEDS"), value/1000, value),
         unit = ifelse(sector %in% c("WHT", "RICE", "OILSEEDS"), "1000 ha", unit),
         unit = ifelse(unit == "million ha", "Mha", unit)
         ) %>%
  left_join(.,IMAGE2FSRegion) %>%
  group_by(model, scenario, variable, sector, unit, year, FSregion) %>%
  summarize(
    value = sum(value))

# Aggregates
# CHECK sum of RICE, WHT, OILSEEDS
IMAGE_CRP_AREA <- IMAGE %>%
              filter(variable %in% "AREA") %>%
              group_by(model, scenario, variable, year, unit, FSregion) %>%
              summarize(value = sum(value, na.rm = T)) %>%
              mutate(sector = "CRP")

IMAGE <- rbind(IMAGE, IMAGE_CRP_AREA) %>% ungroup; rm(IMAGE_CRP_AREA)
xtabs(~sector + variable, data = IMAGE)

# Check
unique(IMAGE$variable)
unique(IMAGE$sector)
xtabs(~IMAGE$variable + IMAGE$sector)
check <- filter(IMAGE, is.na(sector)) # FRTN lacks a sector

# MAGNET
MAGNET <- read.csv("./Results/MAGNET_t_st_2016-11-21.csv") %>%
            rename(sector = FSsector) %>%
            select(-Modelrun) %>%
            filter(unit != "mil USD") %>%
            mutate(unit = ifelse(variable %in% c("NQSECT", "NQT"), tolower(unit), unit)) # lowercase units

# Remove lower level regional aggregations
MAGNET <- filter(MAGNET, !(FSregion %in% c("CHN", "GHA", "IDN", " IND", "NAF", "SAF", "UGA", "WAF", "KEN")))

# Remove _M sectors that do not include primary processing
Xsector <- unique(MAGNET$sector[grep("_M", MAGNET$sector)])
MAGNET <- filter(MAGNET, !(sector %in% Xsector))


# check
xtabs(~MAGNET$variable + MAGNET$unit) 
xtabs(~MAGNET$variable + MAGNET$FSregion) 
xtabs(~MAGNET$variable + MAGNET$sector) 


# Bind in one file
SIMULATION <- bind_rows(MAGNET, IMAGE, GLOBIOM) %>% 
              filter(year>=2010)

TOTAL <- SIMULATION



# Index (2010=100)
TOTAL2 <- TOTAL %>%
  arrange(model, scenario, FSregion, sector, variable, year, unit) %>%
  group_by(model, scenario, FSregion, sector, variable, unit) %>%
  mutate(index = value/value[year==2010]*100) %>%
  arrange(model, scenario, variable, FSregion, sector, year)

xtabs(~variable + model, data = TOTAL2)
xtabs(~sector + model, data = TOTAL2)
xtabs(~unit + model, data = TOTAL2)

# Save files
write.csv(TOTAL2, paste0("Results/TOTAL_", Sys.Date(), ".csv"), row.names = F)
