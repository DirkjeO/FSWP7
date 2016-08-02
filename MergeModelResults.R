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
wdPath<-"D:\\Dropbox\\FOODSECURE Scenarios"
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

# ANALYSIS
# Combine files of four models and add FAOSTAT data
MAGNET2FS_REG <- read.csv(file.path(".\\Mappings\\FStoISO_MAGNETCountryAggregation.csv")) %>%
  dplyr::select(Region = FS_region_name, FS_region_name_short) %>%
  unique()


# Read data per model
# GLOBIOM
# Process
GLOBIOM <- read.csv("./Results/GLOBIOM_FoodSecure_7jun16.csv") %>% 
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
  arrange(model, scenario, FSregion, sector, variable, year) %>%
  group_by(model, scenario, FSregion, sector, variable) %>%
  filter(!any(year==2010))
# write.csv(check, file = "./Results/GLOBIOMmiss.csv", row.names = F)

GLOBIOM <- GLOBIOM %>%
  arrange(model, scenario, FSregion, sector, variable, year) %>%
  group_by(model, scenario, FSregion, sector, variable) %>%
  filter(any(year==2010)) # to remove values with missing 2010 CHECK with AREA!!??

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


# Check
unique(IMAGE$variable)
unique(IMAGE$sector)
xtabs(~IMAGE$variable + IMAGE$sector)
check <- filter(IMAGE, is.na(sector)) # FRTN lacks a sector


# MAGNET
MAGNET <- read.csv("./Results/MAGNET_FoodSecure_2016-07-22.csv") %>%
            rename(sector = FS_sector)

# Bind in one file
SIMULATION <- rbind(MAGNET, IMAGE, GLOBIOM) %>% 
              filter(year>=2010)

TOTAL <- SIMULATION



# Index (2010=100)
# CHECK: GLOBIOM has various results for CALO with different units
# CHECK: GLOBIOM PROD has wrong unit.
# CHECK: product groups GLOBION. VFN = V_F? AGR in MAGNET?
# CHECK HARMONISATION FOOD GROUPS IN TEMPLATE.

TOTAL2 <- TOTAL %>%
  arrange(model, scenario, FSregion, sector, variable, year) %>%
  group_by(model, scenario, FSregion, sector, variable, unit) %>%
  mutate(index = value/value[year==2010]*100) %>%
  arrange(model, scenario, variable, FSregion, sector, year)

xtabs(~model + variable, data = TOTAL2)

write.csv(TOTAL2, "Results/TOTAL.csv", row.names = F)

# Calculate total land

data.land <- TOTAL %>%
              filter(sector %in% c("CRPLND", "GRSLND")) %>%
              group_by(model, FSregion, variable, scenario, year, unit) %>%
              summarize(value = sum(value)) %>%
              rename(Val = value, Scen = scenario, Year = year, Model = model) %>%
              filter(FSregion == "WLD") %>%
              mutate(Item = "AGRLND")

# Combine with historical data
# Load historial data and add 3 year moving average
HIST <- read.csv(file.path(dataPath, "FAOSTAT\\FAOSTAT_hist_2015-09-24.csv")) %>% 
        group_by(Model, Scenario, Region, Sector, Variable) %>% 
        mutate(Value = as.numeric(ma(Value))) %>% # 3-year smoother
        ungroup() %>% 
        as.data.frame(.) 

TOTAL <- rbind(HIST, SIMULATION)  


# File for DataM
DataM <- filter(TOTAL2, Variable %in% c("AREA", "PROD")) %>%
          select(-Value) %>%
          rename(Value = index)
write.csv(DataM, file.path(dataPath, "DataM_test.csv"), row.names = F)

xtabs(~Year + Region, data = HIST)

# Calculations
prodgr <- filter(MAGNET, variable == "PROD" & sector == "FOOD" & FSregion == "WLD") %>%
          group_by(scenario) %>%
          mutate(growth = ((value/value[year == 2010])-1)*100) %>%
          filter(year == 2050)
prodgr
