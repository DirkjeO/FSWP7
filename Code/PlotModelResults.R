# PROJECT: FOODSECURE WP7
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# Merge resuls from different models
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# QUESTIONS
# FOOD PRICES? Definition of FOOD in GLOBIOM.
# COMPARE YEXO across models!
# NEED TO BE CLEAR ABOUT AGGREGATES: FOOD, AGR, ETC.
# DIFFERENCE AREA AND LAND in IMAGE
# Unit of LDEM in MAGNET -> higher than other models.

# PACKAGES
BasePackages <- c("foreign", "stringr", "gdata", "car", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2", "openxlsx", "scales", "lazyeval", "ggthemes", "scales")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <-  c("WDI", "countrycode")
lapply(AdditionalPackages, library, character.only = TRUE)

# SET PATHS
wdPath <-"D:\\Dropbox\\FOODSECURE Scenarios"
setwd(wdPath)


# R SETTINGS
options(scipen=99) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=2)


# CALO EMIS EXPO FEED FOOD FRTN GDPpc GDPval IMDR IMPO NETT  NQT OTHU XCPI XPRI XPRP 

# Compare all
# # Line plot - index
# TOTAL_lineplot_i <- TOTAL2 %>%
#   group_by(variable, sector) %>%
#   select(-value) %>%
#   rename(value = index) %>%
#   do(plots = lineplot_f(., "Index")) 
# 
# pdf(file = "./Results/Graphs/TOTAL.pdf", width = 7, height = 7)
# TOTAL_lineplot_i$plots
# dev.off()

# Load data
TOTAL2 <- read.csv("Results/TOTAL_2016-11-11.csv")


# Comparison GDP, POP and YEXO
GDP_POP_YEXO <- TOTAL2 %>%
  filter(variable %in% c("POPT", "GDPT", "YEXO"))
  
xtabs(~ model + variable, data = GDP_POP_YEXO)

# Line plot - index
GDP_POP_YEXO_lineplot_i <- GDP_POP_YEXO %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/GDP_POP_YEXO_i.pdf", width = 7, height = 7)
GDP_POP_YEXO_lineplot_i$plots
dev.off()

# Comparison of consumption
CONS <- TOTAL2 %>% 
        filter(variable == "CONS")
xtabs(~model + sector, data = CONS)

# Line plot - index
CONS_lineplot_i <- CONS %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/CONS_lineplot_i.pdf", width = 7, height = 7)
CONS_lineplot_i$plots
dev.off()

# Comparison AREA and LAND
LAND_AREA <- TOTAL2 %>%
  filter(variable %in% c("AREA", "LAND")) 


inf.nan.na.clean_f<-function(x){
  x[do.call(cbind, lapply(x, is.nan))]<-NA
  x[do.call(cbind, lapply(x, is.infinite))]<-NA
  x<-x[complete.cases(x),]
  return(x)
}

# IMAGE has zero values for LAND/AREA for a number of sector-region combinations
LAND_AREA <- inf.nan.na.clean_f(LAND_AREA)
xtabs(~model + sector, data = LAND_AREA)

# Line plot - index
LAND_AREA_lineplot_i <- LAND_AREA %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/LAND_AREA_line_i.pdf", width = 7, height = 7)
LAND_AREA_lineplot_i$plots
dev.off()

# Line plot - value
LAND_AREA_lineplot_ha <- LAND_AREA %>%
  group_by(variable, sector) %>%
  do(plots = lineplot_f(., "1000 ha")) 

pdf(file = "./Graphs/LAND_AREA_line_ha.pdf", width = 7, height = 7)
LAND_AREA_lineplot_ha$plots
dev.off()

# 
# LAND_AREA_lineplot_WLD <- LAND_AREA_db %>%
#   filter(FSregion == "WLD") %>%
#   group_by(variable, sector) %>%
#   select(-value) %>%
#   rename(value = index) %>%
#   do(plots = lineplot_f(., "Index")) 
# 
# pdf(file = "./Results/Graphs/LAND_AREA_line_WLD.pdf", width = 7, height = 7)
# IM_GL_lineplot_WLD$plots
# dev.off()

# 
# # BW plot
# LAND_AREA_bwplot <- LAND_AREA_db %>%
#   group_by(variable, sector) %>%
#   select(-value) %>%
#   rename(value = index) %>%
#   do(plots = bwplot_f(., "Index")) 
# 
# pdf(file = "./Results/Graphs/LAND_AREA_bw.pdf", width = 7, height = 7)
# IM_GL_bwplot$plots
# dev.off()

# Compare base data models (by = 2010)
LAND_AREA_baseplot <- LAND_AREA %>%
    filter(scenario == "ECO") %>%
    group_by(variable, sector, scenario) %>%
  do(plots = baseplot_f(., 2010)) 

pdf(file = "./Graphs/LAND_AREA_base.pdf", width = 7, height = 7)
LAND_AREA_baseplot$plots
dev.off()

## Comparison YILD, PROD AND AREA

YILD_PROD_AREA <- filter(TOTAL2, variable %in% c("AREA", "PROD", "YILD"), unit != "USD 2000/cap/d") %>% 
  filter(model != "IMAGE")
xtabs(~model + sector + FSregion, data = YILD_PROD_AREA)

# compare index
YILD_PROD_AREA_lineplot_i <- YILD_PROD_AREA %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/YILD_PROD_AREA_line_i.pdf", width = 7, height = 7)
YILD_PROD_AREA_lineplot_i$plots
dev.off()


# Comparison FS indicators
# Calorie consumption per capita per day
CALO <- filter(TOTAL2, variable == "CALO" & unit == "kcal/cap/d" & sector == "TOT")

# compare index
CALO_lineplot_i <- CALO %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/CALO_line_i.pdf", width = 7, height = 7)
CALO_lineplot_i$plots
dev.off()













# WDI data
# WDI <- WDI(country="all", indicator=c("SP.POP.TOTL"), 
#            start=1960, end=2015) %>%
#   mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
#   filter(!is.na(iso3c)) %>%
#   select(POP = SP.POP.TOTL, iso3c)

#saveRDS(WDI, file = paste("./Data/Add_data", "WDI_", Sys.Date(), ".rds", sep=""))
WDI <- readRDS(file = "./Data/Add_data/WDI_2016-05-25.rds")

# Region concordance
MAGNET2FS_REG <- read.csv(".\\Mappings\\MAGNET2FS_REG.csv") %>%
  dplyr::select(Region = FS_region2, FS_region_name_short) %>%
  unique()

# Country concordance
FS2ISO_REG <- read.csv(".\\Mappings\\FStoISO_MAGNETCountryAggregation.csv") %>%
  select(FS_region_name_short, iso3c = ISO) %>%
  left_join(., MAGNET2FS_REG)

## CALORIE CONSUMPTION
# Load historical data
histcal_r <- read.csv("./Data/Add_data/calcpcpd.csv") %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>% #remove reg aggregates
  select(iso3c, year = Year, value = Value) %>%
  left_join(., FS2ISO_REG) %>% 
  left_join(., WDI) %>%
  group_by(year, Region) %>%
  summarize(value = sum(value*POP, na.rm = T)/sum(POP, na.rm = T))

histcal_w <- read.csv("./Data/Add_data/calcpcpd.csv") %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>% #remove reg aggregates
  select(iso3c, year = Year, value = Value) %>%
  left_join(., FS2ISO_REG) %>% 
  left_join(., WDI) %>%
  group_by(year) %>%
  summarize(value = sum(value*POP, na.rm = T)/sum(POP, na.rm = T)) %>%
  mutate(Region = "WLD")

scen <- expand.grid(Scenario = unique(TOTAL$scenario), Region = unique(TOTAL$FSregion))  
histcal <- rbind(histcal_r, histcal_w) %>%
  left_join(scen, .) %>%
  filter(year <=2010) %>%
  rename(scenario = Scenario, FSregion = Region) %>%
  filter(year >=1990)

histcal_base <- filter(histcal, year == 2010) %>%
  rename(Base2010 = value) %>%
  select(-year) 

# Rebase simulations 2010 to historical data (2010=100)
CALO_i <- CALO %>%
  group_by(scenario, FSregion) %>%
  left_join(., histcal_base) %>%
  mutate(value = Base2010*index/100)

# kcd plot
pdf(file = "./Results/Graphs/CAL_line_kcd.pdf", width = 12, height = 12)
#bwplot2_f(CALO_i, histcal, "kcal/cap/d")
lineplot_f(CALO, "index")
dev.off()

# Plot
pdf(file = "./Results/Graphs/CAL_line_i.pdf", width = 12, height = 12)
#lineplot2_f(CALO_i, histcal, "kcal/cap/d")
lineplot_f(CALO_i, "kcal/cap/d")
dev.off()

# Prices

PRICES <- TOTAL2 %>%
  filter(variable %in% c("XPRI") & sector %in% c("AGR", "CRP"))

PRICES_i <- PRICES %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Results/Graphs/PRICES_line_i.pdf", width = 7, height = 7)
PRICES_i$plots
dev.off()









