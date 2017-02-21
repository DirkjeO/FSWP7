#'========================================================================================================================================
#' Project:  FSWP7
#' Subject:  Plots to illustrate food security in 2050
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# NB using plyr and dplyr together creates conflicts for summarize and rename functions that are present in both
# We load plyr first as we used most functions from dplyr
p_load(plyr) 
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")

### DOWNLOAD RADAR DIAGRAM CODE
# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies=TRUE)

# Additional packages
p_load("ggradar", "cowplot", "countrycode")

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### SET DATAPATH
#dataPath <- "D:\\Dropbox\\FOODSECURE Scenarios\\Results" 
dataPath <- "C:\\Users\\vandijkm\\Dropbox\\FOODSECURE Scenarios\\Results"

# SOURCE
source(file.path(root, "Code/plot_f2.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### PROCESS DATA CUBE
# Load data
TOTAL <- read_csv(file.path(dataPath, "TOTAL_2017-02-16.csv"))

### SELECT RELEVANT INDICATORS
# Select variables
FNS_db <- TOTAL %>%
  filter(model %in% c("MAGNET", "GLOBIOM"), 
         (variable == "GDPC" & sector == "TOT") |
         (variable == "IMDR" & unit ==  "%" & sector == "CER") |
         (variable == "XFPI" & sector == "AGR" & unit == "Paasche index (2010=100)") |
         (variable == "XFPI" & sector == "AGR" & unit == "Paasche index (2007=100)") |
         (variable == "PROT" & unit ==  "g prt/cap/d" & sector == "LSP") |
         (variable == "CALO" & unit ==  "g prt/cap/d" & sector == "LSP") |
         (variable == "CALO" & unit ==  "kcal/cap/d" & sector == "TOT") |
         (variable == "CALO" & unit ==  "%" & sector == "CER")) %>%
  mutate(variable = ifelse((model == "GLOBIOM" & variable == "CALO" & unit == "g prt/cap/d"), "PROT", variable),
         FNS = paste(variable, sector, sep = "_")) %>%
  ungroup()

# Add GDPC values for GLOBIOM
FNS_db_GDPC <- FNS_db %>%
  filter(variable == "GDPC" & sector == "TOT") %>%
  mutate(model = "GLOBIOM")

# Add missing GDPC values for GLOBIOM
FNS_db <- bind_rows(FNS_db, FNS_db_GDPC)

# Set combinations
comb <- expand.grid(scenario = unique(FNS_db$scenario), region = unique(FNS_db$region), 
                    year = unique(FNS_db$year), FNS = unique(FNS_db$FNS), model = unique(FNS_db$model))  

# Add missing values for IMDR
FNS_db <- FNS_db %>%
  left_join(comb, .) %>%
  mutate(index = ifelse(is.na(index), 0, index),
         value = ifelse(is.na(value), 0, value),
         unit = ifelse(is.na(unit), "%", unit),
         sector = ifelse(is.na(sector), "CER", sector),
         variable = ifelse(is.na(variable), "IMDR", variable),
         value = ifelse(value < 0, 0, value),
         index = ifelse(index < 0, 0, index))
  
# Multiply cereal values of GLOBIOM x 100
FNS_db <- FNS_db %>%
  mutate(value = ifelse(model == "GLOBIOM" & FNS == "CALO_CER", value*100, value))

# Add average values
FNS_db <- bind_rows(
  FNS_db %>%
    group_by(scenario, FNS, year, region) %>%
    summarize_at(vars(value, index), mean) %>%
    mutate(model = "Average"),
  FNS_db %>%
    group_by(scenario, FNS, year, region, model) %>%
    summarize_at(vars(value, index), mean)
)
    
  
### GLOBAL PICTURE: RADAR GRAPHS
# Global database
FNS_glob <- FNS_db %>%
  ungroup() %>%
  filter(region == "WLD", year %in% c(2010, 2050)) %>%
  group_by(FNS, model) %>%
  mutate(min_val = min(value),
         min_index = min(index),
         max_val = max(value),
         max_index = max(index)) %>%
  select(-region) %>%
  ungroup()

# Rescale CALO_TOT
FNS_glob_CALO_TOT <- FNS_glob %>%
  filter(FNS == "CALO_TOT") %>%
  mutate(scale = ((value - 2800)/(3400-2800))*100) %>%
  filter(year == 2050)

# Rescale PROT_LSP
FNS_glob_PROT_LSP <- FNS_glob %>%
  filter(FNS == "PROT_LSP") %>%
  mutate(scale = ((value - 20)/(50-20))*100) %>%
  filter(year == 2050)

# CHECK IF WE CAN SHOW FOOD PRICES
# Rescale XFPI_AGR (negative scaling as this is a 'bad')
FNS_glob_XFPI_AGR <- FNS_glob %>%
  filter(FNS == "XFPI_AGR") %>%
  mutate(scale = ((max_index-index)/(max_index-min_index))*100) %>%
  filter(year == 2050)

# Rescale GDPC_TOT
FNS_glob_GDPC_TOT <- FNS_glob %>%
  filter(FNS == "GDPC_TOT") %>%
  mutate(scale = ((value - min_val)/(max_val-min_val))*100) %>%
  filter(year == 2050)

# CALO_CER 
FNS_glob_CALO_CER <- FNS_glob %>%
  filter(FNS %in% c("CALO_CER")) %>%
  mutate(scale = value) %>%
  filter(year == 2050)

# IMDR_CER (negative scaling as this is a 'bad')
FNS_glob_IMDR_CER <- FNS_glob %>%
  filter(FNS %in% c("IMDR_CER")) %>%
  mutate(scale = ((max_val-value)/(max_val-min_val))*100) %>%
  filter(year == 2050)


# Combine scaled variables
FNS_glob_scale <- bind_rows(FNS_glob_CALO_CER, FNS_glob_CALO_TOT, FNS_glob_GDPC_TOT, FNS_glob_IMDR_CER, FNS_glob_PROT_LSP, FNS_glob_XFPI_AGR) %>%
  mutate(scale = scale/100) 


# Global per model
FNS_glob_av <- FNS_glob_scale %>%
  filter(model == "Average") %>%
  select(group = scenario, FNS, scale) %>%
  spread(FNS, scale) 

p1 = ggradar(FNS_glob_av, axis.label.size = 3) + labs(title = "Average")

# Magnet
FNS_glob_ma <- FNS_glob_scale %>%
  filter(model == "MAGNET") %>%
  select(group = scenario, FNS, scale) %>%
  spread(FNS, scale)

p2 = ggradar(FNS_glob_ma, axis.label.size = 3) + labs(title = "MAGNET")

# Globiom
FNS_glob_gl <- FNS_glob_scale %>%
  filter(model == "GLOBIOM") %>%
  select(scenario, FNS, scale) %>%
  spread(FNS, scale)

p3 = ggradar(FNS_glob_gl, axis.label.size = 3) + labs(title = "GLOBIOM")

# Combine plots 
Fig_radar <- plot_grid(p1, p2, p3)

# Global per scenario
# TLTL
FNS_glob_TLTL <- FNS_glob_scale %>%
  filter(scenario == "TLTL") %>%
  select(model, FNS, scale) %>%
  spread(FNS, scale)

p_TLTL = ggradar(FNS_glob_TLTL, axis.label.size = 3) + labs(title = "TLTL")

# ECO
FNS_glob_ECO <- FNS_glob_scale %>%
  filter(scenario == "ECO") %>%
  select(model, FNS, scale) %>%
  spread(FNS, scale)

p_ECO = ggradar(FNS_glob_ECO, axis.label.size = 3) + labs(title = "ECO")

# ONEPW
FNS_glob_ONEPW <- FNS_glob_scale %>%
  filter(scenario == "ONEPW") %>%
  select(model, FNS, scale) %>%
  spread(FNS, scale)

p_ONEPW = ggradar(FNS_glob_ONEPW, axis.label.size = 3) + labs(title = "ONEPW")

# FFANF
FNS_glob_FFANF <- FNS_glob_scale %>%
  filter(scenario == "FFANF") %>%
  select(model, FNS, scale) %>%
  spread(FNS, scale)

p_FFANF = ggradar(FNS_glob_FFANF, axis.label.size = 3) + labs(title = "FFANF")

# Combine plots 
Fig_radar_scen <- plot_grid(p_TLTL, p_FFANF, p_ONEPW, p_ECO)

### LOAD COUNTRY MAPPINGS

# WDI data
# WDI <- WDI(country="all", indicator=c("SP.POP.TOTL"), 
#            start=1960, end=2015) %>%
#   mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
#   filter(!is.na(iso3c)) %>%
#   select(POP = SP.POP.TOTL, iso3c)

#saveRDS(WDI, file = paste("./Data/Add_data", "WDI_", Sys.Date(), ".rds", sep=""))
WDI <- readRDS(file.path(root, "Data/WDI_2016-05-25.rds"))

# Region concordance
MAGNET2FS_REG <- read.csv(file.path(root, "Data\\MAGNET2FS_REG.csv")) %>%
  dplyr::select(Region = FS_region2, FS_region_name_short) %>%
  unique()

# Country concordance
FS2ISO_REG <- read.csv(file.path(root, "Data\\FStoISO_MAGNETCountryAggregation.csv")) %>%
  select(FS_region_name_short, iso3c = ISO) %>%
  left_join(., MAGNET2FS_REG)

# Script to load and process FAOSTAT food security data
FAO_FS_f <- function(var, ey){
  df <- read_excel(file.path(root, "Data/Food_Security_Indicators.xlsx"), sheet = var, skip = 2)
  df <- df[colSums(!is.na(df)) > 0] # Remove columns that are all NA
  ey <- ey -1
  df_proc <- df %>%
    mutate(iso3c = countrycode(FAOST_CODE, "fao","iso3c"),
           iso3c = ifelse(FAOST_CODE == 5001, "WLD", iso3c)) %>%
    filter(!is.na(iso3c)) %>%
    select(-FAOST_CODE, -`Regions/Subregions/Countries`) %>%
    setNames(c(as.character(c(1991:ey)), "iso3c")) %>%
    gather(year, value, -iso3c) %>%
    mutate(year = as.numeric(year))
}

# Set scenario combinations
scen <- expand.grid(Scenario = unique(TOTAL$scenario), Region = unique(TOTAL$region))  

### CALORIE CONSUMPTION
# Load historical data
hist_cal_r <- read.csv(file.path(root, "Data/calcpcpd.csv")) %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>% #remove reg aggregates
  select(iso3c, year = Year, value = Value) %>%
  left_join(., FS2ISO_REG) %>% 
  left_join(., WDI) %>%
  group_by(year, Region) %>%
  summarize(value = sum(value*POP, na.rm = T)/sum(POP, na.rm = T))

hist_cal_w <- read.csv(file.path(root, "Data/calcpcpd.csv")) %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>% #remove reg aggregates
  select(iso3c, year = Year, value = Value) %>%
  left_join(., FS2ISO_REG) %>% 
  left_join(., WDI) %>%
  group_by(year) %>%
  summarize(value = sum(value*POP, na.rm = T)/sum(POP, na.rm = T)) %>%
  mutate(Region = "WLD")

scen <- expand.grid(Scenario = unique(TOTAL$scenario), Region = unique(TOTAL$region))  
hist_cal <- bind_rows(hist_cal_r, hist_cal_w) %>%
  left_join(scen, .) %>%
  #filter(year <=2010) %>%
  dplyr::rename(scenario = Scenario, region = Region) %>%
  filter(year >= 1990, !(region %in% c("ROW", "EU")))

hist_cal_base <- filter(hist_cal, year == 2010) %>%
  dplyr::rename(Base2010 = value) %>%
  select(-year) 

# CALO database
CALO_db <- FNS_db %>%
  filter(FNS == "CALO_TOT")

# Rebase simulations 2010 to historical data (2010=100)
CALO_db <- CALO_db %>%
  left_join(., hist_cal_base) %>%
  mutate(value = Base2010*index/100) %>%
  filter(!(region %in% c("ROW", "EU")))

# Draw plot for world
#CALO_wld <- filter(CALO_db, region == "WLD")
#hist_cal_wld <- filter(hist_cal, region == "WLD")
Fig_CALO <- bwplot2_f(CALO_db, hist_cal, "kcal/cap/d")

### XFPI 
# Historical data
# NB Base year = 2010 so no need to rebase our series. Only Available for the whole world
hist_price_wld <- read_csv(file.path(root, "Data/hist_agr_price.csv")) %>%
  left_join(.,scen) %>%
  dplyr::rename(scenario = Scenario, region = Region) 

# Database
XFPI_db <- FNS_db %>% 
  filter(FNS == "XFPI_AGR") %>%
  dplyr::rename(value = index)

# Draw plot for world
XFPI_wld <- filter(XFPI_db, region == "WLD") 
Fig_XFPI <- bwplot2_f(XFPI_wld, hist_price_wld, "Index (2010 = 100)")

### PROT
# Load historical data
hist_prot <- FAO_FS_f("V_1.5", 2011) %>%
  left_join(., FS2ISO_REG) %>%
  mutate(Region = ifelse(iso3c == "WLD", "WLD", Region)) %>%
  filter(!is.na(Region)) %>%
  group_by(Region, year) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  left_join(scen, .) %>%
  rename(scenario = Scenario, region = Region)  %>%
  filter(!(region %in% c("ROW", "EU")))

# Rebase simulations 2010 to historical data (2010=100)
hist_prot_base <- filter(hist_prot, year == 2010) %>%
  dplyr::rename(Base2010 = value) %>%
  select(-year) 

# Database
PROT_db <- FNS_db %>% 
  filter(FNS == "PROT_LSP") %>%
  left_join(., hist_prot_base) %>%
  mutate(value = Base2010*index/100) %>%
  filter(!(region %in% c("ROW", "EU")))

# Draw plot
Fig_PROT <- bwplot2_f(PROT_db, hist_prot, "gr/cap/day")


### CALO_CER 
# Load historical data
hist_cal_cer <- FAO_FS_f("V_1.3", 2011) %>%
  left_join(., FS2ISO_REG) %>%
  mutate(Region = ifelse(iso3c == "WLD", "WLD", Region)) %>%
  filter(!is.na(Region)) %>%
  group_by(Region, year) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  left_join(scen, .) %>%
  rename(scenario = Scenario, region = Region) %>%
  filter(!(region %in% c("ROW", "EU")))

# Rebase simulations 2010 to historical data (2010=100)
hist_cal_cer_base <- filter(hist_cal_cer, year == 2010) %>%
  dplyr::rename(Base2010 = value) %>%
  select(-year) 

# Database
CALO_cer_db <- FNS_db %>% 
  filter(FNS == "CALO_CER") %>%
  left_join(., hist_cal_cer_base) %>%
  mutate(value = Base2010*index/100) %>%
  filter(!(region %in% c("ROW", "EU")))

# Draw plot
Fig_CALO_cer <- bwplot2_f(CALO_cer_db, hist_cal_cer, "Index (2010 = 100)")

# 
# ### IMDR_CER 
# # Load historical data
# hist_imdr <- FAO_FS_f("V_3.1", 2011) %>%
#   left_join(., FS2ISO_REG) %>%
#   mutate(Region = ifelse(iso3c == "WLD", "WLD", Region)) %>%
#   filter(!is.na(Region)) %>%
#   group_by(Region, year) %>%
#   summarize(value = mean(value, na.rm = T)) %>%
#   left_join(scen, .) %>%
#   rename(scenario = Scenario, region = Region) %>%
#   filter(region == "SSA")
#   filter(!(region %in% c("ROW", "EU")))
# 
# # Rebase simulations 2010 to historical data (2010=100)
# hist_imdr_base <- filter(hist_imdr, year == 2010) %>%
#   dplyr::rename(Base2010 = value) %>%
#   select(-year) 
# 
# # Database
# IMDR_db <- FNS_db %>% 
#   filter(FNS == "IMDR_CER") %>%
#   left_join(., hist_imdr_base) %>%
#   mutate(value = Base2010*index/100) %>%
#   filter(region == "SSA")
#   filter(!(region %in% c("ROW", "EU")))
# 
# xtabs(~region + year, data = IMDR_db)
# # Draw plot
# IMDR_db_wld <- filter(IMDR_db, region == "WLD")
# bwplot2_f(IMDR_db, hist_imdr, "Index (2010 = 100)")
# 

### COMPARE MODEL RESULTS
# Prepare data
comp_db <- filter(FNS_db, model != "Average") %>%
  spread(model, index)

Fig_comp_model <- ggplot() + 
  geom_point(data = comp_db, aes(x = MAGNET, y = GLOBIOM, colour = scenario)) +
  coord_fixed(ratio = 1) +
  facet_wrap(~FNS, scale = "free") +
  geom_abline(intercept = 0, slope =1) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(face = "bold")) +
  #theme(aspect.ratio=1) +
  labs(colour = "Variable") +
  #coord_cartesian(ylim=c(100, 150),xlim=c(100, 150)) +
  coord_cartesian() +
  #scale_y_continuous(labels = comma, breaks=seq(0, 15, 2.5)) +
  #scale_x_continuous(labels = comma, breaks=seq(0, 15, 2.5)) +
  theme(legend.position="bottom",
        legend.box="horizontal") +
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow=1))


