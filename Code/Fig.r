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

### DOWNLOAD RADAR DIAGRAM CODE
# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies=TRUE)

# Additional packages
p_load("ggradar", "cowplot", "countrycode", "plyr")

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### SET DATAPATH
dataPath <- "D:\\Dropbox\\FOODSECURE Scenarios\\Results" 

# SOURCE
source("Code/plot_f2.r")

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD DATA CUBE
TOTAL <- read_csv(file.path(dataPath, "TOTAL_2017-02-16.csv"))

### SELECT RELEVANT INDICATORS
FNS_db <- TOTAL %>%
  filter(model %in% c("MAGNET", "GLOBIOM"), 
         #(variable == "GDPC" & sector == "TOT") |
         (variable == "XFPI" & sector == "AGR" & unit == "Paasche index (2010=100)") |
         (variable == "XFPI" & sector == "AGR" & unit == "Paasche index (2007=100)") |
         (variable == "PROT" & unit ==  "g prt/cap/d" & sector == "LSP") |
         (variable == "CALO" & unit ==  "g prt/cap/d" & sector == "LSP") |
         (variable == "CALO" & unit ==  "kcal/cap/d" & sector == "TOT") |
         (variable == "CALO" & unit ==  "%" & sector == "CER")) %>%
  mutate(variable = ifelse((model == "GLOBIOM" & variable == "CALO" & unit == "g prt/cap/d"), "PROT", variable),
         FNS = paste(variable, sector, sep = "_"))

xtabs(~FNS + model, data = FNS_db)

FNS_db <- bind_rows(
  FNS_db %>%
    group_by(scenario, FNS, year, region) %>%
    summarize(index = mean(index)) %>%
    mutate(model = "Average"),
  FNS_db %>%
    group_by(scenario, FNS, year, region, model) %>%
    summarize(index = mean(index))
)
    
  
### GLOBAL PICTURE: RADAR GRAPHS
FNS_db_glob <- FNS_db %>%
  ungroup() %>%
  filter(region == "WLD", year == 2050) %>%
  select(-year, -region) %>%
  spread(FNS, index) %>%
  rename(group = scenario)

# Global
FNS_db_glob_av <- FNS_db_glob %>%
  filter(model == "Average") %>%
  select(-model) %>%
  mutate_each(funs(rescale), -group)

p1 = ggradar(FNS_db_glob_av) + labs(title = "Average")

# Magnet
FNS_db_glob_ma <- FNS_db_glob %>%
  filter(model == "MAGNET") %>%
  select(-model) %>%
  mutate_each(funs(rescale), -group)

p2 = ggradar(FNS_db_glob_ma) + labs(title = "MAGNET")

# Globiom
FNS_db_glob_gl <- FNS_db_glob %>%
  filter(model == "GLOBIOM") %>%
  select(-model) %>%
  mutate_each(funs(rescale), -group)


p3 = ggradar(FNS_db_glob_gl) + labs(title = "GLOBIOM")

# Combine plots 
plot_grid(p1, p2, p3)


### CALO CONSUMPTION

# WDI data
# WDI <- WDI(country="all", indicator=c("SP.POP.TOTL"), 
#            start=1960, end=2015) %>%
#   mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
#   filter(!is.na(iso3c)) %>%
#   select(POP = SP.POP.TOTL, iso3c)

#saveRDS(WDI, file = paste("./Data/Add_data", "WDI_", Sys.Date(), ".rds", sep=""))
WDI <- readRDS(file = "./Data/WDI_2016-05-25.rds")

# Region concordance
MAGNET2FS_REG <- read.csv(".\\Data\\MAGNET2FS_REG.csv") %>%
  dplyr::select(Region = FS_region2, FS_region_name_short) %>%
  unique()

# Country concordance
FS2ISO_REG <- read.csv(".\\Data\\FStoISO_MAGNETCountryAggregation.csv") %>%
  select(FS_region_name_short, iso3c = ISO) %>%
  left_join(., MAGNET2FS_REG)

## CALORIE CONSUMPTION
# Load historical data
hist_cal_r <- read.csv("./Data/calcpcpd.csv") %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>% #remove reg aggregates
  select(iso3c, year = Year, value = Value) %>%
  left_join(., FS2ISO_REG) %>% 
  left_join(., WDI) %>%
  group_by(year, Region) %>%
  summarize(value = sum(value*POP, na.rm = T)/sum(POP, na.rm = T))

hist_cal_w <- read.csv("./Data/calcpcpd.csv") %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>% #remove reg aggregates
  select(iso3c, year = Year, value = Value) %>%
  left_join(., FS2ISO_REG) %>% 
  left_join(., WDI) %>%
  group_by(year) %>%
  summarize(value = sum(value*POP, na.rm = T)/sum(POP, na.rm = T)) %>%
  mutate(Region = "WLD")

scen <- expand.grid(Scenario = unique(TOTAL$scenario), Region = unique(TOTAL$region))  
hist_cal <- bind_rows(histcal_r, histcal_w) %>%
  left_join(scen, .) %>%
  #filter(year <=2010) %>%
  dplyr::rename(scenario = Scenario, region = Region) %>%
  filter(year >= 1990, !(region %in% c("ROW", "EU")))

hist_cal_base <- filter(histcal, year == 2010) %>%
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
CALO_wld <- filter(CALO_db, region == "WLD")
hist_cal_wld <- filter(hist_cal, region == "WLD")
bwplot2_f(CALO_wld, hist_cal_wld, "kcal/cap/d")

### XFPI 
# Historical data
# NB Base year = 2010 so no need to rebase our series
hist_price_wld <- read_csv("Data/hist_agr_price.csv") %>%
  left_join(.,scen) %>%
  dplyr::rename(scenario = Scenario, region = Region) 

# Database
XFPI_db <- FNS_db %>% 
  filter(FNS == "XFPI_AGR") %>%
  dplyr::rename(value = index)

# Draw plot for world
XFPI_wld <- filter(XFPI_db, region == "WLD") 
bwplot2_f(XFPI_wld, hist_price_wld, "Index (2010 = 100)")

### PROT
# Database
PROT_db <- FNS_db %>% 
  filter(FNS == "PROT_LSP") %>%
  dplyr::rename(value = index)

# Draw plot for world
PROT_wld <- filter(PROT_db, region == "WLD")
bwplot_f(PROT_wld, "gram/cap/d")

### CALO_CER 
# Database
CALO_CER_db <- FNS_db %>% 
  filter(FNS == "CALO_CER") %>%
  dplyr::rename(value = index)

# Draw plot
bwplot_f(CALO_CER_db, "kcal/cap/d")


### COMPARE VALUES
# Prepare data
comp_db <- filter(FNS_db, model != "Average") %>%
  spread(model, index)

Fig_comp <- ggplot() + 
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

Fig_comp


# kcd plot
pdf(file = "./Results/Graphs/CAL_line_kcd.pdf", width = 12, height = 12)
bwplot2_f(CALO_db, histcal, "kcal/cap/d")
lineplot_f(CALO, "index")
dev.off()




# Plot
pdf(file = "./Results/Graphs/CAL_line_i.pdf", width = 12, height = 12)
#lineplot2_f(CALO_i, histcal, "kcal/cap/d")
lineplot_f(CALO_i, "kcal/cap/d")
dev.off()
