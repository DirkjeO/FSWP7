#'========================================================================================================================================
#' Project:  FOODSECURE WP7
#' Subject:  Plot MAGNET HH results
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
#lapply(SpatialPackages, library, character.only = TRUE)
#AdditionalPackages <- c("WDI")
#lapply(AdditionalPackages, library, character.only = TRUE)

### SET WORKING DIRECTORY
wdPath<-"D:\\R\\FSWP7"
setwd(wdPath)

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### FUNCTIONS
# Line plot to compare models
lineplot_f <- function(df, yas){
  unit_c <- paste(unique(df$unit), collapse = " and ")
  title <- unique(with(df, paste(variable, sector, sep="_")))
  title <- paste(title, unit_c, sep = "_")
  point <- filter(df, year == 2050)
  
  p = ggplot() +
    geom_line(data = df, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="Scenario")+ 
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "Model") +
    scale_shape_manual(values=c(16,17,18), name = "Model") +
    ylab(yas) + xlab("") +
    facet_wrap(~region, scale = "free")
  
  p = p +ggtitle(title) 
  
  p = p + guides(fill = guide_legend(keywidth = 1, keyheight = 1, override.aes = 
                                       list(alpha = 0.1, size = 0.5, colour = c("green","cyan","red","purple"))))
  
  p = p + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
    scale_x_continuous(limits = c(2010,2050.1), breaks = seq(2010,2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
    scale_y_continuous(labels = comma) +
    theme(legend.background=element_blank()) +
    theme(legend.key=element_rect(size=0.5, color="white"), # Increase space between legend boxes - doubt if it works
          strip.background = element_rect(colour="white", fill="white")) # Remove box and background of facet
  
  
  p
}

# Line plot to compare HH
lineplot_hh_f <- function(df, yas){
  unit_c <- paste(unique(df$unit), collapse = " and ")
  title <- unique(with(df, paste(variable, sector, region, sep="_")))
  title <- paste(title, unit_c, sep = "_")
  point <- filter(df, year == 2050)
  
  p = ggplot() +
    geom_line(data = df, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="Scenario")+ 
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "Model") +
    scale_shape_manual(values=c(16,17,18), name = "Model") +
    ylab(yas) + xlab("") +
    facet_wrap(~HHLD, scale = "free")
  
  p = p +ggtitle(title) 
  
  p = p + guides(fill = guide_legend(keywidth = 1, keyheight = 1, override.aes = 
                                       list(alpha = 0.1, size = 0.5, colour = c("green","cyan","red","purple"))))
  
  p = p + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
    scale_x_continuous(limits = c(2010,2050.1), breaks = seq(2010,2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
    scale_y_continuous(labels = comma) +
    theme(legend.background=element_blank()) +
    theme(legend.key=element_rect(size=0.5, color="white"), # Increase space between legend boxes - doubt if it works
          strip.background = element_rect(colour="white", fill="white")) # Remove box and background of facet
  
  
  p
}



# Load data
HH <- read.csv("Cache/MAGNETHH_t_st_2016-11-22.csv") %>%
  rename(sector = FSsector, region = FSregion) %>%
  select(-modelrun) %>%
  filter(unit != "mil USD") %>%
  mutate(unit = ifelse(variable %in% c("NQSECT", "NQT"), tolower(unit), unit)) # lowercase units


xtabs(~ variable + unit, data = GDP_POP_YEXO)

# Line plot: comparing acros countries
p_HH <- HH %>%
  filter(HHLD == "hh1") %>%
  group_by(variable, sector, unit) %>%
  do(plots = lineplot_f(., "value")) 

pdf(file = "./Graphs/p_HH_t.pdf", width = 7, height = 7)
p_HH$plots
dev.off()

# Line plot: comparing across HH
p_HH2 <- HH %>%
  group_by(region, variable, sector, unit) %>%
  do(plots = lineplot_hh_f(., "value")) 

pdf(file = "./Graphs/p_HH2_t.pdf", width = 7, height = 7)
p_HH2$plots
dev.off()

