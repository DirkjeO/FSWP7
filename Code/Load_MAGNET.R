#######################################################
##### LOAD AND CREATE VARIABLES             ###########
#######################################################

# PACKAGES
BasePackages <- c("lazyeval", "foreign", "stringr", "gdata", "car", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2", "haven")
lapply(BasePackages, library, character.only = TRUE)
AdditionalPackages <- c("gdxrrw", "micEcon")
lapply(AdditionalPackages, library, character.only = TRUE)

# load required GAMS libraries (folder user specific)
GAMSPath <- "C:\\24.4"
#GAMSPath <- "C:\\Program Files\\GAMS\\win64\\24.6"
igdx(GAMSPath)
# Make sure GDX2HAR.exe and gdxiomh.dll are located in one folder.

# Set working folder
#wdPath <- "D:\\Kuiper\\HHS_BIOF"
wdPath <- "D:\\Shutes\\FOODSECURE"
#wdPath <- "T:\\Shutes\\FOODSECURE"
setwd(wdPath)  
dataPath <- ".\\R\\"
dataResultPath <- "./4_MAGNET/Results"

# Source functions
source(".\\R\\HARtGDX_FSMIP\\Load_Magnet_f.r")

# R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

# Define scenarios, periods, path, project, sourcefile and 
scenarios<-c("FFANF_qpc_ti3_st", "ONEPW_qpc_ti3_st", "TLTL_qpc_ti3_st", "ECO_qpc_ti3_st")
periods<-c("2007-2010", "2010-2020", "2020-2030", "2030-2040", "2040-2050")
#project<-""

# create lookup table for update files
sourcefile<-c("update")
lookup_upd <- expand.grid(scenarios, periods, sourcefile, stringsAsFactors = FALSE)

# split periods in start and finish
TMP <- data.frame(do.call(rbind, str_split(lookup_upd$Var2, "-", 2)))
lookup_upd <- cbind(lookup_upd, TMP) ; rm(TMP)
names(lookup_upd) <- c("scenario", "period", "sourcefile", "start", "year")

# Create list of all relevant har and gdx files by period and scenario 
lookup_upd$harSourceFiles <- paste(with(lookup_upd, paste(scenario, period, sourcefile, sep="_")), ".har", sep="")
lookup_upd$gdxResultFiles <- paste(with(lookup_upd, paste(scenario, period, sourcefile, sep="_")), ".gdx", sep="")

# Create lookup table for update_view files
sourcefile<-c("update_view")
lookup_upd_view <- expand.grid(scenarios, periods, sourcefile, stringsAsFactors = FALSE)

# split periods in start and finish
TMP <- data.frame(do.call(rbind, str_split(lookup_upd_view$Var2, "-", 2)))
lookup_upd_view <- cbind(lookup_upd_view, TMP); rm(TMP)
names(lookup_upd_view) <- c("scenario", "period", "sourcefile", "start", "year")

# Create list of all relevant har and gdx files by period and scenario 
lookup_upd_view$harSourceFiles <- paste(with(lookup_upd_view, paste(scenario, period, sourcefile, sep="_")), ".har", sep="")
lookup_upd_view$gdxResultFiles <- paste(with(lookup_upd_view, paste(scenario, period, sourcefile, sep="_")), ".gdx", sep="")

# Create lookup table for solution files files
sourcefile<-c("Solution")
lookup_sol <- expand.grid(scenarios, periods, sourcefile, stringsAsFactors = FALSE)

# split periods in start and finish
TMP <- data.frame(do.call(rbind, str_split(lookup_sol$Var2, "-", 2)))
lookup_sol <- cbind(lookup_sol, TMP); rm(TMP)
names(lookup_sol) <- c("scenario", "period", "sourcefile", "start", "year")

# Create list of all relevant har and gdx files by period and scenario 
lookup_sol$harSourceFiles <- paste(with(lookup_sol, paste(scenario, period, sourcefile, sep="_")), ".sol", sep="")
lookup_sol$gdxResultFiles <- paste(with(lookup_sol, paste(scenario, period, sourcefile, sep="_")), ".gdx", sep="")

# Create lookup table for slc files
sourcefile<-c("solution")
destinationfile <- c("solution_slc")
lookup_slc <- expand.grid(scenarios, periods, sourcefile, stringsAsFactors = FALSE)

# split periods in start and finish
TMP <- data.frame(do.call(rbind, str_split(lookup_slc$Var2, "-", 2)))
lookup_slc <- cbind(lookup_slc, TMP) ; rm(TMP)
names(lookup_slc) <- c("scenario", "period", "sourcefile", "start", "year")

# Create list of all relevant har and gdx files by period and scenario 
lookup_slc$harSourceFiles <- paste(with(lookup_slc, paste(scenario, period, sourcefile, sep="_")), ".slc", sep="")
lookup_slc$gdxResultFiles <- paste(with(lookup_slc, paste(scenario, period, destinationfile, sep="_")), ".gdx", sep="")
