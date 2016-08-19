#######################################################
##### PROCESS MAGNET VARIABLES             ############
#######################################################

# CHECK: NETT, XFPI YILD, 

### FUNCTIONS
# Simple aggregation over sectors using mapping
subtot_f <-function(df, grp, tvar, map){
  FUN <- match.fun("sum")
  df_subtot <- df %>%
    left_join(.,map) %>%
    na.omit %>% # Remove unmapped sectors
    group_by_(.dots=grp) %>%
    summarise_(value = interp(~FUN(v), v=as.name(tvar))) %>%
    ungroup()
}

# Weighted aggregation
wsubtot_f <-function(df, grp, tvar, weight, map){
  FUN <- match.fun("sum")
  df %>%
    left_join(.,map) %>%
    na.omit %>% # Remove unmapped sectors
    group_by_(.dots=grp) %>%
    #summarize(value = sum(x*y)/sum(y))
    summarise_(value = interp(~FUN(v*w)/FUN(w), v=as.name(tvar), w=as.name(weight))) %>%
    ungroup
}


### CREATE MAPPINGS
# Load concordance table MAGNET2FS
MAGNET2FS_REG <- read.csv(file.path(dataPath, "Concordance\\MAGNET2FS_REG.csv")) %>%
  rename(REG = FS_MAGNET) %>%
  unique()

MAGNET2FS_SEC <- read.csv(file.path(dataPath, "Concordance\\FStoMAGNETSectorAggregation.csv"), na.strings = "") %>%
  dplyr::rename(sector = FS_sector_code, TRAD_COMM = FS_MAGNET_sector_name) %>%
  dplyr::filter(!is.na(sector)) %>% # remove unmapped feed sector in MAGNET
  unique()

# Create regional and sectoral mappings
map_fsreg <- MAGNET2FS_REG %>%
  select(REG, FSregion = FS_region2) %>%
  na.omit %>%
  unique

map_wld <- MAGNET2FS_REG %>%
  select(REG, FSregion = World) %>%
  na.omit %>%
  unique

map_af <- MAGNET2FS_REG %>%
  select(REG, FSregion = FS_region3) %>%
  mutate(FSregion = zap_empty(FSregion)) %>%
  na.omit %>%
  unique

map_hh <- MAGNET2FS_REG %>%
  select(REG, FSregion = FS_hhregion) %>%
  mutate(FSregion = zap_empty(FSregion)) %>%
  na.omit %>%
  unique

map_cer <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = cer) %>%
  na.omit %>%
  unique

map_food <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = food) %>%
  na.omit %>%
  unique

map_crp <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = crp) %>%
  na.omit %>%
  unique

map_lsp <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = lsp) %>%
  na.omit %>%
  unique

map_agr <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = agr) %>%
  na.omit %>%
  unique

map_sec <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = sec) %>%
  na.omit %>%
  unique

map_primfood <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = primfood) %>%
  na.omit %>%
  unique

map_anml <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = anml) %>%
  na.omit %>%
  unique

map_tot <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = tot) %>%
  na.omit %>%
  unique

map_sec_M <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = sec_M) %>%
  na.omit %>%
  unique

map_tot_M <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = tot_M) %>%
  na.omit %>%
  unique

 
# ### TEST df
# prodcon <- constant.f("prodcon", "VALOUTPUT", c("TRAD_COMM","REG", "GDPSOURCE"), c("TRAD_COMM", "REG"), "qo", c("NSAV_COMM", "REG"))
# test <- filter(prodcon, TRAD_COMM %in% c("pdr", "wht", "gro", "hort", "osd", "c_b", "oagr", "crops")) %>% 
#   mutate(REG = toupper(REG))
# test <- AREA_raw
# df <- test
# grp <- c("scenario", "year", "FSsector", "FSregion", "variable", "unit")
# tvar <- "value"
# mapsec <- map_sec
# mapreg <- map_wld
# rm(mapsec, mapreg, df, grp, tvar)



####################################################
#### Variables with sector and region dimension ####
####################################################

MAGNET1_raw <- list()

#### AREA: harvested and grazed area
# MAGNET: land demand per sector (km2) = AREA
# Note: several countries have multiple land types, here only GHA. As "ENDWL_COMM" is not in the grouping variable. These are summed.
MAGNET1_raw[["AREA"]] <- current.f("AREA", "BaseData_b.gdx", "LTYPEDEM", lookup_upd, "LDEM", c("PROD_SECT", "ENDWL_COMM", "REG"), c("PROD_SECT", "REG")) %>%
  rename(TRAD_COMM = PROD_SECT) %>%
  mutate(value = value/10, # MAGNET AREA is in km2
         unit = "1000 ha")

#### PROD: total production
MAGNET1_raw[["PROD"]] <- constant.f("PROD", "VALOUTPUT", c("TRAD_COMM","REG", "GDPSOURCE"), c("TRAD_COMM", "REG"), "qo", c("NSAV_COMM", "REG")) %>%
  mutate(unit = "mil 2007 USD")

#### PRODval: Production value, market prices = VALOUTPUT(SSEC,SREG,SUM) AND VALOUTPUT(SSEC,SREG,SUM)  (NOT certified)
MAGNET1_raw[["PRODval"]] <- current.f("PROD", "BaseData_b_view.gdx", "valoutput", lookup_upd_view, "valoutput", c("TRAD_COMM", "REG", "OUTVALUE"), c("TRAD_COMM","REG")) %>%
  mutate(unit = "mil USD")

#### CONS: total domestic consumption
# NB not CONS as defined in MAGNET but domestic use!

PROD <- constant.f("PROD", "VALOUTPUT", c("TRAD_COMM","REG", "GDPSOURCE"), c("TRAD_COMM", "REG"), "qo", c("NSAV_COMM", "REG")) %>%
  rename(PROD = value) %>%
  ungroup() %>%
  select(-variable)

# NB: in case of EXPO REGSOURCE is the exporter and REDDEST the importer.
### EXPO: export volume at market prices
EXPO <- constant2.f("EXPO", "BaseData_b.gdx", "VXMD", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGSOURCE", "REGDEST"), "qxs", c("TRAD_COMM", "REGSOURCE", "REGDEST")) %>%
  group_by(scenario, year, variable, REGSOURCE, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGSOURCE, EXPO = value) %>%
  ungroup() %>%
  select(-variable) 

# NB: in case of IMPO, REGDEST is the importer and REGSOURCE the exporter.
### IMPO: import volume at market prices
IMPO <- constant2.f("IMPO", "BaseData_b.gdx", "VIMS", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGDEST", "REGSOURCE"), "qxs", c("TRAD_COMM", "REGSOURCE", "REGDEST")) %>%
  group_by(scenario, year, variable, REGDEST, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGDEST, IMPO = value) %>% 
  ungroup() %>%
  select(-variable) 

# Domestic use
MAGNET1_raw[["CONS"]] <- left_join(PROD, IMPO) %>%
  left_join(., EXPO) %>%
  mutate(EXPO = ifelse(is.na(EXPO), 0, EXPO)) %>%
  mutate(IMPO = ifelse(is.na(IMPO), 0, IMPO)) %>%
  mutate(value = PROD+IMPO-EXPO) %>%
  select(-PROD, -IMPO, -EXPO) %>%
  mutate(variable = "CONS",
         unit = "mil 2007 USD")
rm(PROD, EXPO, IMPO)

# Nutrients per sector
MAGNET1_raw[["NQSECT"]] <- current.f("NQSECT", "fsbasecalories_2007-2010_update_view.gdx",  "NQSECT", lookup_upd_view, "NQSECT", c("NUTRIENTS", "PRIM_AGRI", "REG"), c("NUTRIENTS", "PRIM_AGRI","REG"))  %>%
  rename(TRAD_COMM = PRIM_AGRI, unit = NUTRIENTS)

# NB: in case of EXPO REGSOURCE is the exporter and REDDEST the importer.
### EXPO: export volume at market prices in volume
MAGNET1_raw[["EXPO"]] <- constant2.f("EXPO", "BaseData_b.gdx", "VXMD", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGSOURCE", "REGDEST"), "qxs", c("TRAD_COMM", "REGSOURCE", "REGDEST")) %>%
    group_by(scenario, year, variable, REGSOURCE, TRAD_COMM) %>%
    summarize(value = sum(value, na.rm=T)) %>%
    rename(REG = REGSOURCE) %>%
    ungroup() %>%
    mutate(variable = "EXPO",
         unit = "mil 2007 USD")

# NB: in case of IMPO, REGDEST is the importer and REGSOURCE the exporter.
### IMPO: import volume at market prices in volume
MAGNET1_raw[["IMPO"]] <- constant2.f("IMPO", "BaseData_b.gdx", "VIMS", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGDEST", "REGSOURCE"), "qxs", c("TRAD_COMM", "REGSOURCE", "REGDEST")) %>%
  group_by(scenario, year, variable, REGDEST, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGDEST) %>% 
  ungroup() %>%
  mutate(variable = "IMPO",
       unit = "mil 2007 USD")

# NB: in case of EXPO REGSOURCE is the exporter and REDDEST the importer.
### EXPO: export value at market prices
MAGNET1_raw[["EXPOval"]] <- current.f("EXPO", "BaseData_b.gdx", "VXMD", lookup_upd, "VXMD", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGDEST", "REGSOURCE")) %>%
  group_by(scenario, year, variable, REGSOURCE, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGSOURCE) %>%
  ungroup() %>%
  mutate(variable = "EXPO",
         unit = "mil USD")

# NB: in case of IMPO, REGDEST is the importer and REGSOURCE the exporter.
### IMPO: import value at market prices
MAGNET1_raw[["IMPOval"]] <- current.f("IMPO", "BaseData_b.gdx", "VIMS", lookup_upd, "VIMS", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGDEST", "REGSOURCE")) %>%
  group_by(scenario, year, variable, REGDEST, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGDEST) %>% 
  ungroup() %>%
  mutate(variable = "IMPO",
         unit = "mil USD")


### PCONS: Total private domestic consumption volume
# Private domestic consumption volume
# Private consumption of domestic products volume
pridomconsvol <- constant2.f("pridomconsvol", "BaseData_b.gdx", "VDPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"), "qpd", c("TRAD_COMM", "REG"))
# Private consumption of imported products volume
priimpconsvol <- constant2.f("priimpconsvol", "BaseData_b.gdx", "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"), "qpm", c("TRAD_COMM", "REG"))

MAGNET1_raw[["PCONS"]] <- rbind(pridomconsvol, priimpconsvol) %>%
  group_by(REG, TRAD_COMM, scenario, year) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = "PCONS",
         unit = "mil 2007 USD")
rm(pridomconsvol, priimpconsvol)

### PCONS: Total private domestic consumption value
# Private domestic consumption value
# Private consumption of domestic products value
pridomconsval <- current.f("pridomconsval", "BaseData_b.gdx", "VDPM", lookup_upd, "VDPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"))
# Private consumption of imported products value
priimpconsval <- current.f("priimpconsval", "BaseData_b.gdx", "VIPM", lookup_upd, "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"))

# Total private domestic consumption val
MAGNET1_raw[["PCONSval"]] <- rbind(pridomconsval, priimpconsval) %>%
  group_by(REG, TRAD_COMM, scenario, year) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = "PCONS",
         unit = "mil USD")
rm(pridomconsval, priimpconsval)


# Private consumption of imported products volume
MAGNET1_raw[["VIMP"]] <- constant2.f("priimpconsvol", "BaseData_b.gdx", "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"), "qpm", c("TRAD_COMM", "REG")) %>%
  mutate(variable = "VIPM",
         unit = "mil 2007 USD")

# Private consumption of imported products value
MAGNET1_raw[["VIMPval"]] <- current.f("priimpconsval", "BaseData_b.gdx", "VIPM", lookup_upd, "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG")) %>%
  mutate(variable = "VIPM",
         unit = "mil USD")


### FOOD, FEED and OTHU
source("R\\HARtGDX_FSMIP\\FOODFEED.r")
MAGNET1_raw[["FEED"]] <- FEED; rm(FEED)
MAGNET1_raw[["FOOD"]] <- FOOD; rm(FOOD)
MAGNET1_raw[["OTHU"]] <- OTHU; rm(OTHU)

############################################
### Variables with only region dimension ###
############################################
MAGNET2_raw <- list()

# GDP volume
MAGNET2_raw[["GDPT"]] <-  constant2.f("GDPT","BaseData_b_view.gdx", "GDPSRC", c("REG", "GDPSOURCE"), "REG", "qgdp", "REG") %>%
         mutate(unit = "mil 2007 USD")
       
# POP total population
MAGNET2_raw[["POPT"]] <- constant2.f("POPT", "BaseData_b.gdx", "POP", c("REG"), c("REG"), "pop", c("REG")) %>%
  mutate(unit = "mil pers")

# GDP value = GDPSRC(SREG,SUM) AND GDPSRC(SREG,SUM)  (NOT certified)
MAGNET2_raw[["GDPval"]] <- current.f("GDPT", "BaseData_b_view.gdx", "GDPSRC", lookup_upd_view, "GDPSRC", c("REG", "GDPSOURCE"), c("REG")) %>%
  mutate(unit = "mil USD")

MAGNET2_raw[["NQT"]] <- current.f("NQT", "fsbasecalories_2007-2010_update_view.gdx",  "NQT", lookup_upd_view, "NQT", c("NUTRIENTS", "REG"), c("NUTRIENTS", "REG")) %>%
  rename(unit = NUTRIENTS)



#####################
### COMBINE DATA ###
####################

MAGNET1 <- bind_rows(MAGNET1_raw) %>%
              ungroup() %>%
              mutate(REG = toupper(REG)) # REG in capitals for mapping

MAGNET2 <- bind_rows(MAGNET2_raw) %>%
  mutate(REG = toupper(REG)) # REG in capitals for mapping
         
  

#########################################
### MAKE REGION AND SECTOR AGGREGATES ###
#########################################

# Sectoral mappings
MAGNET1 <- bind_rows(
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_cer),
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_sec),
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_sec_M),
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_tot_M),
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_agr),
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_crp),
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_primfood),
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_anml),
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_food),
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_tot)
)

# Regional mappings
MAGNET1 <-bind_rows(
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "FSregion", "variable", "unit"), "value", map_fsreg),
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "FSregion", "variable", "unit"), "value", map_wld),
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "FSregion", "variable", "unit"), "value", map_af),
  subtot_f(MAGNET1, c("scenario", "year", "FSsector", "FSregion", "variable", "unit"), "value", map_hh)
)

MAGNET2 <-bind_rows(
  subtot_f(MAGNET2, c("scenario", "year", "FSregion", "variable", "unit"), "value", map_fsreg),
  subtot_f(MAGNET2, c("scenario", "year", "FSregion", "variable", "unit"), "value", map_wld),
  subtot_f(MAGNET2, c("scenario", "year", "FSregion", "variable", "unit"), "value", map_af),
  subtot_f(MAGNET2, c("scenario", "year", "FSregion", "variable", "unit"), "value", map_hh)
) %>%
  mutate(FSsector = "TOT") # Add TOT for all national level indicators


#################################
### MERGE ALL AGGREGATED DATA ###
#################################

MAGNET1_2 <- rbind(MAGNET1, MAGNET2)


############################
### ADDITIONAL VARIABLES ###
############################

MAGNET3_raw <- list()

### YILD: Endogenous yield
# Need to replace LSP woth LPS defined over RMEAT and DAIRY only.
PRODlsp <- constant.f("PROD", "VALOUTPUT", c("TRAD_COMM","REG", "GDPSOURCE"), c("TRAD_COMM", "REG"), "qo", c("NSAV_COMM", "REG")) %>%
            mutate(unit = "mil 2007 USD") %>% 
            filter(TRAD_COMM %in% c("ctl", "rmk"))

PRODlsp <- subtot_f(PRODlsp, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_lsp)

# Regional mappings
PRODlsp <-bind_rows(
  subtot_f(PRODlsp, c("scenario", "year", "FSsector", "FSregion", "variable", "unit"), "value", map_fsreg),
  subtot_f(PRODlsp, c("scenario", "year", "FSsector", "FSregion", "variable", "unit"), "value", map_wld)
)

MAGNET3_raw[["YILD"]] <-
  YILD <- bind_rows(
           filter(MAGNET1_2, variable %in% c("AREA") & 
                  FSsector %in% c("AGR", "CER", "CRP", "DAIRY", "FOOD", "LSP", "MEAT", "OCEREALS",
                                "OCROPS", "OILSEEDS", "PFB", "RICE", "RMEAT", "SUGAR",  "VFN", "WHT")),
          filter(MAGNET1_2, variable %in% c("PROD") & unit %in% c("mil USD") &
                 FSsector %in% c("AGR", "CER", "CRP", "DAIRY", "FOOD", "MEAT", "OCEREALS",
                                "OCROPS", "OILSEEDS", "PFB", "RICE", "RMEAT", "SUGAR",  "VFN", "WHT")),
          PRODlsp) %>% # Only sectors with land
      select(-unit) %>%
      group_by(scenario, FSregion, FSsector, year) %>%
      summarize(value = value[variable == "PROD"]/value[variable == "AREA"]) %>%
      mutate(variable = "YILD",
      unit = "1000 USD/ha")
rm(PRODlsp)

### YEXO: Exogenous yield
# Cumulative yield growth is extracted with aland2.f (2007 = 1)
# Note that in aland2.f year is made numeric, which creates some problems as it is character in all other data frames. This is corrected again.
aland <- aland2.f("aland", "aland", c("PROD_SECT", "MREG")) %>%
  rename(REG = MREG, 
         aland = value,
         TRAD_COMM = PROD_SECT) %>%
  select(-variable)  %>%
  mutate(year = as.character(year))
 

AREA <-current.f("AREA", "BaseData_b.gdx", "LTYPEDEM", lookup_upd, "LDEM", c("PROD_SECT", "ENDWL_COMM", "REG"), c("PROD_SECT", "REG")) %>%
  rename(TRAD_COMM = PROD_SECT,
         AREA = value) %>% 
  select(-variable) %>%
  filter(year == 2007) %>%
  select(-year)

YEXO_raw <- left_join(aland, AREA) %>%
        mutate(aland_w = aland*AREA) %>%
        select(-aland) %>%
        gather(variable, value, -scenario: -year) %>%
        mutate(unit = "none")

# Sectoral mappings
YEXO_raw <- bind_rows(
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_cer),
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_sec),
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_agr),
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_crp),
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_food),
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_sec_M),
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_tot_M),
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_primfood),
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_anml),
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_tot)
)

# Regional mappings
YEXO_raw <-bind_rows(
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "FSregion", "variable", "unit"), "value", map_fsreg),
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "FSregion", "variable", "unit"), "value", map_wld),
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "FSregion", "variable", "unit"), "value", map_af),
  subtot_f(YEXO_raw, c("scenario", "year", "FSsector", "FSregion", "variable", "unit"), "value", map_hh)
  )

MAGNET3_raw[["YEXO"]]  <- YEXO_raw %>% 
                      group_by(scenario, year, FSsector, FSregion, unit) %>%
                      summarize(value = value[variable == "aland_w"]/value[variable == "AREA"]) %>%
                      mutate(variable = "YEXO",
                      unit = "1000 USD/ha")
rm(YEXO_raw, AREA, aland)


### XPRX: Real export price 
GDPdef <- MAGNET1_2 %>%
  filter(variable %in% c("GDPT")) %>%
  mutate(variable = ifelse(unit == "mil USD", "GDPval", variable)) %>%
  select(-unit, -FSsector) %>%
  spread(variable, value) 

# Paasche price index
MAGNET3_raw[["XPRX"]] <- MAGNET1_2 %>%
  filter(variable %in% c("EXPO")) %>%
  mutate(variable = ifelse(unit == "mil USD", "EXPOval", variable)) %>%
  select(-unit) %>%
  spread(variable, value) %>%
  left_join(., GDPdef) %>%
  mutate(value = EXPOval/EXPO/GDPval*GDPT,
         variable = "XPRX",
         unit = "Paasche index") %>%
  select(-EXPO, -EXPOval, -GDPT, -GDPval)
rm(GDPdef)

### NETT: Net trade
MAGNET3_raw[["NETT"]]  <- MAGNET1_2 %>%
  filter(variable %in% c("EXPO", "IMPO")) %>%
  spread(variable, value) %>%
  mutate(value = EXPO - IMPO,
         variable = "NETT",
         unit = "mil 2007 con USD") %>%
  select(-EXPO, -IMPO)
  


####################
### AVAILABILITY ###
####################

AV <- list()

# AV1
### Per capita total amount of net calories available
# Nutrients
AV[["AV1"]] <- MAGNET1_2 %>%
  filter(variable %in% c("NQT", "POPT") & unit %in% c("CAL", "mil pers")) %>%
  group_by(scenario, FSregion, FSsector, year) %>%
  summarize(value = value[variable == "NQT"]/value[variable == "POPT"]/365) %>%
  mutate(unit = "kcal/cap/d",
         variable = "CALO")


# # AV2
# # Net Share of energy supply (calories) derived from dairy and fruit and vegetables
# # Nutrients
AV[["AV2"]] <- MAGNET1_2 %>%
  filter(variable %in% c("NQSECT") & unit %in% c("CAL") &  FSsector %in% c("TOT","CER")) %>%
  group_by(scenario, FSregion, year) %>%
  summarize(value = (value[FSsector == "CER"]/value[FSsector == "TOT"]*100)) %>%
  mutate(FSsector = "CER",
         unit = "%",
         variable = "CALO")

# AV3
# Average supply of protein derived from animal sources
AV[["AV3"]] <- MAGNET1_2 %>% 
  filter(variable %in% c("POPT") | (variable %in% c("NQSECT") & unit %in% c("PROT") & FSsector %in% c("LSP"))) %>%
  group_by(scenario, FSregion, year) %>%
  summarize(value = (value[variable == "NQSECT"]/value[variable == "POPT"]/365)) %>%
  mutate(FSsector = "LSP",
         unit = "grams/cap/day",
         variable = "PROT")

# AV4 Primary food production: PROD
AV[["AV4"]] <- MAGNET1_2 %>% 
  filter((variable %in% c("PROD") & unit %in% c("mil 2007 USD") & FSsector %in% c("PRIMFOOD")) | variable %in% c("POPT")) %>%
  group_by(scenario, FSregion, year) %>%
  summarize(value = (value[variable == "PROD"]/value[variable == "POPT"]/365)) %>%
  mutate(FSsector = "PRIMFOOD",
         unit = "mil 2007 USD/cap",
         variable = "PROD")

# AV5
### XPRP Real producer price 
GDPdef <- MAGNET1_2 %>%
  filter(variable %in% c("GDPT")) %>%
  mutate(variable = ifelse(unit == "mil USD", "GDPval", variable)) %>%
  select(-unit, -FSsector) %>%
  spread(variable, value)  

# Paasche price index
XPRP_p <- MAGNET1_2 %>%
  filter(variable %in% c("PROD")) %>%
  mutate(variable = ifelse(unit == "mil USD", "PRODval", variable)) %>%
  select(-unit) %>%
  spread(variable, value) %>%
  left_join(., GDPdef) %>%
  mutate(value = PRODval/PROD/GDPval*GDPT,
         variable = "XPRI",
         unit = "Paasche index") %>%
  select(-GDPT, -GDPval, -PROD, -PRODval)
rm(GDPdef)        

AV[["XPRP"]] <- XPRP_p  

# AV6 NETT: Net trade
# Already produced.

# AV7 Not relevant for MAGNET

####################
### ACCESIBILITY ###
####################

AC <- list()

# AC1
# Average share of food expenditures in total household expenditures
VPA <- bind_rows(
          current.f("VDPA", "BaseData_b.gdx", "VDPA", lookup_upd, "VDPA", c("TRAD_COMM", "REG"), c("TRAD_COMM","REG")) %>%
            mutate(unit = "mil. USD"),
          current.f("VIPA", "BaseData_b.gdx", "VIPA", lookup_upd, "VIPA", c("TRAD_COMM", "REG"), c("TRAD_COMM","REG")) %>%
            mutate(unit = "mil. USD")) %>%
       group_by(scenario, REG, year, TRAD_COMM) %>%
       summarize(value = sum(value)) %>%
       mutate(variable = "VPA",
              unit = "mil. USD")

VPA <- bind_rows(
      subtot_f(VPA, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_food),
      subtot_f(VPA, c("scenario", "year", "FSsector", "REG", "variable", "unit"), "value", map_tot)
)

VPA <- bind_rows(
  subtot_f(VPA, c("scenario", "year", "FSsector", "FSregion", "variable", "unit"), "value", map_fsreg),
  subtot_f(VPA, c("scenario", "year", "FSsector", "FSregion", "variable", "unit"), "value", map_wld)
)

AC[["AC1"]] <- VPA %>%
  group_by(scenario, FSregion, year) %>%
  summarize(value = value[FSsector == "FOOD"]/value[FSsector == "TOT"]*100) %>%
  mutate(FSsector = "TOT", 
          variable = "SHRFc",
          unit = "%")
rm(VPA)

# AC2
### GDP per capita
AC[["GDPC"]] <- MAGNET1_2 %>%
          filter(variable %in% c("GDPT", "POPT") & unit %in% c("mil USD", "mil pers")) %>% 
          select(-unit) %>%
          group_by(scenario, FSregion, FSsector, year) %>%
          summarize(value = value[variable == "GDPT"]/value[variable == "POPT"]) %>%
          mutate(variable = "GDPC",
                 unit = "mil 2007 USD/cap")

# AC3
### XFPI Domestic food price index
GDPdef <- MAGNET1_2 %>%
  filter(variable %in% c("GDPT")) %>%
  mutate(variable = ifelse(unit == "mil USD", "GDPval", variable)) %>%
  select(-unit, -FSsector) %>%
  spread(variable, value) 

# Paasche price index
XFPI_p <- MAGNET1_2 %>%
  filter(variable %in% c("PCONS")) %>%
  mutate(variable = ifelse(unit == "mil USD", "PCONSval", variable)) %>%
  select(-unit) %>%
  spread(variable, value) %>%
  left_join(., GDPdef) %>%
  mutate(value = PCONSval/PCONS/GDPval*GDPT,
         variable = "XFPI",
         unit = "Paasche index") %>%
  select(-GDPT, -GDPval, -PCONS, -PCONSval)
rm(GDPdef)    
 
# Laspeyers price index
# NOT CORRECT CHANGE
# XPRI_l <- MAGNET1_2 %>%
#       filter(variable %in% c("PROD", "PRODval")) %>%
#       select(-unit) %>%
#       spread(variable, value) %>%
#       left_join(., GDPdef) %>%
#       group_by(scenario, FSsector, FSregion) %>%
#       mutate(value = PRODval/PROD[year==2007]/GDPval*GDPT,
#                variable = "XPRI",
#                unit = "Laspeyeres index") %>%
#       select(-GDPT, -GDPval, - PROD, - PRODval)

# AC[["XPRI"]] <- bind_rows(XPRI_p, XPRI_l)  
AC[["XFPI"]] <- XFPI_p  
rm(XFPI_p)

# AC4 XPRM Imported food price index
GDPdef <- MAGNET1_2 %>%
  filter(variable %in% c("GDPT")) %>%
  mutate(variable = ifelse(unit == "mil USD", "GDPval", variable)) %>%
  select(-unit, -FSsector) %>%
  spread(variable, value) 

# Private consumption of imported products volume
priimpconsvol <- constant2.f("priimpconsvol", "BaseData_b.gdx", "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"), "qpm", c("TRAD_COMM", "REG"))
# Private consumption of imported products value
priimpconsval <- current.f("priimpconsval", "BaseData_b.gdx", "VIPM", lookup_upd, "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"))

# Paasche price index
AC[["XPRM"]] <-  MAGNET1_2 %>%
  filter(variable %in% c("VIPM")) %>%
  mutate(variable = ifelse(unit == "mil USD", "VIPMval", variable)) %>%
  select(-unit) %>%
  spread(variable, value) %>%
  left_join(., GDPdef) %>%
  mutate(value = VIPMval/VIPM/GDPval*GDPT,
         value = ifelse(is.nan(value), NA, value), # for some combinations data is zero resulting in NAN
         variable = "XFPI",
         unit = "Paasche index") %>%
  select(-GDPT, -GDPval, -VIPM, -VIPMval)
rm(GDPdef)  

# AC7 unskilled agricultural wage vs cereal prices
#[TO ADD]


### HOUSEHOLD LEVEL VARIABLES
MAGNET_HH <- list()

# AC6 HHCONS: Total private domestic consumption volume
# Private consumption of domestic products volume at hh level
VDPMH <- constant2.f("VDPMH", "BaseData_b.gdx", "VDPMH", c("TRAD_COMM", "HHS", "HH_REG"), c("TRAD_COMM", "HHS", "HH_REG"), "qpdh", c("TRAD_COMM", "HHS", "HH_REG"))
# Private consumption of imported products volume at hh level
VIPMH <- constant2.f("VIPMH", "BaseData_b.gdx", "VIPMH", c("TRAD_COMM", "HHS", "HH_REG"), c("TRAD_COMM", "HHS", "HH_REG"), "qpmh", c("TRAD_COMM", "HHS", "HH_REG"))

CONSHH <- rbind(VDPMH, VIPMH) %>%
  group_by(HH_REG, HHS, TRAD_COMM, scenario, year) %>%
  summarize(value = sum(value)) %>%
  rename(REG = HH_REG) %>%
  ungroup() %>%
  mutate(variable = "CONS",
         unit = "mil 2007 USD",
         REG = toupper(REG))

CONSHH <- bind_rows(
  subtot_f(CONSHH, c("scenario", "year", "FSsector", "REG", "variable", "unit", "HHS"), "value", map_cer),
  subtot_f(CONSHH, c("scenario", "year", "FSsector", "REG", "variable", "unit", "HHS"), "value", map_sec),
  subtot_f(CONSHH, c("scenario", "year", "FSsector", "REG", "variable", "unit", "HHS"), "value", map_agr),
  subtot_f(CONSHH, c("scenario", "year", "FSsector", "REG", "variable", "unit", "HHS"), "value", map_crp),
  subtot_f(CONSHH, c("scenario", "year", "FSsector", "REG", "variable", "unit", "HHS"), "value", map_food)
)

# POPT per HH group
POPH <- constant2.f("POPH", "BaseData_b.gdx", "POPH", c("HHS", "HH_REG"), c("HHS", "HH_REG"), "poph", c("HHS", "HH_REG")) %>%
  rename(REG = HH_REG,
         POPT = value) %>%
  mutate(REG = toupper(REG)) %>%
  select(-variable)

MAGNET_HH[["CONS"]] <- left_join(CONSHH, POPH) %>%
  mutate(value = value/POPT,
         value = ifelse(is.infinite(value), NA, value),
         unit = "mil 2007 USD/cap",
         variable = "CONS") %>%
  select(-POPT)
rm(CONSHH, POPH, VDPMH, VIPMH)


### STABILITY
ST <- list()

# S1
# # Cereal import dependency ratio
ST[["IMDR"]] <- MAGNET1_2 %>%
 filter(variable %in% c("PROD", "EXPO", "IMPO") &  FSsector %in% c("CER") & unit == "mil 2007 USD") %>%
 group_by(scenario, FSregion, FSsector, year) %>%
 summarize(value = (value[variable == "IMPO"] - value[variable == "EXPO"])/
                  (value[variable == "PROD"] + value[variable == "IMPO"] - value[variable == "EXPO"])*100) %>%
 mutate(variable = "IMDR",
         unit = "%")


# S2
# Value of food imports over total exports
ST[["SHRM"]] <- MAGNET1_2 %>% 
  filter((variable %in% c("EXPO") & FSsector %in% c("TOT") & unit == "mil 2007 USD") |
         (variable %in% c("IMPO") & FSsector %in% c("FOOD") & unit == "mil 2007 USD")) %>%
  group_by(scenario, FSregion, year) %>%
  summarize(value = (value[variable == "IMPO"]/ value[variable == "EXPO"])*100) %>%
  mutate(variable = "SHRM",
       unit = "%",
       sector = "FOOD")


### UTILIZATION
U <- list()

# U1
#Energy density indicator
# TBA

# U2
#CHECK DOES NOT WORK!!
# Net Share of energy supply (calories) derived from vegetables and fruits
U[["U1"]] <- MAGNET1_2 %>%
  filter(variable %in% c("NQSECT") & unit %in% c("CAL") &  FSsector %in% c("TOT","VFN")) %>%
  group_by(scenario, FSregion, year) %>%
  summarize(value = (value[variable == "NQSECT"]/value[variable == "NQT"]*100)) %>%
  mutate(FSsector = "VFN",
         unit = "%",
         variable = "CALO")

### SUSTAINABILITY
# Relevant for MAGNET?

#####################################
### MERGE ALL AND WRITE DATA FILE ###
#####################################

MAGNET_tot <- bind_rows(MAGNET1_2, MAGNET3_raw, AC, AV, U) %>%
                  mutate(model = "MAGNET",
                  year = as.numeric(year),
                  scenario = revalue(scenario, c("ECO_qpc_ti3_st" = "ECO", "FFANF_qpc_ti3_st"  = "FFANF", "ONEPW_qpc_ti3_st" = "ONEPW",  "TLTL_qpc_ti3_st" = "TLTL")),
                  Modelrun = "qpc_ti3_st")

#FSMIPPath <- "D:\\Dropbox\\FOODSECURE Scenarios\\Results"
FSMIPPath <- "./R/ProcessedModelResults"
#write.csv(MAGNET_tot, file.path(FSMIPPath, paste("MAGNET_FoodSecure_", Sys.Date(), ".csv", sep="")), row.names = F)
write.csv(MAGNET_tot, file.path(FSMIPPath, "MAGNET_qpc_ti3_st.csv"), row.names = F)
xtabs(~FSsector+variable, data = MAGNET_tot)
