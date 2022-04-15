# hack-a-thon 2022

library(tidyverse)
library(sf)

# Read in boundaries of 8 major river basins in SC
# https://sc-department-of-health-and-environmental-control-gis-sc-dhec.hub.arcgis.com/datasets/sc-major-river-basins/explore
basin = st_read("data/SC_Major_River_Basins", "SC_Major_River_Basins")
basin

# Read in shapefile of all sites assessed for 2018 303d list
# Provided by Wade Cantrell at SC DHEC on 2022-04-11
shp_imp = st_read("data/bw.SDE.DHEC_303D_18", "bw.SDE.DHEC_303D_18")
shp_imp

# Larger shapefile of ALL stations (including shellfish monitoring sites, macroinvertebrate sites, much more than the ambient water quality sites used for the 303d list)
# Provided by Wade Cantrell at SC DHEC on 2022-04-11
shp_all = st_read("data/bw.SDE.STATIONS", "bw.SDE.STATIONS")

# SC 2018 303d list: https://scdhec.gov/bow/south-carolina-303d-list-impaired-waters-tmdls
# Converted 303d list from .xls to .csv before reading in
imp = read.csv("data/2018303d_final.csv") # SC Impaired Waters List 303d for 2018
summary(imp) # There are 861 empty rows

# Get rid of empty rows
dim(imp) #1979 rows
imp = imp %>%
  filter(ï..PRIORITY.RANK != "") 
dim(imp) # 1979-861 = 1118 rows

# Plot locations of impaired waters and stations that are not on the list (either bc they meet water quality standards or have a TMDL)
# TMDL = Total Maximum Daily Load - indicates site where approved pollutant limits have been identified and waters are being managed to meet these TMDL
ggplot() +
  geom_sf(data=shp_imp, aes(color=STATUS))

# Plot boundaries of 8 major river basins in SC
ggplot() +
  geom_sf(data=basin, aes(fill=Basin))

summary(imp)
summary(shp_imp)

#----------------------------------------------------
#Organizing and plotting the ranking of SC impaired waters
#----------------------------------------------------

# IMPAIRED WATERS 2018
class(shp_imp$HUC_12)
shp_imp$HUC_12 = as.numeric(shp_imp$HUC_12)
impaired_map_data = imp %>%
  left_join(shp_imp, by = "HUC_12") %>%
  mutate(ï..PRIORITY.RANK = substring(ï..PRIORITY.RANK, 1,1))
summary(impaired_map_data)
impaired_map_data$ï..PRIORITY.RANK
# PLOTTING IMPAIRED WATERS 2018
ImpairedWaterMap2018 = ggplot() +
  geom_sf(data=basin, aes(fill=Basin), alpha = 0.2)+
  geom_sf(data=impaired_map_data$geometry, aes(color=Priority_Rank)) +
  scale_color_discrete(name = c("Priority Rank","Basin"))
ggsave(ImpairedWaterMap2018, filename='figures/impaired_waters_2018_ranking.pdf', height=5, width=9)

# INSTALLING 2016 DATA
install.packages("readxl")
library(readxl)
imp2016 = read_excel("data/SC_303d_lists_2006to2016/PN_2016303d_final.xls")
summary(imp2016)

glimpse(imp2016)
#filter out NAs
imp2016 = imp2016 %>%
  filter(`PRIORITY RANK` != "")

# IMPAIRED WATERS 2016
class(imp2016$HUC_12)
imp2016$HUC_12 = as.numeric(imp2016$HUC_12)
impaired2016_map_data = imp2016 %>%
  left_join(shp_imp, by = "HUC_12") %>%
  mutate(`PRIORITY RANK` = substring(`PRIORITY RANK`, 1,1))
summary(impaired2016_map_data)
class(impaired2016_map_data$`PRIORITY RANK`)

#PLOTTING IMPAIRED WATERS 2016
ImpairedWaterMap2016 = ggplot() +
  geom_sf(data=basin, aes(fill=Basin), alpha = 0.2)+
  geom_sf(data=impaired2016_map_data$geometry, aes(color=impaired2016_map_data$`PRIORITY RANK`))+
  scale_color_discrete(name = c("Priority Rank","Basin"))
ggsave(ImpairedWaterMap2016, filename='figures/impaired_waters_2016_ranking.pdf', height=5, width=9)

# lets try thet GitHub commit again