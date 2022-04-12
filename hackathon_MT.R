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

imp_count <- imp %>%
  count(BASIN)

imp_3 <- imp %>%
  filter(PRIORITY.RANK == 3) 
imp_3_count2018 <- imp_3 %>%
  count(BASIN)
#highest: 1) Peedee = 251, 2)Santee = 181

imp_2 <- imp %>%
  filter(PRIORITY.RANK == 2)
imp_2_count2018 <- imp_2 %>%
  count(BASIN)
#highest: 1) Catawaba = 46, Saluda = 46

imp_1 <- imp %>%
  filter(PRIORITY.RANK == 1)
imp_1_count2018 <- imp_1 %>%
  count(BASIN)
# highest: 1) savannah = 1 

library(readxl)

imp2016 <- readxl::read_excel("data/SC_303d_lists_2006to2016/PN_2016303d_final.xls")
imp2016

imp_count2016 <- imp2016 %>%
  count(BASIN)


imp16_3 <- imp2016 %>%
  filter(`PRIORITY RANK` == 3) 
imp_3_count2016 <- imp16_3 %>%
  count(BASIN)
#highest: 1) Peedee = 240, 2) Santee = 148

imp16_2 <- imp2016 %>%
  filter(`PRIORITY RANK` == 2) 
imp_2_count2016 <- imp16_2 %>%
  count(BASIN)
#highest: 1) Saluda = 21, 2) Santee = 20 

imp16_1 <- imp2016 %>%
  filter(`PRIORITY RANK` == 1) 
imp_1_count2016 <- imp16_1 %>%
  count(BASIN)
# highest: 1) Catawba = 40, 2) Peedee = 9 

