#cleaning the districts shapefile- adding internal district code

#importing packages
library(tidyverse)
library(sp)
library(sf)
library(rgdal)
library(openxlsx)

#reading in shape file...
districts<-readOGR(dsn="./EDGE_SCHOOLDISTRICT_TL20_SY1920"
                   , "EDGE_SCHOOLDISTRICT_TL20_SY1920")
districts<-st_as_sf(districts)
names(districts)[5]<-"NCES_identifier"

#reading in crosswalk to internal district code
crosswalk<-read.xlsx("./ELSI_excel_export_6376850743830017197307/internal to NCES crosswalk.xlsx")
names(crosswalk)<-c("Name", "State", "NCES_identifier", "Internal_identifier")

#merging
districts<-left_join(districts, crosswalk) %>% select(State, NAME, GEO_YEAR, NCES_identifier, Internal_identifier, geometry)
#178 districts have NCES codes but not internal codes. that's okay.

#changing from UTM to USGS coordinates
Sys.time()
districts <- st_transform(districts, "+proj=longlat +datum=WGS84 +no_defs +type=crs")
Sys.time()

names(districts)[4:5]<-c("NCES_district_identifier", "Internal_district_identifier")

save(districts, file="../districts simple feature.R", replace=T)