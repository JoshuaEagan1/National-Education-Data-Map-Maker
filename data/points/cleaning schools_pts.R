#cleaning National building points

#loading packages
library(tidyverse)
library(openxlsx)

#importing data
load("C:/Users/eaganj1/Desktop/NCES Mapper Tool/app/data/districts simple feature.R")
setwd("C:/Users/eaganj1/Desktop/NCES Mapper Tool/app/data/points")
points<-read.xlsx("school points data.xlsx")

#checks
identifiers<-str_split(points$`State.School.ID.[Public.School].2019-20`, "-", simplify = TRUE)
identifiers<-as.data.frame(identifiers)
identifiers <- identifiers[order(identifiers$V1),]
table(identifiers$V1, identifiers$V4=="")
#OK, IL, and HI have different format than the rest

HI<-points %>% filter(tolower(`State.Name.[Public.School].Latest.available.year`)=="hawaii")
#3rd and 4th hyphenated sections in `State.Name.[Public.School].Latest.available.year`
# variable are district and school codes respectively.

IL<-points %>% filter(tolower(`State.Name.[Public.School].Latest.available.year`)=="illinois")
IL_strings<-identifiers %>% filter(V1=="IL")
#second string is region code, third string is county code
OK<-points %>% filter(tolower(`State.Name.[Public.School].Latest.available.year`)=="oklahoma")

#solution: separate internal identifier variables for schools and districts
names(points)<-c("Name", "State", "NCES_Identifier", "County_Number", "Lat", "Lon", "Internal_School_Identifier", "Internal_District_Identifier")
points<- points %>% mutate(Internal_School_Identifier=
                             substr(Internal_School_Identifier, nchar(Internal_District_Identifier)+2, nchar(Internal_School_Identifier))
                           ) %>% select(c(1,3,5,6,7,8))
points[,3:4]<-lapply(points[,3:4], as.numeric)
#points<-SpatialPointsDataFrame(coords=points[,2:3], data=points[,c(1,4)], 
#        , proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

save(points, file="C:/Users/eaganj1/Desktop/NCES Mapper Tool/app/data/schools_pts.R", replace=T)