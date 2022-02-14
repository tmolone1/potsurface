library(tidyverse)
library(lubridate)
library(raster)
library(sf)
library(ggplot2)
source("./scripts/Functions.R")

# read location coordinates
read1<-read_csv("./data/List-20220202102646.csv",skip=1)
read2<-read_csv("./data/List-20220202103020.csv",skip=1)
locs<-merge(read1 %>% dplyr::select(c(Location,Value)), read2 %>% dplyr::select(c(Location,Value)), by="Location", all=FALSE)
names(locs)[2:3]<-c("long","lat")
locs<-locs%>% group_by(Location) %>% summarize(long = mean(long), lat=mean(lat))
locs<-locs %>% filter(long < 0)

#read data and collapse to long tidy table
read<-read_csv("./data/BulkExport-14 Locations-20220212153316.csv",skip=1)
dattbl<-read %>% pivot_longer(cols = 2:ncol(read),names_to = "well_identifier", values_to = "level_ft")
well_names<-dattbl[1:ncol(read)-1,]
# elevations manually obtained be querying https://elevation.nationalmap.gov/arcgis/services/3DEPElevation/ImageServer/WCSServer
tbl<-merge(dattbl,locs,by.x=3,by.y = "Location", all=FALSE)
names(tbl)[1:2]<-c("well_name","elevation")
tbl$elevation<-rep(0,nrow(tbl))
r<-raster("./data/NED_stpln_ft.tif")
x<-tbl$long
y<-tbl$lat
z<-tbl$elevation
coords<-CRS("+init=EPSG:4326")
Locids<-tbl$well_name
pts<-mypts(x,y,z,coords,Locids)
pts.transform<-spTransform(pts,CRS("+init=EPSG:3755"))
pts.transform$elev<-raster::extract(r, pts.transform)
tbl$elevation<-pts.transform$elev
dattbl<-dattbl[ncol(read)*3+1:nrow(dattbl),]  # remove unused data at top of table

names(dattbl)[1]<-"timestamp"
dattbl<-dattbl %>% filter(!is.na(level_ft))
dattbl<-merge(dattbl,tbl,by="well_identifier")
dattbl$level_ft<-as.numeric(dattbl$level_ft)
dattbl <- dattbl %>% mutate(ps_elev=elevation-level_ft) %>% filter(!is.na(ps_elev))

#format date correctly
dattbl$timestamp <- as.Date(ymd_hms(dattbl$timestamp))


# analysis related to individual locations
sum<-dattbl %>% group_by(well_name) %>% mutate(pct=percent_rank(ps_elev), n_obs=n(), min_date=min(timestamp), max_date=max(timestamp)) %>% slice(which.max(timestamp))
write_csv(sum,"./outputs/summary.csv")
