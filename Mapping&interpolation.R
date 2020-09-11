## This code separates the well data from Sunburst into the 3 hydrostratigraphic units
###  Competent Shale, Weathered Shale, and Alluvial Sediments. 
### it Cross checks data within each unit for distance to nearby points and forces a choice between wells that are too 
### close together.  
### It then creates IDW interpolation rasters and ESRI Shapefiles for each hydrostratigraphic unit.
# cleanup
rm(list=ls())
setwd("C:/Users/tmoloney/OneDrive - Trihydro/Workflows/Sunburst/7_LNAPL_Data_Processing/Produces")
load("map_effsols.Rda")
setwd("C:/Users/tmoloney/OneDrive - Trihydro/Workflows/Sunburst/4_GIS_Mapping_WF/Produces")
load("df_all.Rda")
library(sp)
library(raster)
library(rgdal)
library(rgeos)
well_screens<- read.csv("M:/Chevron/Sunburst/ProjectDocs/Sitewide/3DModel/Data/well_screens.csv")
Wells_points <- SpatialPointsDataFrame(df_all[,2:3], 
                                       data= df_all,
                                       proj4string = CRS("+init=ESRI:102700")) # montana state plane NAD83

Wells_points[zerodist(Wells_points)[,1],] # Prints duplicates, should be 0 features
# Bring in LNAPL data
map_effsols<-merge(map_effsols,well_screens[,c("Location","HS_UNIT2")], by.x="Well",by.y="Location", all.x=TRUE)
map_effsols_ws<- SpatialPointsDataFrame(map_effsols[map_effsols$HS_UNIT2=="WS",c("X","Y")],
                                        data= map_effsols[map_effsols$HS_UNIT2=="WS",],
                                        proj4string = CRS("+init=ESRI:102700")) # montana state plane NAD83
map_effsols_cs<- SpatialPointsDataFrame(map_effsols[map_effsols$HS_UNIT2=="COMP",c("X","Y")],
                                        data= map_effsols[map_effsols$HS_UNIT2=="COMP",],
                                        proj4string = CRS("+init=ESRI:102700")) # montana state plane NAD83
map_effsols<- SpatialPointsDataFrame(map_effsols[,c("X","Y")],
                                     data= map_effsols,
                                     proj4string = CRS("+init=ESRI:102700")) # montana state plane NAD83

# setup a grid for interpolations
grd <- as.data.frame(spsample(Wells_points, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
# Add Wells_points's projection information to the empty grid
proj4string(grd) <- proj4string(Wells_points)
plot(grd)
points(Wells_points)

#model value handling
Wells_points$model_value<-ifelse(Wells_points$model_value<=0.005, 0, Wells_points$model_value)
Wells_points$model_value<-as.numeric(Wells_points$model_value)

Wells_points<-Wells_points[!(unique(Wells_points$Location) %in% unique(map_effsols$Well)),]  # drop wells with current LNAPL

#subset the overall wells data by hydrostratigraphic unit
ws<-subset(Wells_points, HS_UNIT2=="WS")
as<-subset(Wells_points, HS_UNIT2=="AS")
comp<-subset(Wells_points, HS_UNIT2=="COMP")
as_int<-subset(Wells_points, HS_UNIT2=="AS" | (HS_UNIT2=="WS" & model_value == 0))  # for interpolation


#### filtering out wells screened at different levels of weathered shale, and distance in space & time handling #####
d <- gDistance(ws, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
newdata <- cbind(ws, ws[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))
colnames(newdata@data)[ncol(newdata)]<-"distance"

# identify locations with 20 feet of each other, generally wells screened at different levels, and force the code to pick one (more recent data prefferred)
pairs<-newdata@data[,c("Location","Location.1","distance")]
choose<-pairs[pairs$distance<21,c(1,2)]
picks<-vector(mode="list", length=nrow(choose))
for (i in seq(1,nrow(choose))) {
  pick<-df_all[df_all$Location %in% choose[i,],]
  if (pick$nearest_date[1]==pick$nearest_date[2]) {
    pick<-pick[which(pick$Status=="Active"),]
  }
  pick_id<-pick$Location[order(pick$nearest_date, decreasing=TRUE)[1]]
  picks[i]<-pick_id 
}
choose$pick<-picks
pts_int_ws<-subset(ws, !Location %in% setdiff(unique(unlist((choose[,c(1,2)]))),unique(picks)))

# drop locations where data is old, and there is a nearby location with more recent data
old<-subset(newdata, newdata$nearest_date<"2011-01-01")
pts_int_ws<-subset(pts_int_ws, !Location %in% setdiff(old$Location[old$distance<200 & old$model_value>0 & old$nearest_date.1>"2011-01-01"],(old$Location.1[old$model_value.1>0 & old$nearest_date<"2011-01-01"])))
d <- gDistance(pts_int_ws, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
newdata <- cbind(pts_int_ws, pts_int_ws[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))
colnames(newdata@data)[ncol(newdata)]<-"distance"
pairs<-newdata@data[newdata$nearest_date<"2011-01-01" & newdata$model_value>0 & newdata$distance<200,c("Location","Location.1","distance")]
old<-subset(newdata, newdata$nearest_date<"2011-01-01")
choose<-pairs[pairs$distance<100,c(1,2)]
picks<-vector(mode="list", length=nrow(choose))
for (i in seq(1,nrow(choose))) {
  pick<-df_all[df_all$Location %in% choose[i,],]
  if (pick$nearest_date[1]==pick$nearest_date[2]) {
    pick<-pick[which(pick$Status=="Active"),]
  }
  if (lubridate::year(pick$nearest_date[1])==lubridate::year(pick$nearest_date[2])) {
    pick<-pick[order(pick$model_value, decreasing=TRUE)[1],]
  }
  pick_id<-pick$Location[order(pick$nearest_date, decreasing=TRUE)[1]]
  picks[i]<-pick_id 
}
choose$pick<-picks
pts_int_ws<-subset(pts_int_ws, !Location %in% setdiff(unique(unlist((choose[,c(1,2)]))),unique(picks)))

### filtering out LNAPL wells that are too close together (<10 ft)
d <- gDistance(map_effsols_ws, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
newdata <- cbind(map_effsols_ws, map_effsols_ws[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))
colnames(newdata@data)[ncol(newdata)]<-"distance"
pairs<-newdata@data[,c("Well","Well.1","distance")]
pairs<-subset(pairs, Well!=Well.1)
choose<-pairs[pairs$distance<10,c(1,2)]
picks<-vector(mode="list", length=nrow(choose))
for (i in seq(1,nrow(choose))) {
  pick<-map_effsols[map_effsols$Well %in% unlist(as.list(choose[i,])),]
  if (pick$last_date[1]==pick$last_date[2]) {
    pick<-pick[which(pick$pers>min(pick$pers)),]
  }
  pick_id<-pick$Well[order(pick$last_date, decreasing=TRUE)[1]]
  picks[i]<-as.character(pick_id)
}
choose$pick<-picks
excl<-subset(map_effsols_ws, Well %in% setdiff(unique(unlist((choose[,c(1,2)]))),unique(picks)))
map_effsols_ws<-subset(map_effsols_ws, !Well %in% setdiff(unique(unlist((choose[,c(1,2)]))),unique(picks)))
### end filtering

ws_not_int<-ws[ws$Location %in% setdiff(ws$Location,pts_int_ws$Location),]

#merge filtered dataset with WS LNAPL wells for WEATHERED SHALE
interp<-merge(pts_int_ws[,c("Location","X","Y","model_value")]@data, map_effsols_ws[,c("Well","X","Y","Benz_EffSol")]@data, by.x = c("Location","X","Y","model_value"),by.y = c("Well","X","Y","Benz_EffSol"), all=TRUE)
interp<- SpatialPointsDataFrame(interp[,c("X","Y")],
                                data= interp,
                                proj4string = CRS("+init=ESRI:102700")) # montana state plane NAD83

#Log_Transform
pre_log_shift<-(abs(min(interp$model_value))+0.001)
interp$model_value<-interp$model_value+pre_log_shift
interp$model_value<-log(interp$model_value, base=10)

# interpolation, idw
idw <- gstat::idw(model_value ~ 1, interp, newdata=grd, idp=6)
idw <-raster(idw)
idw<-10^(idw)-pre_log_shift
plot(idw)

proj4string(idw) <- proj4string(interp)

idw_for_ndcontour<-idw
points(interp)
contour(idw_for_ndcontour, levels=0.005, add=TRUE)
idw_for_ndcontour<-(idw_for_ndcontour>0.005)*idw_for_ndcontour # clip to extent of > 0.005
interp$model_value <- 10^(interp$model_value)-pre_log_shift

###COMPETENT SHALE
# filtering out wells screened at multiple levels in competent shale
d <- gDistance(comp, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
newdata <- cbind(comp, comp[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))
colnames(newdata@data)[ncol(newdata)]<-"distance"

# identify locations with 20 feet of each other, generally wells screened at different levels, and force the code to pick one (more recent data prefferred)
pairs<-newdata@data[,c("Location","Location.1","distance")]
choose<-pairs[pairs$distance<21,c(1,2)]
picks<-vector(mode="list", length=nrow(choose))
for (i in seq(1,nrow(choose))) {
  pick<-df_all[df_all$Location %in% choose[i,],]
  if (pick$nearest_date[1]==pick$nearest_date[2]) {
    pick<-pick[which(pick$Status=="Active"),]
  }
  pick_id<-pick$Location[order(pick$nearest_date, decreasing=TRUE)[1]]
  picks[i]<-pick_id 
}
choose$pick<-picks
pts_int_comp<-subset(comp, !Location %in% setdiff(unique(unlist((choose[,c(1,2)]))),unique(picks)))
pts_int_comp<-subset(pts_int_comp, !Location %in% c("P2RI-46DFL","P2RI-46DFU")) # exclude these locations (they aren't impacted but P2RI-46D is)
cs_not_int<-comp[comp$Location %in% setdiff(comp$Location,pts_int_comp$Location),]
not_int<-Wells_points[Wells_points$Location %in% union(cs_not_int$Location,ws_not_int$Location), ]
int<-Wells_points[!(Wells_points$Location %in% union(cs_not_int$Location,ws_not_int$Location)), ]

#merge filtered dataset with CS LNAPL wells for COMPETENT SHALE
interp<-merge(pts_int_comp[,c("Location","X","Y","model_value")]@data, map_effsols_cs[,c("Well","X","Y","Benz_EffSol")]@data, by.x = c("Location","X","Y","model_value"),by.y = c("Well","X","Y","Benz_EffSol"), all=TRUE)
dummies<-data.frame(Location=factor(c("eastofSDW-26D","westofSDW-19D","southofSDW-19D")),
                    X=c(1387088,1384370,1385221),
                    Y=c(1698635,1694213,1693616),
                    model_value=rep(0,3))
interp<-rbind(interp,dummies)
interp<- SpatialPointsDataFrame(interp[,c("X","Y")],
                                data= interp,
                                proj4string = CRS("+init=ESRI:102700")) # montana state plane NAD83

#Log_Transform
pre_log_shift<-(abs(min(interp$model_value))+0.001)
interp$model_value<-interp$model_value+pre_log_shift
interp$model_value<-log(interp$model_value, base=10)

# interpolation, idw
idw <- gstat::idw(model_value ~ 1, interp, newdata=grd, idp=6)
idw <-raster(idw)
idw<-10^(idw)-pre_log_shift
plot(idw)

proj4string(idw) <- proj4string(interp)

idw_for_ndcontour_cs<-idw
contour(idw_for_ndcontour_cs, levels=0.005, add=TRUE)
idw_for_ndcontour_cs<-(idw_for_ndcontour_cs>0.005)*idw_for_ndcontour_cs # clip to extent of > 0.005
interp$model_value <- 10^(interp$model_value)-pre_log_shift

###ALLUVIAL SEDIMENTS
# filtering out wells screened at multiple levels in alluvial sediments
d <- gDistance(as_int, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
newdata <- cbind(as_int, as_int[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))
colnames(newdata@data)[ncol(newdata)]<-"distance"

# identify locations with 20 feet of each other, generally wells screened at different levels, and force the code to pick one (more recent data prefferred)
pairs<-newdata@data[,c("Location","Location.1","distance")]
choose<-pairs[pairs$distance<50,c(1,2)]
picks<-vector(mode="list", length=nrow(choose))
for (i in seq(1,nrow(choose))) {
  pick<-df_all[df_all$Location %in% choose[i,],]
  if (pick$HS_UNIT2[1]!=pick$HS_UNIT2[2]) {
    pick<-pick[which(pick$HS_UNIT2=="AS"),]
  }
  if (pick$nearest_date[1]==pick$nearest_date[nrow(pick)]) {
    pick<-pick[which(pick$Status=="Active"),]
  }
  pick_id<-pick$Location[order(pick$nearest_date, decreasing=TRUE)[1]]
  picks[i]<-pick_id 
}
choose$pick<-picks
pts_int_as<-subset(as_int, !Location %in% setdiff(unique(unlist((choose[,c(1,2)]))),unique(picks)))

#interpolation, Alluvial Sediments
interp<-pts_int_as[,c("Location","X","Y","model_value")]@data
dummies<-data.frame(Location=factor(c("SD01-TP-07","SD01-TP-08","SD01-TP-09")),
                    X=c(1386149,1386285,1385997),
                    Y=c(1696672,1696721,1697043),
                    model_value=rep(0,3))
interp<-rbind(interp,dummies)
interp<- SpatialPointsDataFrame(interp[,c("X","Y")],
                                data= interp,
                                proj4string = CRS("+init=ESRI:102700")) # montana state plane NAD83

#Log_Transform
pre_log_shift<-(abs(min(interp$model_value))+0.001)
interp$model_value<-interp$model_value+pre_log_shift
interp$model_value<-log(interp$model_value, base=10)

# interpolation, idw
idw <- gstat::idw(model_value ~ 1, interp, newdata=grd, idp=6)
idw <-raster(idw)
idw<-10^(idw)-pre_log_shift
plot(idw)

proj4string(idw) <- proj4string(interp)

idw_for_ndcontour_as<-idw
contour(idw_for_ndcontour_as, levels=0.005, add=TRUE)
idw_for_ndcontour_as<-(idw_for_ndcontour_as>0.005)*idw_for_ndcontour_as # clip to extent of > 0.005
interp$model_value <- 10^(interp$model_value)-pre_log_shift

well_screens <- SpatialPointsDataFrame(well_screens[,c("X","Y")], 
                                       data= well_screens,
                                       proj4string = CRS("+init=ESRI:102700")) # montana state plane NAD83

#### write outputs
setwd("M:/Chevron/Sunburst/GIS/Projects/Groundwater/AnnualReports/Data/Shapefiles/2019")
writeOGR(Wells_points, dsn = getwd(), layer = "GW_analytical_results", driver = "ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(well_screens[(!(well_screens$Location %in% Wells_points$Location) & well_screens$Location %in% map_effsols$Well) | well_screens$Location %in% Wells_points$Location,], dsn = getwd(), layer = "All_Wells", driver = "ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(not_int, dsn = getwd(), layer = "not_interpolated", driver = "ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(excl, dsn = getwd(), layer = "LNAPL_not_interpolated", driver = "ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(int, dsn = getwd(), layer = "interpolated", driver = "ESRI Shapefile", overwrite_layer=TRUE)

### write output for WEATHERED SHALE
writeRaster(idw_for_ndcontour, filename="idwbound_ws_benz.grd", format="GTiff", overwrite=TRUE)
cl<-rasterToContour(idw_for_ndcontour, levels=0.005)
writeOGR(cl, dsn = getwd(), layer = "contour_mcl_ws_benz", driver = "ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(pts_int_ws, dsn = getwd(), layer = "ws_int_wells_benzene", driver = "ESRI Shapefile",overwrite_layer=TRUE)
writeOGR(ws_not_int, dsn = getwd(), layer = "ws_wells_benzene_excl", driver = "ESRI Shapefile",overwrite_layer=TRUE)
writeOGR(map_effsols_ws, dsn = getwd(), layer = "ws_LNAPL_wells", driver = "ESRI Shapefile", overwrite_layer=TRUE)

### write output for COMPETENT SHALE
writeRaster(idw_for_ndcontour_cs, filename="idwbound_cs_benz.grd", format="GTiff", overwrite=TRUE)
cl_cs<-rasterToContour(idw_for_ndcontour_cs, levels=0.005)
writeOGR(cl_cs, dsn = getwd(), layer = "contour_mcl_cs_benz", driver = "ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(comp, dsn = getwd(), layer = "cs_wells_benzene", driver = "ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(map_effsols_cs, dsn = getwd(), layer = "cs_LNAPL_wells", driver = "ESRI Shapefile", overwrite_layer=TRUE)

### write output for ALLUVIAL SEDIMENTS
writeRaster(idw_for_ndcontour_as, filename="idwbound_as_benz.grd", format="GTiff", overwrite=TRUE)
cl_as<-rasterToContour(idw_for_ndcontour_as, levels=0.005)
writeOGR(cl_as, dsn = getwd(), layer = "contour_mcl_as_benz", driver = "ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(as, dsn = getwd(), layer = "as_wells_benzene", driver = "ESRI Shapefile", overwrite_layer=TRUE)

sum3hsu<-idw_for_ndcontour+idw_for_ndcontour_as+idw_for_ndcontour_cs
writeRaster(sum3hsu, filename="sum3hsu.grd", format="GTiff", overwrite=TRUE)
### Code is verified to run to this point as of 2/13/2020.  There are no known deficiencies.  
### It is possible, and likely, that there are inconsistencies between the specifics of code execution when filtering out wells
### and the described behavior of the code in workflow documentation.
