library(gstat)
source("./scripts/data_wrangle.R")
source("./scripts/create_shapefiles.R")
source("./scripts/gridgen.R")

# set date range and aggregate
range_min<-Sys.Date()-180
range_max<-Sys.Date()
rpt<-dattbl %>% filter(timestamp>range_min & timestamp<range_max) %>% group_by(well_name) %>% summarize(lat=mean(lat), long=mean(long),level=mean(ps_elev))



elev<-rpt$level
pts<-SpatialPointsDataFrame(data.frame(rpt$long, rpt$lat), 
                                proj4string=CRS("+init=EPSG:4326"),
                                data=rpt)

# #Fit variogram and display plot to inspect goodness of fit
# v.fit2<-fit.variogram(variogram(elev~1, pts), vgm(psill = 4000, "Exp", range = 7000, nugget = 0), 
#                       fit.sills = TRUE, fit.ranges = FALSE, fit.method=7)
# plot(variogram(elev~1, pts), model = v.fit2)
# 
# # ordinary kriging
# x <- krige(elev~1, locations = pts, newdata = grid, model = v.fit2)
# spplot(x["var1.pred"], main = "ordinary kriging predictions")
# 
# #contour
# krig1<-raster(x)
# cl<-rasterToContour(krig1, levels=as.numeric(seq(4400,5400,100)))
# 
# locs_sp@data<-pts@data
# locs_sf<-st_as_sf(locs_sp)

# TPS
TPS<-myTPS(x,y,elev,grid,coords)

#Contor
conts<-mycontours(TPS,seq(4400,5600,50))
conts<-spTransform(conts,CRS("+init=EPSG:3755")) # wyoming state plane east nad83 Harn 
conts<-st_as_sf(conts)
st_write(conts, dsn = "./outputs", layer = "potsurface_contours", driver = "ESRI Shapefile", append=FALSE) # write shapefile
writeRaster(TPS, filename="./outputs/potsurface_raster.grd", format="GTiff", overwrite=TRUE) # write raster
