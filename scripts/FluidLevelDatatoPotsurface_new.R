library(gstat)
source("./scripts/data_wrangle.R")
source("./scripts/create_shapefiles.R")
source("./scripts/gridgen.R")

# set date range and aggregate
range_min<-Sys.Date()-20000
range_max<-Sys.Date()
rpt<-tbl %>% filter(timestamp>range_min & timestamp<range_max) %>% group_by(well_name) %>% summarize(lat=mean(lat), long=mean(long),level=mean(ps_elev))



elev<-rpt$level
pts<-SpatialPointsDataFrame(data.frame(rpt$long, rpt$lat), 
                                proj4string=CRS("+init=EPSG:4326"),
                                data=rpt)

#Fit variogram and display plot to inspect goodness of fit
v.fit2<-fit.variogram(variogram(elev~1, pts), vgm(psill = 4000, "Exp", range = 7000, nugget = 0), 
                      fit.sills = TRUE, fit.ranges = FALSE, fit.method=7)
plot(variogram(elev~1, pts), model = v.fit2)

# ordinary kriging
x <- krige(elev~1, locations = pts, newdata = grid, model = v.fit2)
spplot(x["var1.pred"], main = "ordinary kriging predictions")

#contour
krig1<-raster(x)
cl<-rasterToContour(krig1, levels=as.numeric(seq(4400,5400,100)))

locs_sp@data<-pts@data
locs_sf<-st_as_sf(locs_sp)
