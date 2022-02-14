library(gstat)
source("./scripts/data_wrangle.R")


# set date range and aggregate
range_min<-Sys.Date()-180
range_max<-Sys.Date()

rpt<-dattbl %>% filter(timestamp>range_min & timestamp<range_max) %>% group_by(well_name) %>% summarize(lat=mean(lat), long=mean(long),elev=mean(ps_elev))

source("./scripts/create_shapefiles.R")
source("./scripts/gridgen.R")

coordinates(rpt)<- ~long+lat

#Fit variogram and display plot to inspect goodness of fit
v.fit2<-fit.variogram(variogram(elev~1, locs_sp), vgm(psill = 4000, "Exp", range = 7000, nugget = 0), 
                      fit.sills = TRUE, fit.ranges = FALSE, fit.method=7)
plot(variogram(elev~1, locs_sp), model = v.fit2)

# ordinary kriging
x <- krige(elev~1, locs_sp, grd_sp, model = v.fit2)
#spplot(x["var1.pred"], main = "ordinary kriging predictions")

#contour
krig1<-rasterFromXYZ(x["var1.pred"])
crs(krig1)<-crs(locs_sp)
cl<-rasterToContour(krig1, levels=as.numeric(seq(4400,5400,100)))
plot(krig1)
lines(cl)
