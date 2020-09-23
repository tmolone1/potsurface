
### this code will not run due to lack of input data in this repository.  But, the code contains the basic outline of steps to 
# produce a potentiometric surface map.

library(sp)
library(raster)
library(rgdal)
library(readr)
library(readxl)
library(gstat)
collar<-read_csv("collar.csv") # from output of "WriteCollar&LithologyFiles.R"
water_levels<-read_excel("L1B Water Levels.1.xlsx", col_types=c("text", "date", "numeric", "text", "numeric"))
water_levels[water_levels$LOCID=='BGMW-01',]$LOCID<-"BG-MW01"  # fix mismatched ID typo

# create SPDF from collar file
Wells_points <- SpatialPointsDataFrame(collar[,c(3,2)], 
                                       data= collar,
                                       proj4string = CRS("+init=ESRI:102654")) # colorado state plane central NAD83

plot(Wells_points) # displays all wells
points(Wells_points[Wells_points$LOCATION %in% water_levels$LOCID,],pch=15)# displays the points with water level data

# merge XY coordinates from collar file to water levels sheet
water_levels<-merge(collar[,c(1:3)],water_levels,by.x=1,by.y=1,all.y=TRUE)
write_csv(water_levels, "April2020_WaterLevels.csv")

# create SPDF from water level data and write shapefile
WL_pts <- SpatialPointsDataFrame(water_levels[,c(3,2)], 
                                 data= water_levels,
                                 proj4string = CRS("+init=ESRI:102654")) # colorado state plane central NAD83
writeOGR(WL_pts, dsn=getwd(), layer = "April2020_WaterLevels", driver = "ESRI Shapefile", overwrite_layer=TRUE)
###USE to write directly to M drive
##writeOGR(WL_pts, dsn="M:/ItoN/MidnightSunTrinityJV/Modeling/Shapefiles", layer = "April2020_WaterLevels", driver = "ESRI Shapefile", overwrite_layer=TRUE)

# manually exclude locations from interpolation
suppress<-c("IP-37") # list IDs to locations to be excluded
# remove locations with NA data and suppressed locations from the water level data sheet
WL_pts_int<-WL_pts[!is.na(WL_pts$`GW ELEV`) & !(WL_pts$LOCATION %in% suppress),]

# setup a grid for interpolations
grd <- as.data.frame(spsample(Wells_points, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
# Add Wells_points's projection information to the empty grid
proj4string(grd) <- proj4string(Wells_points)

#Fit variogram and display plot to inspect goodness of fit
v.fit2<-fit.variogram(variogram(WL_pts_int$`GW ELEV`~1, WL_pts_int), vgm(psill = 4000, "Exp", range = 7000, nugget = 0), 
                      fit.sills = TRUE, fit.ranges = FALSE, fit.method=7)
plot(variogram(WL_pts_int$`GW ELEV`~1, WL_pts_int), model = v.fit2)

# ordinary kriging
x <- krige(WL_pts_int$`GW ELEV`~1, locations = WL_pts_int, newdata = grd, model = v.fit2)
spplot(x["var1.pred"], main = "ordinary kriging predictions")

# display kriging output
krig1<-raster(x) 
plot(krig1)
contour(krig1, levels=as.numeric(seq(6000,6070,5)), add=TRUE)
points(WL_pts_int)

# create well_screened intervals table
lith<-read_csv("Lithology.csv")
lith$'Hole ID' <- as.factor(lith$'Hole ID')
screens<-lith %>%
  group_by(`Hole ID`) %>%
  summarize(screen.top = max(To)-10, screen.bottom = max(To))
###silliness from PDF well table 1
n<-paste0("MW-",seq(1:15))
depth<-c(63.71, 62.11, 53.43, 66.09, 48.63, 86.18, 64.87, 69.49, 29.85, 61.15, 68.59, 79.62, 79.48, 58.15, 63.11)
new<-data.frame(n,depth-10,depth)
colnames(new)<-colnames(screens)
screens<-rbind(screens[!(screens$`Hole ID` %in% new$`Hole ID`),],new)
screens<-merge(screens, water_levels[,c("LOCATION","TOC ELEV","GW ELEV")], by.x= 1,by.y=1, all.y=TRUE)
screens<-screens[!is.na(screens$`GW ELEV`),]
screens[is.na(screens$screen.bottom),]$screen.top<-34
screens[is.na(screens$screen.bottom),]$screen.bottom<-44
screens$depthGW<-screens$`TOC ELEV`-screens$`GW ELEV`
#if the water is below the bottom of the assumed screen interval, adjust the screened interval downward
screens[screens$screen.bottom<screens$depthGW,]$screen.top<-screens[screens$screen.bottom<screens$depthGW,]$depthGW-8
screens[screens$screen.bottom<screens$depthGW,]$screen.bottom<-screens[screens$screen.bottom<screens$depthGW,]$depthGW+2
screens$flatID<-"Well"
write_csv(screens, "well_screens.csv")


# contour at specified interval and write to line shapefile
cl<-rasterToContour(krig1, levels=as.numeric(seq(6000,6070,5))) # 5 ft contour interval
cl$level<-as.integer(levels(cl$level)) # required to make the 'level' filed numeric when the shapefile is exported
writeOGR(cl, dsn = getwd(), layer = "April2020_PotsurfaceContours", driver = "ESRI Shapefile", overwrite_layer=TRUE)
# ##Use to write directly to M drive
# writeOGR(cl, dsn="M:/ItoN/MidnightSunTrinityJV/Modeling/Shapefiles", layer = "April2020_PotsurfaceContours", driver = "ESRI Shapefile", overwrite_layer=TRUE)
# writeOGR(cl, dsn="M:/ItoN/MidnightSunTrinityJV/Modeling/Shapefiles", layer = "April2020_PotsurfaceContours", driver="MapInfo File",
#         dataset_options="FORMAT=MIF")

