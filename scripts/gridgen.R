library(sp)
library(gstat)
mygrid<-function(x, y, z, buff, coords) {
  xyz <- as.data.frame(cbind(x,y,z), colnames=c("x","y","z"))
  pts <- SpatialPointsDataFrame(xyz[,c("x","y")], 
                                data= xyz,
                                proj4string = coords)
  ext<-bbox(pts)+t(matrix(buff,2,2)*c(-1,1))
  grd <- as.data.frame(spsample(pts, "regular", n=50000, bb=ext))
  names(grd)       <- c('lon', 'lat')
  coordinates(grd) <- c('lon', 'lat')
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  # Add Wells_points's projection information to the empty grid
  proj4string(grd) <- proj4string(pts)
  #plot(grd)
  #points(pts)
  return(grd)
}

locs_sp<-as_Spatial(locs_sf)
x<-locs_sp$long
y<-locs_sp$lat
z<-locs_sp$level
buff<-.05
coords<-CRS("+init=EPSG:4326")
grid<-mygrid(x, y, z, buff, coords)


grd_sf  <-  st_as_sf(grid, coords = c("lon", "lat"), 
                     crs = coords, agr = "constant")

grd_sp <- as_Spatial(grd_sf)
rm(grid)
rm(grd_sf)
grd_sp<-spTransform(grd_sp,crs(locs_sp))
