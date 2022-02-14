library(sp)
library(gstat)
library(rgdal)
library(raster)
library(fields)
library(stars)
library(sf)
mygrid<-function(x, y, z, buff, coords) {
  xyz <- as.data.frame(cbind(x,y,z), colnames=c("x","y","z"))
  pts <- SpatialPointsDataFrame(xyz[,c("x","y")], 
                                data= xyz,
                                proj4string = coords)
  ext<-bbox(pts)+t(matrix(buff,2,2)*c(-1,1))
  grd <- as.data.frame(spsample(pts, "regular", n=50000, bb=ext))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  # Add Wells_points's projection information to the empty grid
  proj4string(grd)<-proj4string(pts)
  return(grd)
}

myidw<-function(x, y, z, grd, coords) {
  xyz <- as.data.frame(cbind(x,y,z), colnames=c("x","y","z"))
  pts <- SpatialPointsDataFrame(xyz[,c("x","y")], 
                                data= xyz,
                                proj4string = coords)
  sf_pts<-st_as_sf(pts)
  st_crs(sf_pts) = coords_no  
  sf_grd<-st_as_sf(grd)
  st_crs(sf_grd) = coords_no 
  grd_template_raster<-st_rasterize(sf_grd)
  # interpolation, idw
  fit_IDW <- gstat::idw(z~1,pts,grd,idp=6)
  raster_idw<-raster(fit_IDW)
  #interp_IDW <- predict(fit_IDW, grd_template_raster)
  return(raster_idw)
}

mypts<-function(x, y, z, coords, Locids) {
  xyz <- as.data.frame(cbind(x,y,z), colnames=c("x","y","z"))
  dat<- cbind(Locids,xyz)
  pts <- SpatialPointsDataFrame(xyz[,c("x","y")],
                                data= dat,
                                proj4string = coords)
  return(pts)
}

mycontours<-function(r, levs) {
  cl<-rasterToContour(r, levels=levs)
  return(cl)
}

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}


myTPS<-function(x, y, z, grd, coords) {
  xyz <- as.data.frame(cbind(x,y,z), colnames=c("x","y","z"))
  pts <- SpatialPointsDataFrame(xyz[,c("x","y")], 
                                data= xyz,
                                proj4string = coords)
  fit_TPS <- fields::Tps( # using {fields}
    x = as.matrix(xyz[, c("x", "y")]), # accepts points but expects them as matrix
    Y = xyz$z,  # the dependent variable
    miles = FALSE)
  grd_template_raster<-raster(grd)
  interp_TPS <- interpolate(grd_template_raster, fit_TPS)
  return(interp_TPS)
}