library(sp)
library(gstat)
library(rgdal)
library(raster)
library(fields)
library(stars)
library(sf)
mygrid<-function(pts_df) {
  grd <- as.data.frame(spsample(pts_df, "regular", n=50000, 
	    bb=(bbox(pts_df)+t(matrix(mean(c(diff(range(pts_df@coords[,1])),diff(range(pts_df@coords[,2]))))*.1,2,2)*c(-1,1)))))  # buffer the extent of the data by 10% of the range
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  # Add Wells_points's projection information to the empty grid
  crs(grd)<-crs(pts_df)
  #grd<-spTransform(grd,crs(pts_df))
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

mycontours<-function(r) {
  cl<-rasterToContour(r, levels=
                        seq(signif(r@data@min,2),signif(r@data@max,2),(diff(c(signif(r@data@min,2),signif(r@data@max,2)))/20)))
  return(cl)
}

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}


myTPS<-function(pts_df, grd) {
  fit_TPS <- fields::Tps( # using {fields}
    x = as.matrix(pts_df@coords), # accepts points but expects them as matrix
    Y = pts_df$ps_elev,  # the dependent variable
    miles = FALSE)
  interp_TPS <- interpolate(raster(grd), fit_TPS)
  return(interp_TPS)
}