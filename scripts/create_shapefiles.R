#create shapefile
locs_sp<-SpatialPointsDataFrame(data.frame(rpt$long, rpt$lat), 
                                proj4string=CRS("+init=EPSG:4326"),
                                data=rpt)
locs_sf<-st_as_sf(locs_sp)
# 
st_write(locs_sf, dsn='./outputs', layer="well_locs", driver="ESRI Shapefile", append = FALSE)
