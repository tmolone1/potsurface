#create shapefile
locs_sp<-SpatialPointsDataFrame(data.frame(locs$long, locs$lat), 
                                proj4string=CRS("+init=EPSG:4326"),
                                data=locs)
locs_sf<-st_as_sf(locs_sp)
locs_sf<-locs_sf %>% dplyr::filter(Location %in% tbl$well_name)
# 
st_write(locs_sf, dsn='./outputs', layer="well_locs", driver="ESRI Shapefile", append = FALSE)
