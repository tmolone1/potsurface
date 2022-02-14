library(ggplot2)



cl<-st_as_sf(cl)

p <- ggplot() +
  geom_point(data = locs_sf, aes(x = long, y = lat), color = "red", fill = "grey") + 
  geom_sf(data = cl) + 
  geom_sf_text(data = cl, aes(label = level), size = 2) +
  geom_sf_text(data = locs_sf, aes(label = well_name), size = 2, nudge_y = 0.008) +
  geom_sf_text(data = locs_sf, aes(label = round(elev,2)), size = 2,nudge_y = 0.004) +
  theme_classic()
 
p
