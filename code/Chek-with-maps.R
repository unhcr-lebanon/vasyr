## A few maps to check data



#plot(household$long, household$lat)
#plot(household$lat, household$long)
#plot(household$lat)

xmax <- max(household$lat)
xmin <- min(household$lat)
ymax <- max(household$long)
ymin <- min(household$long)
xlat <- (xmax+xmin)/2
ylong <- (ymax+ymin)/2
bounding = c(ymin-0.2, xmin-0.2, ymax+0.2, xmax+0.2)

## Stammen background
map.toner.lite <- get_stamenmap(bounding, zoom = 9, maptype = "toner-lite")
ggmap(map.toner.lite)
map.toner.lines <- get_stamenmap(bounding, zoom = 9, maptype = "toner-lines")
ggmap(map.toner.lines)
map.toner.background <- get_stamenmap(bounding, zoom = 9, maptype = "toner-background")
ggmap(map.toner.background)
map.toner.hybrid <- get_stamenmap(bounding, zoom = 9, maptype = "toner-hybrid")
ggmap(map.toner.hybrid)
map.toner.labels <- get_stamenmap(bounding, zoom = 9, maptype = "toner-labels")
ggmap(map.toner.labels)
map.terrain.background <- get_stamenmap(bounding, zoom = 9, maptype = "terrain-background")
ggmap(map.terrain.background)
map.terrain <- get_stamenmap(bounding, zoom = 9, maptype = "terrain")
ggmap(map.terrain)
map.terrain.lines <- get_stamenmap(bounding, zoom = 9, maptype = "terrain-lines")
ggmap(map.terrain.lines)

## Googlemap background
cat("Getting a black & white background map from Google \n")
gmap.google <- get_map(location = c(lon = ylong, lat = xlat), zoom=8, color = "bw", source = "google",maptype = "road")
map.google  <- ggmap( gmap.google )
print(map.google )


data.sp <- household
map <- ggplot( data.sp, aes(long, lat)) +
  geom_point( aes(long, lat)) +
  coord_equal()
print(map)

map.point <- ggmap(map.toner.lite) +
  geom_point(aes(x=long, y=lat), data=data.sp, col="orange", alpha=0.2, size=3) +
  theme( legend.title = element_text(size = 8),
         legend.text.align = 0,
         legend.background = element_rect(fill = alpha('white', 0.0)),
         legend.position="top",
         legend.direction="horizontal") +
  labs(title = "Map", subtitle = "", caption = "UNHCR", x = NULL, y = NULL)
ggsave(map.point, filename="out/mappoint.png", width=10, height=8,units="in", dpi=300)


map.density <- ggmap(map.toner.lite) +
  stat_density2d(data = data.sp,
                 aes(x = long, y = lat, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
  theme( legend.title = element_text(size = 8),
         legend.text.align = 0,
         legend.background = element_rect(fill = alpha('white', 0.0)),
         legend.position="top",
         legend.direction="horizontal") +
  labs(title = "Map", subtitle = "", caption = "UNHCR", x = NULL, y = NULL)
ggsave(map.density, filename="out/mapdensity.png", width=10, height=8,units="in", dpi=300)


map.bin <- ggmap(map.toner.lite) +
  stat_bin2d(aes(x = long, y = lat, colour = section3_household.housing.type_of_housing, fill = section3_household.housing.type_of_housing),
             data = data.sp, size = 0.01, bins = 55, alpha = 0.3 ) +
  theme( legend.title = element_text(size = 8),
         legend.text.align = 0,
         legend.background = element_rect(fill = alpha('white', 0.0)),
         legend.position="top",
         legend.direction="horizontal") +
  labs(title = "Map", subtitle = "", caption = "UNHCR", x = NULL, y = NULL)
map.bin
ggsave(map.bin, filename="out/mapbin.png", width=10, height=8,units="in", dpi=300)

map.hex <- ggmap(map.toner.lite) +
  stat_summary_hex(aes(x= long, y= lat, z = data.sp$section2.total_hh),data=data.sp, bins = 100, alpha = 0.8, fun = function(x) {tab <- table(x) names(tab)[which.max(tab)] })+
  theme( legend.title = element_text(size = 8),
         legend.text.align = 0,
         legend.background = element_rect(fill = alpha('white', 0.0)),
         legend.position="top",
         legend.direction="horizontal") +
  labs(title = "Map", subtitle = "", caption = "UNHCR", x = NULL, y = NULL)
map.hex
ggsave(map.hex, filename="out/maphex.png", width=10, height=8,units="in", dpi=300)
