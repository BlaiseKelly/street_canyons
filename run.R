library(gblidar)
library(sf)
library(raster)
library(terra)
library(mapview)
library(osmdata)
library(dplyr)
library(tidyr)
library(birk)
library(leaflet)
library(tmap)

source("functions.R")

#> Linking to GEOS 3.12.1, GDAL 3.8.3, PROJ 9.3.1; sf_use_s2() is TRUE
if (rlang::is_installed("terra")) {
  library(terra)
  options(gblidar.out_raster_type = "SpatRaster")
}

options(gblidar.progress = TRUE) # for readability in this example.

latitude <- 52.36
longitude <- 4.87

##convert to sf point
location <- st_point(c(longitude, latitude))
location <- st_sfc(location, crs = latlong)


# # ##Plot the point sources around site to get information, once drawn click FINISH this creates a "simple features" frame (sf)
# modelled_area <- mapview(location, map.types = c("OpenStreetMap", "Esri.WorldTopoMap", "Esri.WorldImagery", "Esri.WorldShadedRelief")) %>%
#   editMap(title = "Use the rectangle tool to draw the area to be modelled")
# 
# ##Once finished create variable to be plotted
# modelled_area <- modelled_area$finished
# mod_area_rdnew <- st_transform(modelled_area, 27700)

search_box <- st_point(c(532054, 181145)) |>
  st_buffer(500) |>
  st_sfc() |>
  st_set_crs(27700)


# create folder to save
dir.create("data")

# the elevation data for the first return DSM
fz_dsm <- eng_composite(search_box, product = "fz_dsm")
# the hillshade data for the last return DSM
dsm_hs <- eng_composite(search_box, product = "dsm", product_type = "hillshade")

# have a quick look what has been downloaded
plot(fz_dsm, col = hcl.colors(150, "mako"))
plot(dsm_hs, col = hcl.colors(256, "Blues"), legend = FALSE)

#convert to a boundary box
bb <- st_bbox(st_transform(search_box,4326))

##download building data from OSM
x <- opq(bbox = bb) %>% 
  add_osm_feature(key = c('highway')) %>% osmdata_sf()

# extract the road linestrings and pickout those for driving (i.e. ignoring pavements/cycle paths etc which complicate matters)
roads <- osmactive::get_driving_network(x$osm_lines) |> 
  st_transform(27700) |> 
  transmute(osm_id, length = st_length(geometry))

##SPLIT INTO SMALLER LINKS FOR ADVANCED CANYON
##advanced canyon links

# chop the links up into max length
split_l <- split_links(osm_links = roads,
                       max_link_length = 40)

# create vgt file, which also details the coordinates for the next step
vgt_file <- create_vgt(road_links = split_l)

# get the dimensions of the road surroundings
all_roads <- get_road_dimensions(vgt = vgt_file, dsm = fz_dsm)

# returns a list with canyon polygon and left and right hand lines
canyon_linez <- create_canyons(vgt = vgt_file,
                           road_dims = all_roads)

# select the individual outputs for each
canyonz <- canyon_linez[[1]]

line_L <- canyon_linez[[2]]

line_R <- canyon_linez[[3]]

# can combine to get an idea of the road extents
canyon_union <- st_union(canyonz)
#mapview(canyon_union)

roads_latlon <- st_transform(roads, 4326)

vgt_dat <- vgt_can %>% 
  distinct(Source.name, .keep_all = TRUE) %>% 
  select(-X..m., -Y..m., -Side_R, -Side_L)

canyonz_dat <- left_join(canyonz, vgt_file, by = c("main" = "Source.name")) 
Left_lines <- Left_lines %>% 
  left_join(vgt_dat, by = c("L" = "Source.name")) %>% 
  st_transform(4326)
Right_lines <- Right_lines %>% 
  left_join(vgt_dat, by = c("L" = "Source.name")) %>% 
  st_transform(4326)

# determine if the road is classed as a street canyon - i.e. ration 
scs <- vgt_dat |> 
  mutate(L_sc = avgHeight_L/width_L,
         R_sc = avgHeight_R/width_R) |> 
  mutate(sc = (L_sc+R_sc)/2)

canyon_sc <- canyonz |> 
  left_join(scs, by = c("main" = "Source.name")) |> 
  filter(sc >= 1)

canyon_split <- st_as_sf(st_split(canyon_union,roads))

mapview(canyon_split)





canyonz_dat<- mutate(canyonz_dat, avgHeight = (avgHeight_L + avgHeight_R)/2)
st_geometry(canyonz_dat) <- canyonz_dat$geometry
canyonz_dat <- st_transform(canyonz_dat, latlong)
#W_canyons <- filter(canyonz, grepl("W", main))
#E_canyons <- filter(canyonz, grepl("E", main))

save(canyonz_dat, Left_lines, Right_lines, roads, modelled_area, file = "canyons.RData")

mapview(Right_lines, color = "red")+mapview(Left_lines, color = "yellow")+canyonz

mapview(canyonz)+buildings

library(leaflet)
library(htmlwidgets)

lon <- st_coordinates(st_centroid(modelled_area))[1]
lat <- st_coordinates(st_centroid(modelled_area))[2]

height_bins <- seq(from = 0, to = 70, by = 4)

pal_R <- colorNumeric("viridis", height_bins,
                      na.color = "transparent")

# pal_20 <- colorNumeric("viridis", values(pal_20),
#                        na.color = "transparent")

##add pop ups
# pcode_out <- mutate(pcode_out, cntnt=paste0('<strong>Postcode 4 digit: </strong>',PC4,
#                                             '<br><strong>Factor 1 (ug/m3): </strong> ', f1,
#                                             '<br><strong>Factor 5 (ug/m3): </strong> ', f5,
#                                             '<br><strong>Factor 20 (ug/m3): </strong> ', f20))

canyonz_dat <- canyonz |> 
  left_join(all_roads, by = c("main" = "ID")) |> 
  rowwise() |> 
  mutate(avg_height = sum(avgHeight_L+avgHeight_R)/2)

canyonz_diff <- st_difference(canyonz)
mapview(canyonz_diff)+roads

hgt_pal <- cols4all::c4a("hcl.grays")

canyon_pal <- cols4all::c4a("kovesi.rainbow_bgyr_35_85_c73")

canyonz_plot <- select(canyonz_dat, `average height\nsurroundings (m)` = avg_height, geometry)

dir.create("plots/")

tm1 <- tm_shape(dsm_hs) +
  tm_raster(col.scale = tm_scale_intervals(values = hgt_pal), col.legend = tm_legend_hide())+
  tm_shape(canyonz_dat)+
  tm_polygons(fill = "avg_height", fill.scale = tm_scale_intervals(values = canyon_pal),
              fill.legend = tm_legend(title = "average height\nsurroundings (m)",frame = FALSE))

tmap_save(tm1, "plots/canyons.png", height = 10, width = 11.5)

load("london.RData")

m <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB")

m <- m %>% addPolygons(data = modelled_area, color = "black", fillColor = "yellow", group = "selected area")

m <- m %>% addPolylines(data = roads, color = "black", weight = 2, fillOpacity = 0.8, group = "roads")

m <- m %>% addPolygons(data = canyonz_dat, fillColor = ~pal_R(avg_height), color = "black", weight = 1, fillOpacity = 0.8, group = "avg h both")

m <- m %>% addPolylines(data = Left_lines, color = ~pal_R(minHeight_L), weight = 2, fillOpacity = 0.8, group = "left min height")

m <- m %>% addPolylines(data = Left_lines, color = ~pal_R(maxHeight_L), weight = 2, fillOpacity = 0.8, group = "left max height")

m <- m %>% addPolylines(data = Right_lines, color = ~pal_R(minHeight_R), weight = 2, fillOpacity = 0.8, group = "right min height")

m <- m %>% addPolylines(data = Right_lines, color = ~pal_R(maxHeight_R), weight = 2, fillOpacity = 0.8, group = "right max height")

m <- m %>% addLegend("bottomleft", pal=pal_R, values=height_bins, opacity=1, title = "height (m)")

m <- m %>% addLayersControl(overlayGroups = c("selected area"),  baseGroups = c("avg h both", "left min height", "left max height", 
                                                                                "right min height", "right max height", "roads"),
                            options = layersControlOptions(collapsed = FALSE), position = "topright") %>%  hideGroup(c("selected area"))

library(htmlwidgets)

m
dir.create("maps/")
withr::with_dir('./', saveWidget(m, file="maps/canyons.html"))
