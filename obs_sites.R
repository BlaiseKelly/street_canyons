library(gblidar)
library(sf)
library(raster)
library(terra)
library(mapview)
library(mapedit)
library(osmdata)
library(dplyr)
library(tidyr)
library(birk)
library(tmap)
library(openairmaps)
library(openair)

source("functions.R")

#> Linking to GEOS 3.12.1, GDAL 3.8.3, PROJ 9.3.1; sf_use_s2() is TRUE
if (rlang::is_installed("terra")) {
  library(terra)
  options(gblidar.out_raster_type = "SpatRaster")
}

aurn_urban_traffic_all <- openair::importMeta(source = "aurn", all = TRUE) |> 
  filter(site_type == "Urban Traffic") |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  filter(end_date == "ongoing")

aurnz <- unique(aurn_urban_traffic$code)

aurn_urban_traffic <- openair::importMeta(source = "aurn", all = FALSE) |> 
  filter(site_type == "Urban Traffic") |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  filter(code %in% aurnz)

countries <- geographr::boundaries_countries20 |> 
  filter(country20_name == "England")

aurn_urban_traffic <- aurn_urban_traffic[countries,]



canyon_list <- list()
canyon_shape <- list()
site_roads <- list()
elev_list <- list()
ll = list()
rl = list()
a <- "BRAS"
for (a in aurnz){
  tryCatch({
    
  s <- aurn_urban_traffic[aurn_urban_traffic$code == a,]

# create domain using default location in the function
domain <- st_buffer(s, 50)

# have a look at it
#mapview(domain)+s

# download DSM data for the domain
fz_dsm <- eng_composite(domain, product = "fz_dsm")
# the hillshade data is also useful for plots
#dsm_hs <- eng_composite(domain, product = "dsm", product_type = "hillshade")

rd_domain <- st_buffer(st_transform(s,27700), 20)

#convert domain to a boundary box
bb <- st_bbox(st_transform(rd_domain,4326))

#dat <- osmactive::get_travel_network(boundary = rd_domain)

ok <- FALSE
while (!ok) {
  result <- try(x <- opq(bbox = bb) %>%
                  add_osm_feature(key = c('highway')) %>% osmdata_sf(), silent = TRUE)
  
  if (inherits(result, "try-error")) {
    message("❌ Query failed, retrying in 12seconds...")
    Sys.sleep(2)
  } else {
    ok <- TRUE
    message("✅ Query succeeded!")
  }
}

##download road data from OSM

# extract the road linestrings and pickout those for driving (i.e. ignoring pavements/cycle paths etc which complicate matters)
roads <- osmactive::get_driving_network(x$osm_lines) |> 
  st_transform(27700) |> 
  transmute(osm_id = s$code, length = st_length(geometry)) |> 
  st_intersection(rd_domain)

s_m <- st_transform(s,27700)

if(NROW(roads)>1){

nr <- roads[st_nearest_points(roads,s_m),] 

nr$dist = as.numeric(st_distance(nr,s_m))

nr <- nr |> 
  arrange(dist) |> 
  slice(1)

} else {
  
  nr <- roads
  
}
#mapview(nr)
##SPLIT INTO SMALLER LINKS FOR ADVANCED CANYON
##advanced canyon links

# chop the links up into max length
# split_l <- split_links(osm_links = roads,
#                        max_link_length = 20)

#nr <- st_as_sf(st_line_sample(nr, n = 20))

df2 <- st_line_sample(nr, n = 20)
df_out <- data.frame(st_coordinates(df2))
df_out$osm_id <- s$code
sl <- st_as_sf(df_out, coords = c("X", "Y"), crs = 27700)
sl <- sl %>% group_by(osm_id) %>% dplyr::summarise(do_union = FALSE) %>% st_cast("LINESTRING")
nr <- st_as_sf(sl, crs = 27700)

# create vgt file, which also details the coordinates for the next step
vgt_file <- create_vgt(road_links = nr, link_id = "osm_id")

# get the dimensions of the road surroundings
road_elev <- get_road_dimensions(vgt = vgt_file,Width2Scope = 30, dsm = fz_dsm)

all_roads <- road_elev[[1]]

elevationz <- road_elev[[2]] |> 
  st_as_sf(coords = c("x", "y"), crs = 27700) |> 
  mutate(ID = a)
  #group_by(dist,side) %>% 
  #dplyr::summarise(do_union = FALSE) %>% st_cast("LINESTRING")

# returns a list with canyon polygon and left and right hand lines
canyon_linez <- create_canyons(vgt = vgt_file,
                               road_dims = all_roads)

# select the individual outputs for each
canyonz <- canyon_linez[[1]] |> 
  left_join(all_roads, by = c("main" = "ID"))

lines_L <- canyon_linez[[2]]

lines_R <- canyon_linez[[3]]


# summarising street canyon
canyonz_dat <- canyon_linez[[1]] |> 
  left_join(all_roads, by = c("main" = "ID")) |> 
  mutate(g = width_L+width_R) |> 
  mutate(H = (avgHeight_L+avgHeight_R)/2) |> 
  mutate(sc_ratio = H/g) |> 
  mutate(sc = if_else(sc_ratio >1, "YES", "NO")) |> 
  rowwise() |> 
  mutate(avg_height = sum(avgHeight_L+avgHeight_R)/2) |> 
  mutate(ID = a)


canyon_list[[a]] <- canyonz_dat
elev_list[[a]] <- elevationz
site_roads[[a]] <- roads
ll[[a]] <- canyon_linez[[2]]
rl[[a]] <- canyon_linez[[3]]

print(a)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

all_canyons <- do.call(rbind, canyon_list) 
all_ll <- do.call(rbind, ll)
all_rl <- do.call(rbind,rl)
all_elevs <- do.call(rbind, elev_list)
all_roads <- do.call(rbind, site_roads)



save(all_canyons, all_ll,all_rl, all_elevs, all_roads, file = "canyon_dat.RData")

all_canyons_order <- all_canyons |> select(main, sc_ratio) |> filter(main %in% aurnz) |> st_set_geometry(NULL) |> arrange(desc(abs(sc_ratio)))

top_10 <- all_canyons_order[2:10,]
png(filename = "all_geo.png",
  width = 300, height = 300, units = "mm", res = 300
)
par(mfrow = c(3, 3))
for (t in top_10$main){
  
  can <- all_canyons[all_canyons$main == t,]
  left_line <- all_ll[all_ll$L == t,]
  right_line <- all_rl[all_rl$L == t,]

  s <- aurn_urban_traffic[aurn_urban_traffic$code == t,]
  
  # create domain using default location in the function
  domain <- st_buffer(s, 50)
  
  s_m <- st_transform(s, 27700)
  
  # have a look at it
  #mapview(domain)+s
  
  # download DSM data for the domain
  fz_dsm <- eng_composite(domain, product = "fz_dsm")

  plot(fz_dsm, main = paste0("site: ", t, " ", s$site, ", left height (m) = ", round(can$avgHeight_L,1), "\nright height (m) = ", round(can$avgHeight_R,1), ", canyon ratio = ", round(can$sc_ratio,1)))
  plot(st_geometry(left_line), col = "red", lwd = 5, add = TRUE)
  plot(st_geometry(right_line), col = "blue", lwd = 5, add = TRUE)
  plot(st_geometry(s_m),pch = 19, col = "pink", cex = 2, add = TRUE)

  print(t)
}

# STEP 4: Turn off the graphics device
dev.off()

 

for (t in top_10$main){

aq_df <- openair::importAURN(site = t, year = 2025) 

min_d8 <- min(aq_df$date)
max_d8 <- str_sub(max(aq_df$date),1,-10)

png(filename = paste0("plots/polar_",which(t == top_10$main), ".png"),
    width = 150, height = 130, units = "mm", res = 300
) 

  
  if(sum(aq_df$no2, na.rm = TRUE)>0){
  
  pp <- polarPlot(aq_df, "no2", key.header = "NO2 (ug/m3)", main = paste(aq_df$site[1], "between", min_d8, "and", max_d8))
    
  } else {

    pp <- polarPlot(aq_df, "pm2.5", key.header = "PM25 (ug/m3)", main = paste(aq_df$site[1], "between", min_d8, "and", max_d8))
    
  }

plot(pp)

dev.off()


  ## get data from nearest meteo site for date range
  #noaa <- find_noaa_sites(sites = l, start_date = year(min_d8), end_date = year(max_d8))
  
  #noaa_df <- noaa %>% select(date, ws,wd,air_temp, atmos_pres, RH, ceil_hgt)
  
  print(t)
}

library(magick)
imgs <- image_read(c("plots/polar_1.png","plots/polar_2.png","plots/polar_3.png",
                     "plots/polar_4.png","plots/polar_5.png","plots/polar_6.png",
                     "plots/polar_7.png","plots/polar_8.png","plots/polar_9.png"))
row1 <- image_append(imgs[1:3])  # top row
row2 <- image_append(imgs[4:6])  # middle row
row3 <- image_append(imgs[7:9])  # bottom row

combined <- image_append(c(row1, row2, row3), stack = TRUE)

image_write(combined, path = "plots/all_polar_combined.png",format = "png")
