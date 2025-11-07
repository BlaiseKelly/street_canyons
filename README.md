# Street Canyon Tool


This tool calculates estimates for street canyons, often used for air
quality dispersion modelling. The user only needs to enter the
coordinates and area and it will fetch Open Street Map linestrings and
LiDAR data and estimate the location of significant features to
determine the potential for a street canyon.

The OSM data is obtained using the [osmdata
package](https://github.com/ropensci/osmdata) The LiDAR data is obtained
using the [gblidar package](https://github.com/h-a-graham/gblidar)

# Street Canyons

The definition of a street canyon is defined in [ADMS documentation
(chapter
2)](https://www.cerc.co.uk/environmental-software/assets/data/doc_techspec/P28_02.pdf)
as:

<center>

User inputs $H_{avg_{i}}$ Average building height  
$H_{max_{i}}$ Maximum building height  
$H_{min_{i}}$ Minimum building height  
$g_{i}$ Width from road centreline to canyon wall  
$b_{i}$ Length of road with adjacent buildings

Street canyon characterisation  
$H = (H_{avg_{L}} + H_{avg_{R}})/2)$ Overall average canyon height (m)  
$H_{min} = min(H_{avg_{L}},H_{avg_{R}})$ Overall minimum canyon height
(m)  
$H_{min} = max(H_{avg_{L}},H_{avg_{R}})$ Overall maximum canyon height
(m)  
$H_{\Delta_{i}} = 2(H_{avg_{i}}-H_{min_{i}})$ Range of bui8lding heights
for each side (m)  
$g = g_{L}+g_{R}$ Total canyon width (m)  
$\alpha_{i} = 1-b_{i}/L_{R}$ Porosity for each side, where $L_{R}$ is
the road length  
$H/g$ Ratio between canyon height and width  
$\phi_{c}$ Angle of the canyon segment relative to north (degrees)
</center>

# Define location

Use the function create_domain to create a domain to import LiDAR and
OSM data

``` r
source("functions.R")
library(sf)
```

    Linking to GEOS 3.13.1, GDAL 3.11.0, PROJ 9.6.0; sf_use_s2() is TRUE

``` r
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
# create domain using default location in the function (St Pauls Catherdral)
domain <- create_domain()
```

# Import LiDAR

Use the package gblidar to get a Digital Surface Model (DSM) raster.
Also fetch a hillshade raster for quickly scoping the height variation
and for plotting.

``` r
#remotes::install_github("https://github.com/h-a-graham/gblidar")
library(gblidar)
# download DSM data for the domain
fz_dsm <- eng_composite(domain, product = "fz_dsm")
# the hillshade data is also useful for plots
dsm_hs <- eng_composite(domain, product = "dsm", product_type = "hillshade")

# have a quick look what has been downloaded
par(mfrow = c(1, 2))
plot(fz_dsm, col = hcl.colors(150, "mako"))
plot(dsm_hs, col = hcl.colors(256, "Blues"), legend = FALSE)
```

# Import OSM data

Use osmdata to import the main carriageway (don’t want pavements or
cycle paths in this particular case as it creates multiple linestrings
for a single road, although this might be useful for some applications).

``` r
library(osmdata)
```

    Data (c) OpenStreetMap contributors, ODbL 1.0. https://www.openstreetmap.org/copyright

``` r
library(osmactive) # use osmactive to cleanly select the main carriageway
#convert domain to a boundary box
bb <- st_bbox(st_transform(domain,4326))

##download road data from OSM
x <- opq(bbox = bb) %>% 
  add_osm_feature(key = c('highway')) %>% osmdata_sf()

# extract the road linestrings and pickout those for driving (i.e. ignoring pavements/cycle paths etc which complicate matters)
roads <- osmactive::get_driving_network(x$osm_lines) |> # select the main carriageway
  st_transform(27700) |> 
  transmute(osm_id, length = st_length(geometry))
```

# Split road links up

The canyon function works best if the road widths are as

``` r
# chop the links up into max length
split_l <- split_links(osm_links = roads,
                       max_link_length = 40)
```

# Calculate road geometry

Width2Scope defines how far away from the centre point it should scan,
default is 20m. StartPoint defines how far away from the centre point it
should start scanning, default is 0.5, often don’t want the centre as it
can contain street furniture such as traffic islands. What intervals
should it scan? Default is 0.5m. dsm is the input Digital Surface Model
(DSM) raster

``` r
# create vgt file, which also details the coordinates for the next step
vgt_file <- create_vgt(road_links = split_l)

# get the dimensions of the road surroundings
all_roads <- get_road_dimensions(vgt = vgt_file,Width2Scope = 20, StartPoint = 0.5, Interval = 0.5, dsm = fz_dsm)
```

# Street Canyon calc

Determine if the section is a street canyon or not

``` r
# returns a list with canyon polygon and left and right hand lines
canyon_linez <- create_canyons(vgt = vgt_file,
                           road_dims = all_roads)

# select the individual outputs for each
canyonz <- canyon_linez[[1]]

lines_L <- canyon_linez[[2]]

lines_R <- canyon_linez[[3]]
```
