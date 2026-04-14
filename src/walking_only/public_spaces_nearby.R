library(sf)
library(tidyverse)
library(nycgeo)
library(patchwork)
library(tidytransit)

options(java.parameters = '-Xmx10G')
library(r5r)

core <- r5r::build_network("data/subway_r5r_stuff/")
public_spaces <- read.csv("data/nyc-public-space.csv")
public_spaces_geo <- st_read("data/nyc-public-space.geojson") %>%
  st_transform(4326)
subway <- tidytransit::read_gtfs("gtfs_subway.zip")
nyc_map <- nycgeo::nyc_boundaries("tract")

st_crs(nyc_map) <- st_crs(2263)
nyc_centroids <- nyc_map %>%
  st_centroid() %>%
  st_transform(4326)

#make destinations (the public spaces) in lon/lat
destinations <- public_spaces_geo %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(public_spaces) %>%
  transmute(
    id = space_id,
    lon = X,
    lat = Y
  )

origin_point

