library(sf)
library(tidyverse)
library(dotenv)
library(nycgeo)
library(patchwork)
library(tidytransit)

load_dot_env()
public_spaces <- read.csv("data/nyc-public-space.csv")
public_spaces_geo <- st_read("data/nyc-public-space.geojson")
nyc_map <- nycgeo::nyc_boundaries("tract")


#transform to same type
st_crs(nyc_map) <- st_crs(2263)
ps_proj <- st_transform(public_spaces_geo, 2263)
nyc_land_boundaries <- nyc_boundaries()
st_crs(nyc_land_boundaries) <- 2263


subway <- read_gtfs("gtfs_subway.zip")

names(subway)
validate_gtfs(subway)

subway$routes |> View()
subway$trips |> View()