library(sf)
library(tidyverse)
library(nycgeo)
library(patchwork)
library(tidytransit)

options(java.parameters = '-Xmx10G')
library(r5r)

public_spaces <- read.csv("data/nyc-public-space.csv")
public_spaces_geo <- st_read("data/nyc-public-space.geojson")
subway <- read_gtfs("gtfs_subway.zip")

#transform to same type
nyc_map <- nycgeo::nyc_boundaries("tract")
st_crs(nyc_map) <- st_crs(2263)
nyc_centroids <- nyc_map %>%
  st_centroid() %>%
  st_transform(4326)
subway_stops <- subway$stops %>%
  st_transform(4326)


core <- r5r::build_network("data/subway_r5r_stuff/")



public_spaces <- read.csv("data/nyc-public-space.csv")
public_spaces_geo <- st_read("data/nyc-public-space.geojson")


public_spaces <- st_read("data/nyc-public-space.geojson") %>%
  st_transform(4326)


# =========================================================
# 4. KEEP SAFE ID + COORDS (IMPORTANT)
# =========================================================
public_spaces <- public_spaces %>%
  mutate(dest_id = space_id)


# r5r NEEDS plain lat/lon data.frame
destinations <- public_spaces %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(public_spaces) %>%
  transmute(
    id = dest_id,
    lon = X,
    lat = Y
  )


# =========================================================
# 5. ORIGIN POINT
# =========================================================
origin <- data.frame(
  id = "origin",
  lon = -73.9855,
  lat = 40.7580
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


# =========================================================
# 6. TRAVEL TIME MATRIX
# =========================================================
tt <- travel_time_matrix(
  core,
  origins = origin,
  destinations = destinations,
  mode = c("WALK", "TRANSIT"),
  departure_datetime = as.POSIXct("2026-03-01 08:00:00"),
  max_trip_duration = 60
)


# =========================================================
# 7. JOIN RESULTS BACK
# =========================================================
public_spaces_result <- public_spaces %>%
  left_join(tt, by = c("dest_id" = "to_id"))


# =========================================================
# 8. CLASSIFY BY TIME BUCKETS
# =========================================================
public_spaces_result <- public_spaces_result %>%
  mutate(
    reachable_10 = travel_time_p50 <= 10,
    reachable_20 = travel_time_p50 <= 20,
    reachable_30 = travel_time_p50 <= 30
  )


# =========================================================
# 9. FILTERED DATASETS
# =========================================================
spaces_10 <- filter(public_spaces_result, reachable_10)
spaces_20 <- filter(public_spaces_result, reachable_20)
spaces_30 <- filter(public_spaces_result, reachable_30)


ggplot() +
  geom_sf(data = nyc_map) + 
  geom_sf(data = public_spaces, alpha = 0.2) +
  geom_sf(data = spaces_30, color = "red", size = 1) +
  geom_sf(data = spaces_20, color = 'orange', size =1) + 
  geom_sf(data = spaces_10, color = 'green', size = 1) + 
  geom_sf(data = origin, color = "blue", size = 3) +
  theme_minimal() +
  labs(title = "NYC Public Space Accessibility (≤ 30 min)")
