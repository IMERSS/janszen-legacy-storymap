library(dplyr)
library(leaflet)
library(raster)
library(sf)
library(tidyverse)

source("scripts/utils.R")

# Layer 2: Salish Sea Boundary
boundary <- mx_read("spatial_data/vectors/Salish_Sea")

# Layer 3: Overview Localities (polygons)
polygons.overview <- mx_read("spatial_data/vectors/overview_polygons")

# Layer 4: Overview (points)
points.overview <- mx_read("spatial_data/vectors/overview_points")

points.coordinates <- data.frame(st_coordinates(points.overview))

points.overview$X <- points.coordinates$X
points.overview$Y <- points.coordinates$Y

# Define map bounds based on extent of combined SHP files (all shapes represented in project)

bbox1 <- st_as_sfc(st_bbox(polygons.overview))
bbox2 <- st_as_sfc(st_bbox(points.overview))

bbox3 <-  c(bbox1, bbox2)

bbox <- unname(st_bbox(bbox3))

# Render leaflet map

Map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addPolygons(data = polygons.overview, color = "#d5b43c", weight = 1, fillOpacity = 0,
              label = paste(polygons.overview$Locality, polygons.overview$Province, polygons.overview$Country, sep = ", ")) %>%
  addCircleMarkers(data = points.overview, ~X, ~Y, label = paste(points.overview$Locality,
              points.overview$Province, points.overview$Country, sep = ", "),
              fillColor = "#d5b43c",
              fillOpacity = 1,
              stroke = F,
              radius = 4)  %>% 
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

print(Map)
