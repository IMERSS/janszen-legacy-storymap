library(sf)
library(leaflet)
library(raster)

source("scripts/utils.R")

# Layer 1: Boundary
boundary <- mx_read("spatial_data/vectors/Salish_Sea")

# Define map bounds based on extent of combined SHP files (all shapes represented in project)

bbox <- st_bbox(boundary) %>% as.vector()

# Render leaflet map

Map <- leaflet() %>%
  addTiles("https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png") %>%
  addPolygons(data = boundary, color = "#d5b43c", weight = 1, fillOpacity = 0) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  
  print(Map)