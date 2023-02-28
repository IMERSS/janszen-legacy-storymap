library(sf)
library(leaflet)
library(raster)

source("scripts/utils.R")

# Layer 1: Salish Sea DSM
SS_DSM <- raster("spatial_data/rasters/SS_DEM_400x400.tif")

# Convert floating point to integers
dataType(SS_DSM)="INT4S"

SS_DSM <- round(SS_DSM)

# Layer 2: Boundary
boundary <- mx_read("spatial_data/vectors/boundary")

# Create raster palette

pal <- colorNumeric(c("#08306b", "#f7fbff"), values(SS_DSM),
                    na.color = "transparent")

# Define map bounds based on extent of combined SHP files (all shapes represented in project)

bbox <- st_bbox(boundary) %>% as.vector()

# Render leaflet map

Map <- leaflet() %>%
  addTiles("https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png") %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  
  print(Map)