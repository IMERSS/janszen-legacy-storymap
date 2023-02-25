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

# Layer 3: 1996-2000 Localities (polygons)
polygons.1996_2000 <- mx_read("spatial_data/vectors/1996-2000_polygons")

# Layer 4: 1996-2000 Localities (points)
# points.1996-2000 <- mx_read("spatial_data/vectors/1996-2000_points")

# can't get point data to display?

# Create raster palette

pal <- colorNumeric(c("#08306b", "#f7fbff"), values(SS_DSM),
                    na.color = "transparent")

# Define map bounds based on extent of combined SHP files (all shapes represented in project)

bbox <- st_bbox(polygons.1996_2000) %>% as.vector()

# Render leaflet map

Map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addPolygons(data = polygons.1996_2000, color = "#d5b43c", weight = 1, fillOpacity = 0) %>%
  #addPolygons(data = points.1996_2000, color = "#d5b43c", weight = 1, fillOpacity = 90) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

print(Map)