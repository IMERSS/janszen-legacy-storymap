library(sf)
library(leaflet)
library(raster)

source("scripts/utils.R")

# Layer 1: Salish Sea DSM
SS_DSM <- raster("spatial_data/rasters/SS_DEM_400x400.tif")

# Convert floating point to integers
dataType(SS_DSM)="INT4S"

SS_DSM <- round(SS_DSM)

# Layer 2: Salish Sea Region
boundary <- mx_read("spatial_data/vectors/boundary")

# Layer 3: Salish Sea Islands and Mainland
islands <- mx_read("spatial_data/vectors/islands")

# Layer 4: Outer Gulf Islands
OGI <- mx_read("spatial_data/vectors/lists/Outer_Gulf_Islands")

# Layer 5: Saanich
Saanich <- mx_read("spatial_data/vectors/lists/Saanich")

# Layer 6: San Juan Islands
San.Juans <- mx_read("spatial_data/vectors/lists/San_Juan_Islands")

# Layer 7: Saltspring Island
Saltspring <- mx_read("spatial_data/vectors/lists/Saltspring_Island")

# Create raster palette

pal <- colorNumeric(c("#08306b", "#f7fbff"), values(SS_DSM),
                    na.color = "transparent")

# Define map bounds based on extent of combined SHP files

project.area <- dplyr::bind_rows(list(OGI, Saanich, San.Juans, Saltspring))

bbox <- st_bbox(project.area) %>% as.vector()

# Render leaflet map

Map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addRasterImage(SS_DSM, colors =pal, opacity = 0.8) %>%
  addPolygons(data = boundary, color = "blue", weight = 2, fillOpacity = 0) %>%
  addPolygons(data = islands, color = "blue", weight = 1, fillOpacity = 0) %>%
  addPolygons(data = OGI, fillColor = "#8d5a99", weight = 1, fillOpacity = 90) %>%
  addPolygons(data = Saanich, fillColor = "#f3a6b2", weight = 1, fillOpacity = 90) %>%
  addPolygons(data = Saltspring, fillColor = "#d5b43c", weight = 1, fillOpacity = 90) %>%
  addPolygons(data = San.Juans, fillColor = "#729b6f", weight = 1, fillOpacity = 90) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

print(Map)