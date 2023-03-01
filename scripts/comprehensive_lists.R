library(sf)
library(leaflet)
library(raster)

source("scripts/utils.R")


# Layer 2: Salish Sea Region
boundary <- mx_read("spatial_data/vectors/boundary")

# Layer 3: Salish Sea Islands and Mainland
islands <- mx_read("spatial_data/vectors/islands")

# Layer 4: Outer Gulf Islands
OGI <- mx_read("spatial_data/vectors/lists/Outer_Gulf_Islands")

# Layer 5: Saanich
Saanich <- mx_read("spatial_data/vectors/lists/Saanich")
Saanich$Name <- "Saanich and surrounding islets"

# Note temporary labels added to Saanich polygon (need to fix this one)

# Layer 6: San Juan Islands
San.Juans <- mx_read("spatial_data/vectors/lists/San_Juan_Islands")

# Layer 7: Saltspring Island
Saltspring <- mx_read("spatial_data/vectors/lists/Saltspring_Island")

# Define map bounds based on extent of combined SHP files

project.area <- dplyr::bind_rows(list(OGI, Saanich, San.Juans, Saltspring))

bbox <- st_bbox(project.area) %>% as.vector()

# Render leaflet map
# Note labels / structure screwy for Saanich polygons

Map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addPolygons(data = OGI, fillColor = "#8d5a99", weight = 1, fillOpacity = 90,
              label = OGI$Name) %>%
  addPolygons(data = Saanich, fillColor = "#f3a6b2", weight = 1, fillOpacity = 90,
              label = Saanich$Name) %>%
  addPolygons(data = Saltspring, fillColor = "#d5b43c", weight = 1, fillOpacity = 90,
              label = Saltspring$Name) %>%
  addPolygons(data = San.Juans, fillColor = "#729b6f", weight = 1, fillOpacity = 90,
              label = San.Juans$Name) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

print(Map)