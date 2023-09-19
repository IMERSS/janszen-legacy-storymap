library(sf)
library(leaflet)
library(raster)

source("scripts/utils.R")

nudgePoint <- function (frame, slugValue, latAmount=0, longAmount=0) {
    filtered <- frame[frame$Slug == slugValue, ];
    coords <- st_coordinates(filtered$geometry);
    coords[, "X"] <- coords[, "X"] + longAmount
    coords[, "Y"] <- coords[, "Y"] + latAmount
    frame[frame$Slug==slugValue, ]$geometry <- st_sfc(st_point(coords))
    frame
}

# Layer 1: Boundary
boundary <- mx_read("spatial_data/vectors/Salish_Sea")

# Define map bounds based on extent of combined SHP files (all shapes represented in project)

#bbox <- st_bbox(boundary) %>% as.vector()

allTerritories <- fetch_first_nations_territories()

# Not yet found: pune’luxutth’, leey’qsun, tla'amin
# some invalid Unicode character in wsanec slug
toSelect <- c("kwantlen", "katzie", "tulalip", "samish", "scianew", "klallam", "cayuse-umatilla-and-walla-walla", 
              "nlakapamux", "stolo-treaty-association", "halalt", "semiahmoo", "snaw-naw-as", "kwakwakawakw", "lig%ca%b7ildax%ca%b7", 
              "we-wai-kai", "we-wai-kum", "komoks", "homalco", "sliammon", "shishalh", "skwxwu7mesh-uxwumixw","x%ca%b7m%c9%99%ce%b8k%ca%b7%c9%99y%cc%93%c9%99m", 
              "quwutsun", "snuneymuxw", "stzuminus", "tsawwassen-sc%cc%93%c9%99wa%ce%b8en", "lekwungen-songhees", "lummi", "w%cc%b1sanec")

territories <- allTerritories[allTerritories$Slug %in% toSelect, ]
# https://github.com/r-spatial/sf/issues/406
st_agr(territories) = "constant"
# https://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes
centres <- sf::st_centroid(territories)

# Nudge some labels so they do not overlap
centres <- nudgePoint(centres, "lummi", -0.08)
centres <- nudgePoint(centres, "stzuminus", -0.05)
centres <- nudgePoint(centres, "lig%ca%b7ildax%ca%b7", -0.1)
centres <- nudgePoint(centres, "scianew", -0.15)

centres <- nudgePoint(centres, "samish", -0.06)
centres <- nudgePoint(centres, "komoks", -0.1)
centres <- nudgePoint(centres, "katzie", 0.1)
centres <- nudgePoint(centres, "semiahmoo", -0.05)

centres <- nudgePoint(centres, "snaw-naw-as", -0.02)
centres <- nudgePoint(centres, "snuneymuxw", 0.09)
centres <- nudgePoint(centres, "stolo-treaty-association", -0.2, 1)

centres <- nudgePoint(centres, "shishalh", -0.17)
centres <- nudgePoint(centres, "skwxwu7mesh-uxwumixw", -0.1)

centres <- nudgePoint(centres, "quwutsun", 0, -0.7)
centres <- nudgePoint(centres, "kwantlen", 0.7)
centres <- nudgePoint(centres, "lekwungen-songhees", -0.03)

centres <- nudgePoint(centres, "x%ca%b7m%c9%99%ce%b8k%ca%b7%c9%99y%cc%93%c9%99m", 0.07) # Musqueam

bbox <- st_bbox(boundary) %>% as.vector()

# Render leaflet map

Map <- leaflet(options=list(mx_mapId="Acknowledgement")) %>%
  addTiles("https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png") %>%
  addPolygons(data = boundary, color = "#d5b43c", weight = 1, fillOpacity = 0) %>%
  addPolygons(data = territories, weight = 1, color = territories$color, label=territories$Name, fillOpacity = 0.6) %>%
# https://stackoverflow.com/questions/48147282/how-to-add-labels-on-top-of-polygons-in-leaflet
  addLabelOnlyMarkers(data = centres,
                      label = ~Name,
                      labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE,
                                                  style=list("color"="white", "font-size"="15px"))) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  
print(Map)

