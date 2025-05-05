library(sf)
library(dplyr)
library(leaflet)
library(stringi)
library(stringr)
source("scripts/utils.R")
source("scripts/synthesize_iNat_GBIF.R")

acceptedRanks <- c("genus", "subgenus", "species", "subspecies", "hybrid", "complex", "variety")

inputNames <- list(polygon ="tabular_data/Outer_Gulf_Islands/Outer_Gulf_Islands.geojson",
                   iNatObs = "tabular_data/Catalogues/Outer_Gulf_Islands_iNaturalist_Tracheophyta_Catalogue_2025_05_04.csv",
                   gbifObs = "tabular_data/Catalogues/gbif-outer-gulf-2025-05-04-0014671-250426092105405.csv")

outputNames <- list(iNatObs.filtered = "tabular_data/Processed_Catalogues/Outer_Gulf_Islands_Polygon_iNaturalist_Tracheophyta_Catalogue_2025_05_04.csv",
                    gbifObs.iNat.not.iNat = "tabular_data/Processed_Catalogues/Outer_Gulf_Islands_GBIF_iNat_Not_iNat_2025_05_04.csv",
                    gbifObs.not.iNat = "tabular_data/Processed_Catalogues/Outer_Gulf_Islands_GBIF_Not_iNat_2025_05_04.csv",
                    combined = "tabular_data/Processed_Catalogues/Outer_Gulf_Islands_GBIF_And_iNat_2025_05_04.csv")
                    

                     
                     
                     
synthesize_iNat_GBIF(inputNames, outputNames)


# 
# # Select the first 1,000 observations
# selected_observations <- filtered_observations %>% slice_head(n = 1000)
# 
# # Extract coordinates for plotting
# coords <- st_coordinates(selected_observations)
# 
# leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data = selected_observations, 
#                    radius = 3, 
#                    color = "blue", 
#                    fillOpacity = 0.7) %>%
#   addPolygons(data = Saanich.polygon, 
#               color = "red", 
#               weight = 1, 
#               fillOpacity = 0.2)
# 

