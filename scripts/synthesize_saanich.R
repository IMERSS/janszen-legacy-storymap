library(sf)
library(dplyr)
library(leaflet)
library(stringi)
library(stringr)
source("scripts/utils.R")
source("scripts/synthesize_iNat_GBIF.R")

acceptedRanks <- c("genus", "subgenus", "species", "subspecies", "hybrid", "complex", "variety")

inputNames <- list(polygon ="tabular_data/Saanich/Saanich.geojson",
                   iNatObs = "tabular_data/Catalogues/Saanich_iNaturalist_Tracheophyta_Catalogue_2025_05_03.csv",
                   gbifObs = "tabular_data/Catalogues/gbif-saanich-2025-05-03-0012989-250426092105405.csv")

outputNames <- list(iNatObs.filtered = "tabular_data/Processed_Catalogues/Saanich_Polygon_iNaturalist_Tracheophyta_Catalogue_2025_05_03.csv",
                    gbifObs.iNat.not.iNat = "tabular_data/Processed_Catalogues/Saanich_GBIF_iNat_Not_iNat_2025_05_03.csv",
                    gbifObs.not.iNat = "tabular_data/Processed_Catalogues/Saanich_GBIF_Not_iNat_2025_05_03.csv",
                    combined = "tabular_data/Processed_Catalogues/Saanich_GBIF_And_iNat_2025_05_03.csv")
                    

                     
                     
                     
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

