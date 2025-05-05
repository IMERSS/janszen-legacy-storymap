library(sf)
library(dplyr)
library(leaflet)
library(stringi)
library(stringr)
source("scripts/utils.R")

library(sf)
library(dplyr)
library(leaflet)
library(stringi)
library(stringr)
source("scripts/utils.R")

acceptedRanks <- c("genus", "subgenus", "species", "subspecies", "hybrid", "complex", "variety")

inputNames <- list(polygon ="tabular_data/Saanich/Saanich.geojson",
                   iNatObs = "tabular_data/Catalogues/Saanich_iNaturalist_Tracheophyta_Catalogue_2025_05_03.csv",
                   gbifObs = "tabular_data/Catalogues/gbif-saanich-2025-05-03-0012989-250426092105405.csv")

outputNames <- list(iNatObs.filtered = "tabular_data/Processed_Catalogues/Saanich_Polygon_iNaturalist_Tracheophyta_Catalogue_2025_05_03.csv",
                    gbifObs.iNat.not.iNat = "tabular_data/Processed_Catalogues/Saanich_GBIF_iNat_Not_iNat_2025_05_03.csv",
                    gbifObs.not.iNat = "tabular_data/Processed_Catalogues/Saanich_GBIF_Not_iNat_2025_05_03.csv",
                    combined = "tabular_data/Processed_Catalogues/Saanich_GBIF_And_iNat_2025_05_03.csv")

# Function to split scientificName into taxonName and scientificNameAuthorship
split_scientific_name <- function(name) {
  words <- str_split(name, " ", simplify = TRUE)  # Split into words
  
  if (length(words) < 2) return(c(name, NA))  # If there's only one word, return it as taxonName
  
  # Find the first authority-forming word (ignoring the first word)
  auth_idx <- which(str_detect(words[-1], "^[^a-z]") | words[-1] == "de")
  
  if (length(auth_idx) > 0) {
    auth_idx <- auth_idx[1] + 1  # Adjust index because we ignored the first word
    taxon_name <- paste(words[1:(auth_idx - 1)], collapse = " ")  # Get the taxon name
    authorship <- paste(words[auth_idx:length(words)], collapse = " ")  # Get the authority
  } else {
    taxon_name <- name  # If no authority, taxonName is the full name
    authorship <- NA  # No authorship found
  }
  
  return(c(taxon_name, authorship))  # Return a character vector
}

synthesize_iNat_GBIF <- function (inputNames, outputNames) {
  # Filter the raw iNat data for the project polygon since iNat can only be queried by bounding box
  polygon <- st_read(inputNames$polygon)
  
  bounding.iNat <- timedRead(inputNames$iNatObs) %>% mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
  
  # Convert bounding.iNat to an SF dataframe
  bounding.iNat.sf <- st_as_sf(bounding.iNat, coords = c("longitude", "latitude"), crs = 4326)
  
  # Filter observations by the polygon
  filtered.iNat.sf <- st_intersection(bounding.iNat.sf, Saanich.polygon)
  filtered.iNat <- filtered.iNat.sf %>%
    mutate(longitude = st_coordinates(.)[,1], latitude = st_coordinates(.)[,2]) %>% st_drop_geometry()
  
  timedWrite(filtered.iNat, outputNames$iNatObs.filtered)
  
  # Analyse which GBIF records with iNat provenance are not found in iNat itself
  
  gbif <- timedFread(inputNames$gbifObs)
  
  # Solution from https://stackoverflow.com/a/29529342 for https://stackoverflow.com/questions/29529021/replace-a-data-frame-column-based-on-regex
  extractINatId <- function (iNatRows) {
    ids <- stri_match_first_regex(iNatRows$occurrenceID, "^.*/(.+)$")[,2]
    imerssids <- stri_match_first_regex(iNatRows$occurrenceID, "^imerss.org:iNat:(.+)$")[,2]
    iNatRows$iNatObsID <- ifelse(is.na(ids), imerssids, ids)
    iNatRows
  }
  
  gbif.iNatRows <- gbif[institutionCode=="iNaturalist"]
  
  gbif.iNatRows <- extractINatId(gbif.iNatRows)
  gbif.iNatRows <- gbif.iNatRows[!duplicated(gbif.iNatRows$iNatObsID), ]
  
  # iNat Rows in GBIF not in iNat itself
  gbif.iNat.not.iNat <- dplyr::anti_join(gbif.iNatRows, filtered.iNat, by=c("iNatObsID"="id"))
  
  # This last contains iNat records that got into GBIF that are not in any iNat project -
  # This will contain records whose georeferencing precision lies outside the bounds of the island, that in general we will want
  timedWrite(gbif.iNat.not.iNat, outputNames$gbifObs.iNat.not.iNat)
  
  gbif.not.iNat <- gbif[institutionCode!="iNaturalist"]
  timedWrite(gbif.not.iNat, outputNames$gbifObs.not.iNat)
  
  # The GBIF records we will use are sum of i) those with no iNat provenance ii) those with iNat provenance not found in iNat
  used.gbif <- bind_rows(gbif.not.iNat, gbif.iNat.not.iNat)
  
  
  # Unscrew the traditional mangling of GBIF authorities etc.
  
  # Apply function and store results in new columns
  split_results <- t(sapply(used.gbif$scientificName, split_scientific_name))  # Apply function row-wise
  
  # Convert to data frame and bind to original df
  gbifname <- as.data.frame(split_results, stringsAsFactors = FALSE)
  
  gbif.next <- used.gbif %>%
    mutate(scientificName = gbifname$V1, scientificNameAuthority = gbifname$V2) %>%
    mutate(taxonRank = tolower(taxonRank)) %>%
    mutate(decimalLatitude = as.numeric(decimalLatitude), decimalLongitude = as.numeric(decimalLongitude))
  
  
  iNat.next <- filtered.iNat %>% 
    #  mutate(decimalLatitude = round(as.numeric(coalesce(private_latitude, latitude)), 6)) %>%
    #  mutate(decimalLongitude = round(as.numeric(coalesce(private_longitude, longitude)), 6)) %>%
    mutate(recordedBy = coalesce(user_name, user_login)) %>%
    mutate(occurrenceID = paste0("https://www.inaturalist.org/observations/", id)) %>%
    rename(eventDate = time_observed_at,
           scientificName = scientific_name,
           commonName = common_name,
           iNaturalistTaxonId = taxon_id,
           taxonRank = taxon_rank,
           coordinateUncertaintyInMetres = positional_accuracy) %>%
    select(-private_latitude, -private_longitude, -latitude, -longitude, -observed_on,
           -user_name, -user_login, -id, -positioning_method, -positioning_device)
  
  combined <- bind_rows(iNat.next, gbif.next)
  
  accepted <- combined %>% filter(taxonRank %in% acceptedRanks)
  
  timedWrite(accepted, outputNames$combined)
  
}





