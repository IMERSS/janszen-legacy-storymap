library(sf)
library(geojsonsf)
library(jsonlite)

lat_lon <- function (data) {
  return (st_transform(data, "+proj=longlat +datum=WGS84"))
}

# Attach the region's label as an "mx_regionId" option in the output data
labelToOption <- function (label) {
  return (list(mx_regionId = label))
}

roundmulti <- function (multi, digits) {
  multi <- lapply(multi, function (matrix) {
    matrix <- lapply(matrix, function (coords) {
      round(coords, digits)
    })
  })
  return (st_multipolygon(multi))
}

roundpoly <- function (poly, digits) {
  poly <- lapply(poly, function (matrix) {
      round(matrix, digits)
  })
  return (st_polygon(poly))
}

round_sf <- function (fc, digits) {
  # https://gis.stackexchange.com/questions/329110/removing-empty-polygon-from-sf-object-in-r
  simple  <- fc %>% st_simplify(preserveTopology = TRUE, dTolerance = 5) %>% dplyr::filter(!st_is_empty(.))
  geom <- simple$geometry
  geom <- lapply(geom, function (one) {
    if (inherits(one, "MULTIPOLYGON")) {
      one <- roundmulti(one, digits)
    } else if (inherits(one, "POLYGON")) {
      one <- roundpoly(one, digits)
    } else if (inherits(one, "XY")) {
      one # Don't do anything to points, they are exact
    } else if (!st_is_empty(one)) {
      stop(paste("I don't know what it is ", class(one)))
    }
  })
  simple$geometry <- st_sfc(geom)
  simple
}

mx_read <- function (filename, digits = 4) {
  st_data <- st_read(filename, quiet=TRUE);
  dropped <- st_zm(st_data, drop = T, what = "ZM")
  trans <- lat_lon(dropped);
  rounded <- round_sf(trans, digits);
}

mx_http_fetch <- function (target, url) {
  response <- httr::GET(url, httr::progress())
  
  # Check if the download was successful
  if (httr::status_code(response) == 200) {
    # Write the downloaded content to the file
    base::writeBin(httr::content(response, "raw"), target)
    size <- file.info(target)$size
    cat("Downloaded ", size, " bytes as ", target, "\n")
    
  } else {
    stop("Error ", httr::status_code(response), " when downloading file ", url, "\n");
  }
}

fetch_first_nations_territories = function () {
    file_path <- "external_data/indigenousTerritories.json"
    if (!file.exists(file_path)) {
        mx_http_fetch(file_path, "https://native-land.ca/wp-content/themes/NLD-2021/files/indigenousTerritories.json")
    }
    geojsonsf::geojson_sf(file_path)
}


