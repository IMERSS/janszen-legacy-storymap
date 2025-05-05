library(sf)
library(geojsonsf)
library(jsonlite)
library(googledrive)
library(stringr)
library(readr)

# Helpful utility to template an arbitrary list of string arguments and then dump them to the console with a terminating newline
wg <- function (...) {
  args <- list(...)
  line <- paste(sapply(args, str_glue, .envir = parent.frame()), collapse = "")

  # Output the result using writeLines
  writeLines(line)
}

downloadGdrive <- function (id, file_path, overwrite = FALSE) {
  if (overwrite || !file.exists(file_path)) {
    drive_download(as_id(id), path = file_path, overwrite = TRUE)
  }
}

idToDrib <- function (id) {
  path <- str_glue("https://drive.google.com/drive/folders/{id}")
  drib <- drive_get(as_id(path))
}

# Adapted from https://stackoverflow.com/a/64687628
downloadGdriveFolder <- function (id, file_path, skip_if_exists = TRUE) {
  exists <- file.exists(file_path)
  if (!exists || !skip_if_exists) {
    if (!exists) {
      dir.create(file_path)
    }
    # folder link to id
    folder_drib = idToDrib(id)

    # find files in folder
    files = drive_ls(folder_drib)

    cat("Fetching ", nrow(files), " files in folder ", folder_drib$name, "\n")

    # loop dirs and download files inside them
    for (i in seq_along(files$name)) {
      resource <- files$drive_resource[[i]]

      target <- str_c(file_path, "/", resource$name)
      if (resource$mimeType == "application/vnd.google-apps.folder") {
        cat (resource$name, " is a folder\n")
        downloadGdriveFolder(resource$id, target, skip_if_exists)
        # If there were subfolders, this would list them:
        # i_dir = drive_ls(files[i, ])
      }
      else {

        try({
          if (file.exists(target)) {
            wg("File {target} already exists, skipping download")
          } else {
            drive_download(as_id(files$id[i]), path = target)
          }
        })
      }
    }
  } else {
    wg("Path {file_path} already exists, skipping download\n")
  }
}

timedRead <- function (toread) {
  start <- Sys.time()
  frame <- read.csv(toread, encoding = 'UTF-8', colClasses=c("character"), na.strings = c("", "NA"))
  end <- Sys.time()
  cat("Read ", nrow(frame), " rows from ", toread, " in ", (end - start), "s")
  frame
}

timedFread <- function (toread) {
  start <- Sys.time()
  frame <- data.table::fread(toread, quote = "", encoding = 'UTF-8', colClasses=c("character"))
  end <- Sys.time()
  cat("Read ", nrow(frame), " rows from ", toread, " in ", (end - start), "s")
  frame
}

timedWrite <- function (x, towrite) {
  start <- Sys.time()
  readr::write_csv(x, towrite, na = "")
  end <- Sys.time()
  cat("Written ", nrow(x), " rows to ", towrite, " in ", (end - start), "s")
}

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

# Taken from
#' @title Expand Spatial Bounding Box
#' @description Expand an \code{sf} bounding box by an expansion factor
#' @param bbox An \code{sf} bounding box. See \code{\link[sf:st_bbox]{sf::st_bbox}}.
#' @param ef Expansion factor, must be positive and length 1, 2 or 4.
#' If \code{length(ef)==1} then the expansion factor is applied equally in all 4 directions.
#' If \code{length(ef)==2} then the first refers to the \code{x} coordinate
#' and the second is associated with \code{y} coordinate expansion. If \code{length(ef)==4} then
#' then the first 2 refer to expansion to the west and east of the x coordinate, and the last
#' to expand the y coordinate in the south and north direction respectively.
#' @author Josh M. London
#' @importFrom sf st_bbox
#' @export
#'
st_expand <- function(bbox, ef) {
  if(length(ef)==1) ef <- c(ef, ef, ef, ef)
  if(length(ef)==2) ef <- c(ef[1],ef[1], ef[2],ef[2])
  if(!length(ef)%in%c(1,2,4)) stop("'ef' argument must be of length 1, 2, or 4")
  xmin <- as.numeric(bbox$xmin)
  xmax <- as.numeric(bbox$xmax)
  ymin <- as.numeric(bbox$ymin)
  ymax <- as.numeric(bbox$ymax)
  x_min <- xmin - ef[1]*(xmax-xmin)
  x_max <- xmax + ef[2]*(xmax-xmin)
  y_min <- ymin - ef[3]*(ymax-ymin)
  y_max <- ymax + ef[4]*(ymax-ymin)
  bbox <- st_bbox(c(xmin = x_min, xmax = x_max,
                    ymax = y_max, ymin = y_min),
                  crs = st_crs(bbox))
  return(bbox)
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
