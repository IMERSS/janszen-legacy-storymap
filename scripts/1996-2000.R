library(sf)
library(leaflet)
library(raster)

source("scripts/utils.R")

# Layer 2: Boundary
boundary <- mx_read("spatial_data/vectors/boundary")

# Layer 3: 1996-2000 Localities (polygons)
polygons.1996_2000 <- mx_read("spatial_data/vectors/1996-2000_polygons")

# Layer 4: 1996-2000 Localities (points)
points.1996_2000 <- mx_read("spatial_data/vectors/1996-2000_points")

points.coordinates <- data.frame(st_coordinates(points.1996_2000))

points.1996_2000$X <- points.coordinates$X
points.1996_2000$Y <- points.coordinates$Y

# Define map bounds based on extent of combined SHP files (all shapes represented in project)

bbox <- st_bbox(polygons.1996_2000) %>% as.vector()

# Render leaflet map

Map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addPolygons(data = polygons.1996_2000, color = "#d5b43c", weight = 1, fillOpacity = 0,
              label = paste(polygons.1996_2000$Locality, polygons.1996_2000$Province, polygons.1996_2000$Country, sep = ", ")) %>%
  addCircleMarkers(data = points.1996_2000, ~X, ~Y, label = paste(points.1996_2000$Locality,
              points.1996_2000$Province, points.1996_2000$Country, sep = ", "),
              fillColor = "#d5b43c",
              fillOpacity = 1,
              stroke = F,
              radius = 4)  %>% 
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

print(Map)

# Create pie chart summarizing no. of records digitized by research collection (RBCM, UBC)

RBCM.records <- read.csv("tabular_data/HJ_RBCM_Records_2021-11-15.csv")
UBC.records <- read.csv("tabular_data/HJ_UBC_records_2023-02-12.csv")

# First remove non-numeric characters from collection numbers

RBCM.records$CollectorsFieldNumber <- gsub("[^0-9.]", "", RBCM.records$CollectorsFieldNumber)
RBCM.records$CollectorsFieldNumber <- as.numeric(RBCM.records$CollectorsFieldNumber)

UBC.records$Collector.Number <- gsub("[^0-9.]", "", UBC.records$Collector.Number)
UBC.records$Collector.Number <- as.numeric(UBC.records$Collector.Number)

# Remove records not attributable to Harvey from UBC records

UBC.records <- UBC.records %>% dplyr::filter(str_detect(Primary.Collector, 'Harvey Janszen'))

# Select records having collection numbers in the range of 1:1929 (collections from this time period, as per field notes)

RBCM.records.1996_2000 <- subset(RBCM.records, RBCM.records$CollectorsFieldNumber %in% 2630:2920)
UBC.records.1996_2000 <- subset(UBC.records, UBC.records$Collector.Number %in% 2630:2920)

# Note: there are duplicate specimens accessioned at both RBCM and UBC for this time period;
# Thus, to account for the number of records that remain undigitized, subtract the total number of 
# unique collection numbers represented in both research collections from the total number of specimens 
# accounted for in Harvey's journal

range(RBCM.records.1996_2000$CollectorsFieldNumber)
range(UBC.records.1996_2000$Collector.Number)

sort(unique(RBCM.records.1996_2000$CollectorsFieldNumber))
sort(unique(UBC.records.1996_2000$Collector.Number))

nrow(RBCM.records.1996_2000$CollectorsFieldNumber)
nrow(UBC.records.1996_2000$Collector.Number)

unique.RBCM.UBC.records <- c(RBCM.records.1996_2000$CollectorsFieldNumber, UBC.records.1996_2000$Collector.Number)

unique.RBCM.UBC.records <- length(unique(unique.RBCM.UBC.records))

# Sum specimens accessioned for the time period and infer number of records that remain undigitized

RBCM.HJ <- nrow(RBCM.records.1996_2000) 
UBC.HJ <- nrow(UBC.records.1996_2000)
undigitized <- 290-(unique.RBCM.UBC.records)

# Create Plotly Donut Chart

Collection <- c("RBCM", "UBC", "Undigitized")
Records <- c(RBCM.HJ, UBC.HJ, undigitized)

colors <-  c("#ffcf20FF", "#10a53dFF", "#541352FF")

digitized.summary <- data.frame(Collection,Records)

digitized.records.plot <- digitized.summary %>% plot_ly(labels = ~Collection, values = ~Records, 
                                                textposition = 'inside',
                                                textinfo = 'label+percent',
                                                insidetextfont = list(color = '#FFFFFF'),
                                                hoverinfo = 'text',
                                                marker = list(colors = colors,
                                                line = list(color = '#FFFFFF', width = 1)), showlegend = T,
                                                width = 450,
                                                height = 300)
digitized.records.plot <- digitized.records.plot %>% add_pie(hole = 0.6)
digitized.records.plot <- digitized.records.plot %>% layout(title = "Records digitized by collection",  showlegend = T,
                                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

digitized.records.plot

# Create pie chart summarizing no. of pages digitized from journal

digitized.journal <- read.csv("tabular_data/digitization-progress_2023-03-14.csv")

digitized.journal <- digitized.journal %>% filter(journalNumber == 8)

# Summary digitized vs undigitized pages

digitized <- digitized.journal$total_digitized_pages
undigitized <- digitized.journal$journalPages - digitized.journal$total_digitized_pages

# Create Plotly Donut Chart

Digitized <- c("Digitized", "Undigitized")
Pages <- c(digitized, undigitized)

digitized.journal.summary <- data.frame(Digitized,Pages)

colors.2 <-  c("#10a53dFF", "#541352FF")

digitized.journal.plot <- digitized.journal.summary %>% plot_ly(labels = ~Digitized, values = ~Pages, 
                                                                textposition = 'inside',
                                                                textinfo = 'label+percent',
                                                                insidetextfont = list(color = '#FFFFFF'),
                                                                hoverinfo = 'text',
                                                                marker = list(colors = colors.2,
                                                                              line = list(color = '#FFFFFF', width = 1)), showlegend = T,
                                                                width = 450,
                                                                height = 300) 
digitized.journal.plot <- digitized.journal.plot %>% add_pie(hole = 0.6)
digitized.journal.plot <- digitized.journal.plot %>% layout(title = "Journal Pages Digitized",  showlegend = T,
                                                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

digitized.journal.plot

