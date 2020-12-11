library(sf)
library(tidyverse)
library(sp)
library(ggmap)
library(ggraph)
library(leaflet)
library(spatstat)
library(readxl)

csv_data <- read.csv("raw_data/verkehrsunfälle_stadt_zh_relevant_komma.csv")
csv_zaehlstellen <- read.csv("raw_data/verkehrszaehlung_stadt_zh_relevant_komma.csv")
tail(csv_data)



data_points <- st_as_sf(csv_data,  coords = c("AccidentLocation_CHLV95_E", "AccidentLocation_CHLV95_N"), crs = 2056)
zaehlstellen <- st_as_sf(csv_zaehlstellen,  coords = c("EKoord", "NKoord"), crs = 2056)

ggplot() +
  geom_sf(data = data_points)


# data_points_shp <- st_write(data_points, "verkehrsunfaelle_stadt_zh_points_new.shp", driver = "ESRI Shapefile")
# data_points_shp <- st_write(zaehlstellen, "zaehlstellen_cleaned_points.shp", driver = "ESRI Shapefile")

test2