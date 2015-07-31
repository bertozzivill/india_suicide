####################################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Prep a shapefile for India that removes Telangana (it wasn't a state for the years
##    of our analysis), and create a dataset that maps from state name/id to 'merged' id
## Output: a spatialPolygonsDataFrame named 'indis_map' where state_id is the ID variable for each
##  polygon, and a data.table named 'loc' that maps state name to state_id (1-36 alphabetically, including
## Telangana) and merged_id (1-35 alphabetically, except that Telangana=2)
####################################################################################################

library(data.table)
library(maptools)
library(rgeos)
library(rgdal)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Desktop/practicum/suicide/data/plots/shapefiles/"

#load locations file
loc <-fread(paste0(main_dir, "loc.csv"))

# load shape file
india_map <- readOGR(paste0(main_dir, "download_toofew/india-states-edited-2.shp"), layer="india-states-edited-2")

statenames <- data.table(india_map@data)
#sort @data attribute
india_map@data$unordered_id <- sapply(india_map@polygons, function(x) x@ID)
india_map@data <- india_map@data[order(india_map$NAME_1),]
india_map@data$ordered_id <- order(india_map@data$NAME_1)

#unsort @data
india_map@data <- india_map@data[order(as.numeric(india_map$unordered_id)),]

#change ID attribute of each @polygons attribute
idx<-1
for (ordered_val in india_map@data$ordered_id){
  print(ordered_val)
  india_map@polygons[[idx]]@ID <- as.character(ordered_val)
  print(india_map@polygons[[idx]]@ID)
  idx <- idx+1
}

#sort @polygons attribute
india_map <- india_map[order(india_map@data$ordered_id),]

# save 
save(india_map, file=paste0(main_dir, "prepped_shapefile.rdata"))
