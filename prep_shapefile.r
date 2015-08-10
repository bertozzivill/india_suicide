####################################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Prep a shapefile for India that includes the proper shape of Jammu and Kashmir.
##  Note that this shapefile contains boundaries for the union territories of Puducherry, Andaman & Nicobar,
##  and Chandigarh, but NOT Dadra & Nagar Haveli, Daman & Diu, or Lakshadweep.
## Output: a spatialPolygonsDataFrame named 'india_map' where state_id is the ID variable for each
##  polygon (1-32, in alphabetical order)
####################################################################################################

library(data.table)
library(maptools)
library(rgeos)
library(rgdal)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/plots/shapefiles/"

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
