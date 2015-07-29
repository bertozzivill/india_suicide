####################################################################################################
## Author: Laura Dwyer-Lindgren
## Description: Prep a shapefile for the merged counties that are the unit of analysis for the
##              small area risk factors work in the US.
## Output: a spatialPolygonsDataFrame named 'map' where mcounty is the ID variable for each polygon.
####################################################################################################

library(data.table)
library(maptools)
library(rgeos)
library(rgdal)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Desktop/practicum/suicide/data/plots/shapefiles/"

#load locations file
load(paste0(main_dir, "../../clean/causes.rdata"))
loc <- unique(data[order(state), list(state)])
loc$merged_id <-rownames(loc)
#add Telangana (formerly part of Andhra Pradesh)
loc <- rbind(loc, list("Telangana", 2))
loc <- loc[order(state)]
loc$state_id<-as.numeric(rownames(loc))

# load shape file
india_map <- readOGR(paste0(main_dir, "IND_adm1.shp"), layer="IND_adm1")
#india_map <- spTransform(india_map, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
india_map@data <- data.table(state_id = as.numeric(as.character(india_map@data$ID_1)))

#manually make adjustments to merge Telangana back into Andhra Pradesh
india_map@data[state_id==32, state_id:=2]
india_map <- unionSpatialPolygons(india_map, IDs=india_map@data$state_id)
india_map <- SpatialPolygonsDataFrame(india_map, data.table(state_id = sapply(india_map@polygons, function(x) x@ID)), F)
india_map@data[, state_id := as.numeric(as.character(state_id))]
india_map <- india_map[order(india_map@data$state_id),]
india_map@data <- merge(india_map@data, loc, by="state_id")
india_map@data <- india_map@data[order(state_id),]

# save these shapefiles (to be used for actual geographic analysis where distances and position matter)
save(india_map, file=paste0(main_dir, "prepped_shapefile.rdata"))
save(loc, file=paste0(main_dir, "loc.rdata"))
