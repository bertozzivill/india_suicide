####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Suicide data from the NCRB in India were formatted uniformly in the
## 'clean_data' script in this folder. Here, I make preliminary maps for 
## each of the cleaned datasets. 
####################################################################################

library(foreign)
library(data.table)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(RColorBrewer)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Desktop/practicum/suicide/data/"
mapcolors <- c("#001C73", "#0070FF", "#00A884", "#A3FF73", "#FFFF00", "#FFBF00", "darkorange", "#FF0000", "#730000")
shapefile_dir <- paste0(main_dir, "plots/shapefiles/")

## format shapefile
load(paste0(shapefile_dir, "prepped_shapefile.rdata"))
india_map <- data.table(fortify(india_map))
india_map[, state_id := as.numeric(id)]

load(paste0(shapefile_dir, "loc.rdata"))

files <- c("causes")

for (name in files){
  print(name)
  load(paste0(main_dir, "clean/", name, ".rdata"))
  data[, sex := factor(sex, labels=c("Males", "Females"))]
  data <- merge(data, loc, by="state", all.x=T)
  data[is.na(deaths), deaths:=0] #set nulls in kerala in 2006 to zero, for now
  
  #plot percent change in death counts from 2001 to 2010, by classification
  summed <- data[year %in% c(2001,2010), list(deaths=sum(deaths)), by="state,state_id,year,classification"]
  summed[, year:= paste0("year_", year)]
  summed <- data.table(dcast(summed, 'state + state_id+classification ~ year', value.var="deaths"))
  summed[, percent_change:=(year_2010-year_2001)/year_2010]
  
  #map all deaths in all years, as a for-example
#   mapdata <- merge(summed, india_map, by="state_id", allow.cartesian=T)
#   map_plot<- ggplot(mapdata) +
#     geom_polygon(aes(x=long, y=lat, group=group, fill=deaths)) +
#     geom_path(data=mapdata, aes(x=long, y=lat, group=group)) +
#     scale_fill_gradientn(colours=brewer.pal(7, "Reds")) +
#     scale_x_continuous("", breaks=NULL) +
#     scale_y_continuous("", breaks=NULL) +
#     coord_fixed(ratio=1) +
#     guides(fill=guide_colourbar(title="Deaths:", barheight=20)) +
#     labs(title = paste0("All Suicides")) +
#     theme_bw(base_size=20)
#   
#   print(map_plot)
  
}