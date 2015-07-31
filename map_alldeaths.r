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
shapefile_dir <- paste0(main_dir, "plots/shapefiles/")

## format shapefile
load(paste0(shapefile_dir, "prepped_shapefile.rdata"))
india_map <- data.table(fortify(india_map))
india_map[, state_id := as.numeric(id)]

loc<- fread(paste0(shapefile_dir, "loc.csv"))

files <- c("means")

for (name in files){
  print(name)
  load(paste0(main_dir, "clean/", name, ".rdata"))
  data[, sex := factor(sex, labels=c("Males", "Females"))]
  #drop states we don't want
  data <- merge(data, loc, by="state", all=T) 
  data[is.na(deaths), deaths:=0] #set nulls in kerala in 2006 to zero, for now
  
  #convert death counts for each classification to % of the whole state 
  summed <- data[, list(deaths=sum(deaths)), by="state,state_id,year,classification,classification_label"]
  summed[, summed_deaths:= sum(deaths), by="state,year"]
  summed[, class_percent:= ifelse(summed_deaths==0, 0,deaths/summed_deaths)]
  setkeyv(summed, c("year", "classification"))
  
  #DEATHS BY PERCENT:
  pdf(paste0(main_dir, "plots/", name, "/death_proportions_all.pdf"), width=14, height=8)
  for (yearval in unique(summed$year)){
    print(yearval)
    mapdata <- merge(summed[J(yearval)], india_map, by="state_id", allow.cartesian=T)
    map_plot<- ggplot(mapdata) +
      geom_polygon(aes(x=long, y=lat, group=group, fill=class_percent)) +
      geom_path(data=mapdata, aes(x=long, y=lat, group=group)) +
      scale_fill_gradientn(colours=brewer.pal(7, "Reds"), limits=c(0,1)) +
      facet_wrap(~classification) + 
      scale_x_continuous("", breaks=NULL) +
      scale_y_continuous("", breaks=NULL) +
      coord_fixed(ratio=1) +
      guides(fill=guide_colourbar(title="", barheight=20)) +
      labs(title = paste0("Proportion of Deaths by ", capitalize(name), ", ", yearval)) +
      theme_bw(base_size=20)
    
    print(map_plot)
  }
  dev.off()
  
}