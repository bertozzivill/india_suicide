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

files <- c("causes", "means", "profession", "education")
files <- c("profession", "education")

for (name in files){
  print(name)
  load(paste0(main_dir, "clean/", name, ".rdata"))
  data[, sex := factor(sex, labels=c("Males", "Females"))]
  data <- merge(data, loc, by="state", all.x=T)
  data[is.na(deaths), deaths:=0] #set nulls in kerala in 2006 to zero, for now
  
  #TEST: remove lakshadweep
  data <- data[state_id!=19]
  
  #convert death counts for each classification to % of the whole state 
  summed <- data[, list(deaths=sum(deaths)), by="state,state_id,year,classification,classification_label"]
  summed[, summed_deaths:= sum(deaths), by="state,year"]
  summed[, class_percent:= ifelse(summed_deaths==0, 0,deaths/summed_deaths)]
  setkeyv(summed, c("year", "classification"))
  
  #DEATHS BY PERCENT:
  for (class in unique(summed$classification)){
    print(class)
    for_extremes <- summed[classification==class]
    class_label <- for_extremes$classification_label[[1]]
    print(class_label)
    newmin <- min(for_extremes$class_percent)-(mean(for_extremes$class_percent)/3)
    newmax <- max(for_extremes$class_percent)+(mean(for_extremes$class_percent)/3)
    minval <- ifelse(newmin<0, 0, newmin)
    maxval <- ifelse(newmax>1, 1, newmax)
    pdf(paste0(main_dir, "plots/", name, "/death_proportions_", class_label, ".pdf"), width=14, height=8)
    print(paste0(main_dir, "plots/", name, "/death_proportions_", class_label, ".pdf"))
    for (yearval in unique(summed$year)){
      mapdata <- merge(summed[J(yearval, class)], india_map, by="state_id", allow.cartesian=T)
      map_plot<- ggplot(mapdata) +
        geom_polygon(aes(x=long, y=lat, group=group, fill=class_percent)) +
        #geom_path(data=mapdata, aes(x=long, y=lat, group=group)) +
        scale_fill_gradientn(colours=brewer.pal(7, "Reds"), limits=c(minval,maxval)) +
        scale_x_continuous("", breaks=NULL) +
        scale_y_continuous("", breaks=NULL) +
        coord_fixed(ratio=1) +
        guides(fill=guide_colourbar(title="", barheight=20)) +
        labs(title = paste0("Proportion of Deaths Due to ", capitalize(name), ": ", class, ", ", yearval)) +
        theme_bw(base_size=20)
      
      print(map_plot)
    }
    dev.off()
  }
  
  
  #con
#   summed[, year:= paste0("year_", year)]
#   summed <- data.table(dcast(summed, 'state + state_id+classification ~ year', value.var="deaths"))
#   summed[, percent_change:=(year_2010-year_2001)/year_2001]
#   summed[is.na(percent_change), percent_change:=0] #zeros in both years -> zero percent change
#   summed[is.infinite(percent_change), percent_change:=NA] #zeros in first year -> missing percent change
#   
  #map all deaths in all years, as a for-example

  
}