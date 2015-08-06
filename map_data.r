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
rbPal <- colorRampPalette(c("red", "gold","dark green"))
redgreencolors <- rbPal(7)
#mapcolors <- c("#001C73", "#0070FF", "#00A884", "#A3FF73", "#FFFF00", "#FFBF00", "darkorange", "#FF0000", "#730000")
shapefile_dir <- paste0(main_dir, "plots/shapefiles/")

source(paste0(main_dir, "../code/plotting_fns.r"))

##function to get rates and proportions from dataset
sumvars <- function(data, pop, bysum, byprop, byrate){
  summed <- data[, list(deaths=sum(deaths)), by=bysum]
  summed[, summed_deaths:= sum(deaths), by=byprop]
  summed[, prop:= ifelse(summed_deaths==0, 0,deaths/summed_deaths)]
  sumpop <- pop[, list(pop=sum(pop)), by=byrate]
  summed <- merge(summed, sumpop, by=intersect(names(summed), names(sumpop)), all=T)
  summed[, rate:=(deaths/pop)*1000]
  return(summed)
}

## format shapefile
load(paste0(shapefile_dir, "prepped_shapefile.rdata"))
india_map <- data.table(fortify(india_map))
india_map[, state_id := as.numeric(id)]

load(paste0(main_dir, "clean/pop.rdata"))
pop[, year:=as.factor(year)]
pop[, age:=as.factor(age)]
pop <- pop[age!=0]

files <- c("causes", "means")

for (name in files){
  print(name)
  load(paste0(main_dir, "clean/", name, ".rdata"))
  data[, sex := factor(sex, labels=c("Males", "Females"))]
  data[, year:=as.factor(year)]
  data[, age:=as.factor(age)]
  data[, classification:=as.factor(classification)]
  #remove age=0 
  data <-data[age!=0]
  
  for (typeval in c("prop", "rate")){
    if (typeval=="prop"){
      labelvar<-"Proportions"; limvals<-c(0,1); colors<-brewer.pal(7, "Reds");
    }else{
      labelvar<-"Rates"; limvals<-NULL; colors<-redgreencolors;
    }
    print(paste("plotting", labelvar))
  
    for (facet_val in c("classification", "year")){
      loop_val <- ifelse(facet_val=="classification", "year", "classification")
      summed <- sumvars(data, pop, bysum="state,state_id,year,classification,classification_label", byprop="state,year", byrate="state,state_id,year")
      setkeyv(summed, loop_val)
      
      pdf(paste0(main_dir, "plots/", name, "/state/death_", typeval, "_loop_", loop_val, ".pdf"), width=14, height=8)
      for (this_loop in unique(summed[[loop_val]])){
        print(this_loop)
        mapdata <- merge(summed[J(this_loop)], india_map, by="state_id", allow.cartesian=T)
        image <- map_plot(mapdata,
                          fillvar=typeval,
                          facet_str=paste0("~", facet_val),
                          title=paste("Death", labelvar, "by", capitalize(name), ",", this_loop),
                          colors=rev(colors),
                          limvals=limvals)
        
        print(image)
      }
      dev.off()
    }
  }
  
}