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
library(gdata)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Desktop/practicum/suicide/data/"
rbPal <- colorRampPalette(c("red", "gold","dark green"))
redgreencolors <- rbPal(7)
#mapcolors <- c("#001C73", "#0070FF", "#00A884", "#A3FF73", "#FFFF00", "#FFBF00", "darkorange", "#FF0000", "#730000")
shapefile_dir <- paste0(main_dir, "plots/shapefiles/")

source(paste0(main_dir, "../code/plotting_fns.r"))

## format shapefile
load(paste0(shapefile_dir, "prepped_shapefile.rdata"))
india_map <- data.table(fortify(india_map))
india_map[, state_id := as.numeric(id)]

#read in datasets for cleaner age names and plots to loop through
agenames <- fread(paste0(main_dir, "plots/agenames.csv"))
looplist <- fread(paste0(main_dir, "plots/plot_loops.csv"), header=T)

#read data
load(paste0(main_dir, "clean/pop.rdata"))
pop[, year:=as.factor(year)]
pop <- pop[age!=0]
pop <- merge(pop, agenames, by="age", all=T)
pop[, age:=as.factor(age)]

files <- c("causes", "means")

for (name in files){
  print(name)
  load(paste0(main_dir, "clean/", name, ".rdata"))
  #merge on better age names
  data <- merge(data, agenames, by="age", all=T)
  data[, sex := factor(sex, labels=c("Males", "Females"))]
  data[, year:=as.factor(year)]
  data[, age:=as.factor(age)]
  data[, classification:=as.factor(classification)]
  #remove age=0 
  data <-data[age!=0]
  data[, all:=paste("All", capitalize(name))]
  
  for (typeval in c("rate", "prop")){
    if (typeval=="prop"){
      labelvar<-"Proportions"; colors<-brewer.pal(7, "Reds");
    }else{
      labelvar<-"Rates"; colors<-rev(redgreencolors);
    }
    print(paste("plotting", labelvar))
    
    #loop through every type of map you want
    for (level_val in unique(looplist$level)){
      print(level_val)
      this_level <- looplist[level==level_val]
      #assign values to column names
      for (colname in names(this_level)){
        assign(colname, this_level[[colname]][[1]])
      }
      
      if (facet_val=="classification" & typeval=="prop") limvals <- c(0,1) else limvals<-NULL 
      
      #make loop_vals into a vector
      loop_vals <- strsplit(loop_vals, "_")[[1]]
      
      #loop through the elements of loop val to get name combinations (i.e. every combination of agename and sex)
      loop_idx <- 1
      to_combine <- NULL
      for (loop_val in loop_vals){
        unique_loops <- unique(data[[loop_val]])
        to_combine[[loop_idx]] <- unique_loops
        loop_idx <- loop_idx+1
      }
      loop_combos <- expand.grid(to_combine)
      
      #generate summed dataset
      summed <- sumvars(data, pop, bysum=bysum, byprop=byprop, byrate=byrate)
      setkeyv(summed, loop_vals)
      
      #start pdf
      pdf(paste0(main_dir, "plots/", name, "/state/", ifelse(startsWith(level, "year"), "year/", "class/"), typeval, "_", level, ".pdf"))
      
      #finally, loop through the values of 'loop_combos' to make individual plots
      loop_depth <- length(loop_vals)
      for(n in 1:nrow(loop_combos)){
        nvals <- loop_combos[n, 1:loop_depth]
        ntitle<- ifelse(length(nvals)==1, as.character(nvals), do.call("paste", nvals))
        print(ntitle)
        mapdata <- merge(summed[J(nvals)], india_map, by="state_id", allow.cartesian=T)
        image <- map_plot(mapdata,
                          fillvar=typeval,
                          facet_str=paste0("~", facet_val),
                          title=paste("Death", labelvar, "by", capitalize(name), ",", ntitle),
                          colors=colors,
                          limvals=limvals)
        print(image)
      }
      dev.off()
    }
  
  }
  
}