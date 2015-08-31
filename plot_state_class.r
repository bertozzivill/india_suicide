####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Suicide data from the NCRB in India were formatted uniformly in the
## 'clean_data' script in this folder. Here, I make preliminary plots for 
## each of the cleaned datasets. 
####################################################################################

library(foreign)
library(data.table)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(grDevices)
library(RColorBrewer)
options(scipen=10)


rm(list=ls())
main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/"

#prep and format shapefils
rbPal <- colorRampPalette(c("red", "gold","dark green"))
redgreencolors <- rbPal(7)
shapefile_dir <- paste0(main_dir, "plots/shapefiles/")
load(paste0(shapefile_dir, "prepped_shapefile.rdata"))
india_map <- data.table(fortify(india_map))
india_map[, state_id := as.numeric(id)]

#read in data
files <- c("causes", "means", "profession")
source(paste0(main_dir, "../code/plotting_fns.r"))
source(paste0(main_dir, "../code/multiplot.r"))
load(paste0(main_dir, "clean/pop.rdata"))
pop[, year:=as.factor(year)]
pop[, age:=as.factor(age)]
pop[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
pop <- pop[age!=0]

rate_per <- 100000

pdf(paste0(main_dir, "plots/summary/states_by_class.pdf"), width=14, height=8)

for (name in files){
  print(name)
  load(paste0(main_dir, "clean/", name, ".rdata"))
  data[, sex := factor(sex, labels=c("Males", "Females"))]
  data[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
  data[, national:="National"]
  data[, year:=as.factor(year)]
  data[, age:=as.factor(age)]
  data[, classification:=as.factor(classification)]
  
  #remove age=0
  data <-data[age!=0]
  
  #set colors 
  n <- length(unique(data$classification))
  if (n>8){
    colors <- brewer.pal(8, "Set2")
    colors <- c(colors, "#A067B1")
  }else{
    colors <- brewer.pal(n, "Set2")
  } 
  
  level_base <- "state"
  for (levelval in c("year", "sex", "agename")){
    print(levelval)
    level_str <- ifelse(levelval=="year", paste0(level_base, ",", levelval), paste0(level_base, ",year,", levelval))
    summed <- sumvars(data, pop, bysum=paste0(level_str, ",classification,national"), byprop=level_str, byrate=level_str, rate_per=rate_per)
    summed[, year:= as.numeric(as.character(year))]
    levelval <- ifelse(levelval=="year", "national", levelval)
    setkeyv(summed, levelval)
    
    for (eachval in unique(summed[[levelval]])){
      print(eachval)
      yvar <- ifelse(name=="means", "rate", "prop")
      #yvar <- "prop"
      
      image <- ggplot(summed[J(eachval)], aes_string(x="year", y=yvar, group="classification"))+
        geom_line(aes(color=classification), size=2) +
        #stat_smooth(aes(color=classification),method="lm")+
        facet_wrap(~state, scales="free_y") +
        scale_color_manual(values=colors)+
        scale_x_continuous(breaks=c(2001, 2006, 2010), minor_breaks=c(2002,2003,2004,2005,2007,2008,2009)) +
        labs(title=paste("Death", capitalize(yvar), "by", capitalize(name), eachval),
             x="Year",
             y=capitalize(yvar))
      
      print(image)
      
    }
    }
  }
  
graphics.off()
