####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: We're interested in seeing if some causes/means of suicide are more 
##              common in states with high percentages of agricultural workers. Here,
##              we prepare data for what percent of each state's population engages
##              in agriculture.
####################################################################################

library(foreign)
library(data.table)
library(reshape2)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/"

#import data
loc<- fread(paste0(main_dir, "clean/loc.csv"))
ag_pop <- fread(paste0(main_dir, "clean/ag_pop.csv"))
load(paste0(main_dir, "clean/pop_for_ag.rdata"))

#collapse ag by sex
ag_pop <- ag_pop[,list(ag_pop=sum(ag_pop)), by="state,year"]

#collapse pop by age and sex
pop <- pop[age!=0 & age!=65]
pop <- pop[, list(pop=sum(pop)), by="year,state,state_id"]
pop[, year:=as.integer(as.character(year))]
ag_pop<- merge(ag_pop, pop, by=c("year","state"), all.x=T)
ag_pop[, perc_ag:=(ag_pop/pop)*100]
ag_pop[, year:=as.factor(year)]


#load and prep shapefile
rbPal <- colorRampPalette(c("red", "gold","dark green"))
redgreencolors <- rbPal(7)
shapefile_dir <- paste0(main_dir, "plots/shapefiles/")

source(paste0(main_dir, "../code/plotting_fns.r"))

load(paste0(shapefile_dir, "prepped_shapefile.rdata"))
india_map <- data.table(fortify(india_map))
india_map[, state_id := as.numeric(id)]

#map values and change
plot_dir <- paste0(main_dir, "plots/agriculture/")

mapdata <- merge(ag_pop, india_map, by="state_id", allow.cartesian=T)
pdf(paste0(plot_dir, "agriculture_percent.pdf"), width=14, height=8)
image <- map_plot(mapdata,
                  fillvar="perc_ag",
                  facet_str="~year",
                  title=paste("Percent of Population in Agriculture, by  Year"),
                  colors=redgreencolors)
print(image)
dev.off()

#reshape
ag_pop[, year_str:=paste0("perc_", year)]
wider <- dcast.data.table(ag_pop, state + state_id ~ year_str, value.var="perc_ag")
wider[, perc_change:=perc_2011-perc_2001]
wider[, label:="Change Over Time"]

mapdata <- merge(wider, india_map, by="state_id", allow.cartesian=T)
perc <- map_plot(mapdata,
                  fillvar="perc_change",
                  facet_str="~label",
                  title=paste("Percent Change in Population in Agriculture, 2001-2011"),
                  colors=redgreencolors)
print(perc)

#ARBITRARY: Say people are in a "high-agriculture" state if perc_ag in 2011 >10
wider[, ag_state:=ifelse(perc_2011>10, 1,0)]
wider[, label:="Agriculture State?"]

mapdata <- merge(wider, india_map, by="state_id", allow.cartesian=T)
binary <- map_plot(mapdata,
                 fillvar="ag_state",
                 facet_str="~label",
                 outline=T,
                 title=paste("Agriculture States for Analysis"),
                 colors=c("white", "red"))
print(binary)
