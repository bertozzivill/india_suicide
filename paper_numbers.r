####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Tabulate all the numbers that will actually go into the paper,
## all-suicides leves
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
pop[, ag_status := ifelse(ag_state==1, "Agricultural", "Non-Agricultural")]
pop[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
pop <- pop[age!=0]

rate_per <- 100000

##############################
### All suicides
###############################

#is_agriculture and is_developed
pdf(file=paste0(main_dir, "plots/summary/dev_ag.pdf"), width=14, height=8)
loc <-  fread(paste0(main_dir, "clean/loc.csv"))
loc[, ag_status := ifelse(ag_state==1, "Agricultural", "Non-Agricultural")]
loc[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
mapdata <- merge(loc, india_map, by="state_id", allow.cartesian=T)

map_1 <- ggplot(mapdata) +
        geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(dev_status))) +
        scale_x_continuous("", breaks=NULL) +
        scale_y_continuous("", breaks=NULL) +
        geom_path(data=mapdata, aes(x=long, y=lat, group=group))+
        coord_fixed(ratio=1) +
        guides(fill=guide_legend(title="", barheight=15)) +
        labs(title = "Development Status") +
        theme_bw(base_size=15) +
        theme(legend.position="bottom")

map_2 <- ggplot(mapdata) +
        geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(ag_status))) +
        scale_x_continuous("", breaks=NULL) +
        scale_y_continuous("", breaks=NULL) +
        geom_path(data=mapdata, aes(x=long, y=lat, group=group))+
        coord_fixed(ratio=1) +
        guides(fill=guide_legend(title="", barheight=15)) +
        labs(title = "Agricultural Status") +
        theme_bw(base_size=15) +
        theme(legend.position="bottom")

multiplot(map_1, map_2, cols=2)

dev.off()

#count and rate of suicides nationally
national <- pop[, list(deaths=sum(deaths), pop=sum(pop)), by="year"]
national[, rate:= (deaths/pop)*100000]

#count and rate of suicides nationally by sex
national_sex <- pop[, list(deaths=sum(deaths), pop=sum(pop)), by="year,sex"]
national_sex[, rate:= (deaths/pop)*100000]

#count and rate of suicides dev-wise
dev <- pop[, list(deaths=sum(deaths), pop=sum(pop)), by="year,dev_status"]
dev[, rate:= (deaths/pop)*100000]

#count and rate of suicides dev-wise, by sex
dev_sex <- pop[, list(deaths=sum(deaths), pop=sum(pop)), by="year,dev_status,sex"]
dev_sex[, rate:= (deaths/pop)*100000]

#count and rate of suicides ag-wise, by sex
ag_sex <- pop[, list(deaths=sum(deaths), pop=sum(pop)), by="year,ag_status,sex"]
ag_sex[, rate:= (deaths/pop)*100000]

#count and rate of suicides state-wise
states <- pop[, list(deaths=sum(deaths), pop=sum(pop)), by="year,state_id,state"]
states[, rate:= (deaths/pop)*100000]

#count and rate of suicides state-wise
states_sex <- pop[, list(deaths=sum(deaths), pop=sum(pop)), by="year,state_id,state,sex"]
states_sex[, rate:= (deaths/pop)*100000]

#count and rate of suicides state-wise
states_sex_age <- pop[, list(deaths=sum(deaths), pop=sum(pop)), by="year,state_id,state,sex,age"]
states_sex_age[, rate:= (deaths/pop)*100000]

pdf(file=paste0(main_dir, "plots/summary/rates_2001_2010.pdf"), width=14, height=8)
mapdata <- merge(states, india_map, by="state_id", allow.cartesian=T)
#map of suicide rates for 2001 and 2010, next to each other
image_1 <- ggplot(mapdata[year %in% c(2001, 2010)]) +
        geom_polygon(aes_string(x="long", y="lat", group="group", fill="rate")) +
        facet_wrap(~year) +
        scale_fill_gradientn(colours=brewer.pal(7, "YlOrBr")[2:7]) +
        scale_x_continuous("", breaks=NULL) +
        scale_y_continuous("", breaks=NULL) +
        coord_fixed(ratio=1) +
        guides(fill=guide_colourbar(title="", barheight=15)) +
        labs(title = "Suicide Rate Per 100,000") +
        theme_bw(base_size=15)
print(image_1)
graphics.off()

pdf(file=paste0(main_dir, "plots/summary/age_15_morewomen.pdf"), width=14, height=8)
  image <- ggplot(states_sex_age[state %in% c("Madhya Pradesh", "Uttar Pradesh") & age==15], aes(x=year, y=rate, group=sex)) +
            geom_line(aes(color=sex), size=2) +
            stat_smooth(method=lm, aes(color=sex), se=F) +
            facet_wrap(~state) +
            theme(legend.position="bottom", legend.title=element_blank()) +
            labs(title="Suicide Rate per 100,000, Age 15-29, Madhya Pradesh and Uttar Pradesh",
                 x="Year",
                 y="Suicides per 100,000")

  print(image)
            
graphics.off()
