####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Tabulate all the numbers that will actually go into the paper,
## reasons-for-suicide level
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
source(paste0(main_dir, "../code/plotting_fns.r"))
source(paste0(main_dir, "../code/multiplot.r"))
load(paste0(main_dir, "clean/pop.rdata"))
pop[, year:=as.factor(year)]
pop[, age:=as.factor(age)]
pop[, ag_status := ifelse(ag_state==1, "Agricultural", "Non-Agricultural")]
pop[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
pop <- pop[age!=0]

rate_per <- 100000

load(paste0(main_dir, "clean/profession.rdata"))
data[, sex := factor(sex, labels=c("Males", "Females"))]
data[, ag_status := ifelse(ag_state==1, "Agricultural", "Non-Agricultural")]
data[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
data[, national:="National"]
data[, year:=as.factor(year)]
data[, age:=as.factor(age)]
data[, classification:=as.factor(classification)]
#remove age=0 
data <-data[age!=0]

##############################
### All suicides
###############################

#count and rate of suicides nationally
bystr <- "year"
national <- sumvars(data, pop, bysum=paste0(bystr, ",classification"), byprop=bystr, byrate=bystr, rate_per=rate_per)

#count and rate of suicides nationally by sex
bystr <- "year,sex"
national_sex <- sumvars(data, pop, bysum=paste0(bystr, ",classification"), byprop=bystr, byrate=bystr, rate_per=rate_per)

#count and rate of suicides nationally by sex and age
bystr <- "year,sex,agename"
national_sex_age <- sumvars(data, pop, bysum=paste0(bystr, ",classification"), byprop=bystr, byrate=bystr, rate_per=rate_per)

#count and rate of suicides dev-wise
bystr <- "year,dev_status"
dev <- sumvars(data, pop, bysum=paste0(bystr, ",classification"), byprop=bystr, byrate=bystr, rate_per=rate_per)

#count and rate of suicides dev-wise, by age
bystr <- "year,dev_status,agename"
dev_age <- sumvars(data, pop, bysum=paste0(bystr, ",classification"), byprop=bystr, byrate=bystr, rate_per=rate_per)

#count and rate of suicides dev-wise, by sex
bystr <- "year,dev_status,sex"
dev_sex <- sumvars(data, pop, bysum=paste0(bystr, ",classification"), byprop=bystr, byrate=bystr, rate_per=rate_per)

#count and rate of suicides ag-wise
bystr <- "year,ag_status"
ag <- sumvars(data, pop, bysum=paste0(bystr, ",classification"), byprop=bystr, byrate=bystr, rate_per=rate_per)

#count and rate of suicides ag-wise, by sex
bystr <- "year,ag_status,sex"

#count and rate of suicides ag-wise, by sex
bystr <- "year,state,sex"
statewise <- sumvars(data, pop, bysum=paste0(bystr, ",classification"), byprop=bystr, byrate=bystr, rate_per=rate_per)