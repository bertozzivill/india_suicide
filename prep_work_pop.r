####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Prep population and working population estimates from the censuses,
##              to be used to determine whether or not a state is "agricultural".
##              Want sex-specific population and working population estimates for
##              ages 15-65, years 2001 and 2011
####################################################################################

library(foreign)
library(data.table)
library(reshape2)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/raw/agriculture/"

#2001 is already in the right format
work_2001 <- fread(paste0(main_dir, "census_work_2001_all_land.csv"))
work_2001[, year:=2001]

#2011 needs to sum over age and reshape long
work_2011 <- fread(paste0(main_dir, "census_work_urban_2011_mid.csv"))
work_2011 <- work_2011[,list(tot_pop=sum(tot_pop), work_pop=sum(work_pop)), by="state,land_type"]
work_2011[, year:= 2011]

#combine 2001 and 2011
work_pop <- rbind(work_2001, work_2011)

#find proportion of census population is agriculture
work_pop[, prop_work:=work_pop/tot_pop]

#save
setnames(work_pop, "tot_pop", "census_pop")
save(work_pop, file=paste0(main_dir, "../../clean/agriculture/work_pop.rdata"))
