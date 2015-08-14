####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Prep GBD2015 Population estimates (by state and year) for use as 
##              denominators in suicide analysis.
####################################################################################

library(foreign)
library(data.table)
library(reshape2)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/"

agenames <- fread(paste0(main_dir, "agenames.csv"))

yearvals <- 2001:2010
data <- fread(paste0(main_dir, "raw/india_pop.csv"))

#drop unneeded years & aggregated sex vals
data<-data[year %in% yearvals & sex!="both"]

#collapse age:
agemap <- data.table(age_group_id=2:21,
                     age=c(rep(0, length(2:7)),
                           rep(15, length(8:10)),
                           rep(30, length(11:13)),
                           rep(45, length(14:16)),
                           rep(60, length(17:21)))
                     )

data <- merge(agemap, data, by="age_group_id", all.x=T)

#drop urban/rural disaggregates and national aggregate (keep only states)
data <- data[as.numeric(substr(ihme_loc_id, 5, 9))<5000]

#generate state_ids corresponding to our shapefile (which includes Andaman & Nicobar, Puducherry, and Chandigarh)
# also map Telangna to Andhra Pradesh
loc <- unique(data$location_name)
loc <- c(loc, "Andaman & Nicobar Islands", "Chandigarh", "Puducherry")
loc <- loc[!loc %in% c("Telangana")]
loc <- data.table(location_name=sort(loc), state_id=1:length(loc))
loc <- rbind(loc, list("Telangana", 2))

data <- merge(data, loc, by="location_name", all.x=T)

#collapse on age and state to come up with a final dataset
data <- data[, list(pop=sum(pop), sex=sex_id), by="state_id,year,sex_id,age"]
data$sex_id<-NULL

#merge on nice age names
data <- merge(data, agenames, by="age", all=T)

#load acutal 'loc', merge on
loc<- fread(paste0(main_dir, "clean/loc.csv"))
data <- merge(data, loc[, list(state, state_id, developed)], by="state_id", all.x=T)

#new columns
data$national <- "National"
data[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]

#order columns
setcolorder(data, c("national", "dev_status", "developed", "state", "state_id", "year", "sex", "age", "agename", "pop"))
data <- data[order(state_id, year, sex, age)]

#set values to factor
data[, year:=as.factor(year)]
data[, age:=as.factor(age)]
pop<-copy(data)

#save
save(pop, file=paste0(main_dir, "clean/pop.rdata"))
