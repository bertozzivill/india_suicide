####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Prep data on suicides by age and sex for 2011-2014 to include in
##              the summary analysis
####################################################################################

library(foreign)
library(data.table)
library(reshape2)
library(R.utils)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/"

#load population
load(paste0(main_dir, "clean/pop.rdata"))

#load raw 2011-24 data
data <- fread(paste0(main_dir, "raw/summary_2011_2014_semi_clean.csv"), na.strings="-")

#remove commas from numbers >1000, convert to numeric
for (col in names(data)[3:length(names(data))]){
  data[[col]] <- as.numeric(gsub(",", "", data[[col]]))
}

#replace na's
data[is.na(data)] <- 0

#find deaths, age zero
data[, male_0:= male_all - (male_15+male_30+male_45+male_60)]
data[, female_0:= female_all - (female_15+female_30+female_45+female_60)]
data[, c("male_all", "female_all") :=NULL, with=F]

#reshape long on sex and age, split
data <- melt(data, id.vars=c("state", "year"), value.name = "deaths")
data[, sex:= capitalize(paste0(gsub("_[0-9]*", "", variable), "s"))]
data[, age:= as.integer(gsub("[a-z]*_", "", variable))]
data[, variable:=NULL]

#format state names: 
# -- capitalize normally
# fix typo in Arunachal Pradesh
# remove "UT" from Delhi

#handy capitalization function
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


data[state=="ARUNACHALPRADESH", state:="ARUNACHAL PRADESH"]
data[, state:= capitalize(tolower(state))]
data$state <- sapply(data$state, simpleCap)

data[state=="Delhi (ut)", state:="Delhi"]

# add Telangana back into Andhra Pradesh 
data[state=="Telangana", state:="Andhra Pradesh"]
data <- data[, list(deaths=sum(deaths)), by=c("year", "state", "sex", "age")]

#merge on population
pop[, year:=as.numeric(as.character(year))]
pop[, age:=as.integer(as.character(age))]
pop[, sex:= as.character(sex)]
pop <- pop[year %in% 2011:2014]

new_data <- merge(pop, data, by=c("year", "state", "sex", "age"), all.x=T)

#save
save(new_data, file=paste0(main_dir, "clean/deaths_2011_2014_temp.rdata"))

