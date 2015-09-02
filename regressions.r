####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Run bivariate regressions on a number of different levels to get
## rough estimates of change over time.
####################################################################################

library(foreign)
library(data.table)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(RColorBrewer)
options(scipen=10)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/"

files <- c("causes", "means", "profession")
source(paste0(main_dir, "../code/plotting_fns.r"))
source(paste0(main_dir, "../code/multiplot.r"))
load(paste0(main_dir, "clean/pop.rdata"))
pop[, year:=as.factor(year)]
pop[, age:=as.factor(age)]
pop[, ag_status := ifelse(ag_state==1, "Agricultural", "Non-Agricultural")]
pop[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
pop <- pop[age!=0]

all <- pop[, list(data_type="all", national, dev_status, ag_status, state, year, sex, agename, classification="All", deaths)]

#reshape long, collapse on geographic level
pop <- melt(pop, id.vars=c("year", "sex", "agename", "pop"), 
             measure.vars=c("national", "dev_status", "ag_status", "state"),
             variable.name="geog_status", 
             value.name="geog_val")
pop <- pop[, list(pop=sum(pop)), by="geog_status,geog_val,year,sex,agename"]

#append all category-split files into one big dataset
data <- lapply(files, function(name){
  load(paste0(main_dir, "clean/", name, ".rdata"))
  typename <- ifelse(name=="causes", "reason", name)
  data[, data_type:= name]
  return(data)
})

data <- do.call(rbind, data)
data[, ag_status := ifelse(ag_state==1, "Agricultural", "Non-Agricultural")]
data <- data[age!=0, list(deaths=sum(deaths)), by="data_type,national,dev_status,ag_status,state,year,sex,agename,classification"]
data <- rbind(all, data)

#reshape long on geographic status
data <- melt(data, id.vars=c("data_type", "year", "sex", "agename", "classification", "deaths"), 
             measure.vars=c("national", "dev_status", "ag_status", "state"),
             variable.name="geog_status", 
             value.name="geog_val")
data <- data[, list(deaths=sum(deaths)), by="data_type,geog_status,geog_val,year,sex,agename,classification"]

#merge with population, calculate rates/proportions as necessary
rate_per <- 100000
data <- merge(data, pop, by=c("geog_status", "geog_val", "year", "sex", "agename"))
data <- data[, list(data_type, geog_status, geog_val, year, sex, agename, classification, deaths, pop)]
setkeyv(data, c("data_type", "geog_status"))
data[, year:= as.numeric(as.character(year))]
data[, yearcount:= year-2001] #center year

#every regression will be run with an indicator variable on geog_value and classification (if possible). We have to run four of these:
## 1. No other indicators
## 2. Indicator on sex
## 3. Indicator on age
## 4. Indicator of sex and age

base_byvals <- "yearcount,data_type,geog_status,geog_val,classification"
byval_list <- c("", ",sex", ",agename", ",sex,agename")
all_outputs <- NULL
regress_idx <- 1

#loop through data types
for (dtype in unique(data$data_type)){
#for (dtype in c("all")){
  print(paste("Running regression for", dtype))
  measure_type <- ifelse(dtype=="all", "rate", "proportion")
  base_formula <- ifelse(dtype=="all", "measure_val ~ yearcount", "measure_val ~ yearcount + classification")
  
  #loop through geographic types
  for(gtype in unique(data$geog_status)){
  #for (gtype in c("national")){
    print(gtype)
    if (gtype!="national") base_formula <- paste(base_formula, "+ geog_val")
    
    subset <- data[J(dtype, gtype)]
    
    #loop through 'byvals'
    for (idx in 1:4){
      byvals <- paste0(base_byvals, byval_list[[idx]])
      to_regress <- subset[, list(deaths=sum(deaths), pop=sum(pop)), by=byvals]
      
      if (measure_type=="rate"){
        to_regress[, measure_val:= (deaths/pop)*rate_per]
      }else{
        propvals <- paste0("yearcount,geog_val", byval_list[[idx]])
        to_regress[, sum_deaths:= sum(deaths), by=propvals]
        to_regress[, measure_val:= deaths/sum_deaths]
        to_regress[is.na(measure_val), measure_val:=0]
      }
      
      # run a separate regression for each combination of 'byvals', except year
      by_vector <- strsplit(byvals, split=",")[[1]][-1]
      unique_combos <- lapply(by_vector, function(byval){unique(to_regress[[byval]])})
      unique_combos <- data.table(expand.grid(unique_combos))
      setnames(unique_combos, by_vector)
      setkeyv(to_regress, by_vector)
      
      model_outputs <- lapply(as.numeric(rownames(unique_combos)), function(idx){
        out <- lm(measure_val~yearcount, data=to_regress[J(unique_combos[idx])])
      })
      #recover betas
      betas <- lapply(model_outputs, function(out){
        return(out$coefficients[["yearcount"]])
      })
      #recover standard errors
      sigmas <- lapply(model_outputs, function(out){
        return(summary(out)$coefficients[2,2])
      })
      
      unique_combos$year_beta <- unlist(betas)
      unique_combos$sigma <- unlist(sigmas)
      all_outputs[[regress_idx]] <- unique_combos
      regress_idx <- regress_idx + 1
    }
    
  }

}

#combine everything together into one big data.table
all_outputs <- do.call("rbind", c(all_outputs, fill=T))
all_outputs <- all_outputs[, list(data_type, geog_status, geog_val, sex, agename, classification, year_beta, sigma)]

#test for significance
new_alpha <- 0.05/nrow(all_outputs)
all_outputs[sigma<new_alpha & year_beta!=0]

regressions <- copy(all_outputs)

save(regressions, file=paste0(main_dir, "outputs/regressions.rdata"))

