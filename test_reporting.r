####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Take a look at the assumption that change in mortality rate are due
## solely to changes in reporting. See how many excess/deficit deaths there would
## be if you extended the 2001 mortality rate through the entire time series
####################################################################################

library(foreign)
library(data.table)
library(reshape2)
library(ggplot2)
library(Hmisc)
options(scipen=10)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/"

files <- c("causes", "means", "profession")
source(paste0(main_dir, "../code/plotting_fns.r"))
source(paste0(main_dir, "../code/multiplot.r"))
load(paste0(main_dir, "clean/pop.rdata"))
pop[, year:=as.factor(year)]
pop[, age:=as.factor(age)]
pop[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
pop <- pop[age!=0]


###########################################################
## Plotting
#########################################################

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

colors <- gg_color_hue(n=2)

rate_per <- 100000


for (natval in c("national", "dev_status", "state")){
  print(natval)
  bylist <- paste0("year,", natval)
  byvec <- unlist(strsplit(bylist, split=","))
  
  #collapse to national level, plot differences
  summed <- pop[, list(pop=sum(pop), deaths=sum(deaths)), by=bylist]
  summed[, rate:= (deaths/pop)*rate_per]
  first_year<- summed[year==2001]; first_year[, c("year", "pop", "deaths"):=NULL]; setnames(first_year, "rate", "initial_rate")
  summed <- merge(summed, first_year, by=intersect(names(summed), names(first_year)))
  summed[, samerate_deaths:= (initial_rate/rate_per)*pop]
  summed[, death_diff:= deaths-samerate_deaths]
  
  reshaped <- melt(summed, id.vars=byvec, measure.vars=c("deaths", "rate", "samerate_deaths", "initial_rate"))
  reshaped[, type:= ifelse(variable %in% c("deaths", "rate"), "Actual", "Constant")]
  reshaped[,variable:=as.character(variable)]
  reshaped[, variable:=ifelse(variable %in% c("deaths", "rate"), capitalize(variable), capitalize(gsub("[a-z]*_", "", variable)))]
  
  summed[, years_since_2000:=as.factor(as.numeric(as.character(year))-2000)]
  reshaped[, years_since_2000:=as.factor(as.numeric(as.character(year))-2000)]
  
  pdf(file=paste0(main_dir, "plots/reporting_test/", natval, ".pdf"), width=14, height=8)
  
  scales <- ifelse(natval=="state", "free_y", "fixed")
  xval <- ifelse(natval=="state", "years_since_2000", "year")
  xlab <- ifelse(natval=="state", "Years Since 2000", "Year")
  
  rate_view <- ggplot(reshaped[variable=="Rate"], aes_string(x=xval, y="value", group="type")) +
    geom_line(aes(color=type), size=2) +
    facet_wrap(as.formula(paste0("~", natval)), scales=scales) +
    labs(title= "Actual vs Constant Mortality Rate",
         x=xlab,
         y="Rate per 100,000")
  
  deaths_view <- ggplot(reshaped[variable=="Deaths"], aes_string(x=xval, y="value", group="type")) +
    geom_line(aes(color=type), size=2) +
    facet_wrap(as.formula(paste0("~", natval)), scales=scales) +
    labs(title= "Actual vs Constant Death Count",
         x=xlab,
         y="Deaths")

  diff_view <- ggplot(summed, aes_string(x=xval, group="1")) +
    geom_line(aes(y=death_diff), size=2, color=colors[[1]]) +
    geom_line(aes(y=0), size=2, color=colors[[2]]) +
    facet_wrap(as.formula(paste0("~", natval)), scales=scales) +
    labs(title= "Difference in Deaths, Actual-Constant",
         x=xlab,
         y="Deaths")
  
  print(rate_view); print(deaths_view); print(diff_view)
  
  dev.off()
  
}





