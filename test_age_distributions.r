####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: See how much age distributions vary across state
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
pop[, ag_status := ifelse(ag_state==1, "Agricultural", "Non-Agricultural")]
pop[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
pop <- pop[age!=0]

#plot
ggplot(data=pop,aes(x=age,y=pop,fill=sex)) + 
  geom_bar(subset=.(sex=="Females"), stat="identity") + 
  geom_bar(subset=.(sex=="Males"),aes(y=pop*(-1)), stat="identity") + 
  facet_wrap(~state, scale="free_y") +
  #scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  #coord_flip()


