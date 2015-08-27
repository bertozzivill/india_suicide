####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Determine what proportion of suicides were by farmers, and see 
## how that proportion changes over time and by agricultural/nonagricultural state
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

name <- "profession"
rate_per <- 100000


load(paste0(main_dir, "clean/", name, ".rdata"))
data[, sex := factor(sex, labels=c("Males", "Females"))]
data[, ag_status := ifelse(ag_state==1, "Agricultural", "Non-Agricultural")]
data[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
data[, national:="National"]
data[, year:=as.factor(year)]
data[, age:=as.factor(age)]
data[, classification:=as.factor(classification)]

#remove age=0 
data <-data[age!=0]

#collapse
summed <- sumvars(data, pop, bysum=paste0("ag_status,year,classification,sex"), byprop=paste0("ag_status,year,sex"), byrate=paste0("ag_status,year,sex"), rate_per=rate_per)
summed <- summed[classification=="SE: Farm/Agriculture"]

pdf(file=paste0(main_dir, "plots/agriculture/prop_change.pdf"), width=14, height=8)

image <- ggplot(summed, aes(x=year, y=prop, group=ag_status, color=ag_status))+
          geom_line(size=2) +
          stat_smooth(method="lm") +
          #scale_y_continuous(limits=c(0,0.2)) +
          facet_grid(.~sex ) +
          theme(legend.position="bottom", legend.title=element_blank()) +
          labs( title= "Farmer Suicides as a Proportion of All Suicides, by Agricultural Status",
                x="Year",
                y="Proporiton")

print(image)
dev.off()
