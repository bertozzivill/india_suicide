####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Suicide data from the NCRB in India were formatted uniformly in the
## 'clean_data' script in this folder. Here, I make preliminary plots for 
## each of the cleaned datasets. 
####################################################################################

library(foreign)
library(data.table)
library(reshape2)
library(ggplot2)
library(Hmisc)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Desktop/practicum/suicide/data/"
source(paste0(main_dir, "../code/line_plot.r"))

files <- c("causes", "means")

for (name in files){
  print(name)
  load(paste0(main_dir, "clean/", name, ".rdata"))
  data[, sex := factor(sex, labels=c("Males", "Females"))]
  data[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
  data[, year:=as.factor(year)]
  data[, age:=as.factor(age)]
  
  #deaths by age for whole country over time (by sex and classification)
  print("plotting by classification, sex, and age")
  summed <- data[, list(deaths=sum(deaths)), by="year,dev_status,classification,sex,age"]
  summed[, summed_deaths:= sum(deaths), by="year,dev_status,age,sex"]
  summed[, prop:= ifelse(summed_deaths==0, 0,deaths/summed_deaths)]
  setkeyv(summed, c("age", "dev_status"))
  
  pdf(file=paste0(main_dir, "plots/", name, "/dev_status/counts_class_loop_age.pdf"), width=14, height=8)
  for(ageval in unique(summed$age)){
    image <- ggplot(summed[J(ageval)], aes(x=factor(year), y=deaths/1000, group=classification)) +
      geom_line(aes(color=classification), size=3) +
      facet_grid(sex~dev_status, scales="free") +
      guides(fill=guide_colourbar(title=capitalize(name), barheight=20)) +
      labs(title=paste("National Deaths by", capitalize(name), ",", ageval),
           x="Year",
           y="Deaths, Thousands")
    print(image)
  }
  dev.off()
  
  pdf(file=paste0(main_dir, "plots/", name, "/dev_status/props_class_loop_age.pdf"), width=14, height=8)
  for(ageval in unique(summed$age)){
    image <- ggplot(summed[J(ageval)], aes(x=factor(year), y=prop, group=classification)) +
      geom_line(aes(color=classification), size=3) +
      facet_grid(sex~dev_status) +
      guides(fill=guide_colourbar(title=capitalize(name), barheight=20)) +
      labs(title=paste("National Deaths by", capitalize(name), ",", ageval),
           x="Year",
           y="Proportion")
    print(image)
  }
  dev.off()
  
  #deaths by classification for whole country over time (by sex and age)
  print("plotting by classification, sex, and age")
  summed <- data[, list(deaths=sum(deaths)), by="year,dev_status,classification,sex,age"]
  summed[, summed_deaths:= sum(deaths), by="year,dev_status,sex,classification"]
  summed[, prop:= ifelse(summed_deaths==0, 0,deaths/summed_deaths)]
  setkeyv(summed, c("classification", "dev_status"))
  
  pdf(file=paste0(main_dir, "plots/", name, "/dev_status/counts_age_loop_class.pdf"), width=14, height=8)
  for(class in unique(summed$classification)){
    image <- ggplot(summed[J(class)], aes(x=factor(year), y=deaths/1000, group=factor(age))) +
      geom_line(aes(color=factor(age)), size=3) +
      facet_grid(sex~dev_status, scales="free") +
      guides(fill=guide_colourbar(title=capitalize(name), barheight=20)) +
      labs(title=paste("National Deaths by", capitalize(name), ",", class),
           x="Year",
           y="Deaths, Thousands")
    print(image)
  }
  dev.off()
  
  pdf(file=paste0(main_dir, "plots/", name, "/dev_status/props_age_loop_class.pdf"), width=14, height=8)
  for(class in unique(summed$classification)){
    image <- ggplot(summed[J(class)], aes_string(x="year", y="prop", group="age")) +
      geom_line(aes(color=factor(age)), size=3) +
      facet_grid(sex~dev_status) +
      scale_colour_discrete(name="Age") +
      labs(title=paste("National Deaths by", capitalize(name), ",", class),
           x="Year",
           y="Proportion")
    print(image)
  }
  dev.off()
  
}