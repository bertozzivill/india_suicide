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

files <- c("causes", "means")
source(paste0(main_dir, "../code/plotting_fns.r"))

for (name in files){
  print(name)
  load(paste0(main_dir, "clean/", name, ".rdata"))
  data[, sex := factor(sex, labels=c("Males", "Females"))]
  data[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
  data[, national:="National"]
  data[, year:=as.factor(year)]
  data[, age:=as.factor(age)]
  data[, classification:=as.factor(classification)]
  
  #remove age=0 
  data <-data[age!=0]
  
  for (natval in c("national", "dev_status")){
    
    #deaths by classification for whole country over time (collapse over sex and age)
    print("plotting by classification")
    summed <- data[, list(deaths=sum(deaths)), by=eval(paste0(natval,",year,classification"))]
    summed[, summed_deaths:= sum(deaths), by=eval(paste0(natval,",year"))]
    summed[, prop:= ifelse(summed_deaths==0, 0,deaths/summed_deaths)]
    
    for(typeval in c("prop")){
      labelvar<-ifelse(typeval=="deaths", "Deaths", ifelse(typeval=="prop", "Proportion", "Mortality Rate"))
      pdf(file=paste0(main_dir, "plots/", name, "/", natval, "/", typeval, "_by_class_bar.pdf"), width=14, height=8)
      image  <- bar_plot(data=summed, 
                          yvar=typeval, 
                          fillvar="classification",
                          facet_str=paste0(".~", natval), 
                          title="Suicides Over Time",
                          ylabel=labelvar)
      print(image)
      dev.off()
    }
    
    #deaths by classification for whole country over time (collapse over age, but not sex)
    print("plotting by classification")
    summed <- data[, list(deaths=sum(deaths)), by=eval(paste0(natval,",year,classification,sex"))]
    summed[, summed_deaths:= sum(deaths), by=eval(paste0(natval,",year,sex"))]
    summed[, prop:= ifelse(summed_deaths==0, 0,deaths/summed_deaths)]
    
    for(typeval in c("prop")){
      labelvar<-ifelse(typeval=="deaths", "Deaths", ifelse(typeval=="prop", "Proportion", "Mortality Rate"))
      pdf(file=paste0(main_dir, "plots/", name, "/", natval, "/", typeval, "_by_class_sex_bar.pdf"), width=14, height=8)
      image  <- bar_plot(data=summed, 
                          yvar=typeval, 
                          fillvar="classification",
                          facet_str=paste0(natval, "~sex"), 
                          title="Suicides Over Time",
                          ylabel=labelvar)
      print(image)
      dev.off()
    }
    
    #deaths by age for whole country over time (by sex and classification)
    print("plotting by classification, sex, and age")
    summed <- data[, list(deaths=sum(deaths)), by=eval(paste0(natval,",year,classification,sex,age"))]
    summed[, summed_deaths:= sum(deaths), by=eval(paste0(natval,",year,age,sex"))]
    summed[, prop:= ifelse(summed_deaths==0, 0,deaths/summed_deaths)]
    setkeyv(summed, c("age", natval))
    
    for(typeval in c("prop")){
      labelvar<-ifelse(typeval=="deaths", "Deaths", ifelse(typeval=="prop", "Proportion", "Mortality Rate"))
      
      pdf(file=paste0(main_dir, "plots/", name, "/", natval ,"/", typeval, "_class_loop_age_bar.pdf"), width=14, height=8)
      for(ageval in unique(summed$age)){
        image  <- bar_plot(data=summed[J(ageval)], 
                            yvar=typeval, 
                            fillvar="classification",
                            facet_str=paste0(natval, "~sex"), 
                            title=paste("Deaths by", capitalize(name), ",", ageval),
                            ylabel=labelvar)
        print(image)
      }
      dev.off()
    }
    
    #deaths by classification for whole country over time (by sex and age)
    print("plotting by classification, sex, and age")
    summed <- data[, list(deaths=sum(deaths)), by=eval(paste0(natval,",year,classification,sex,age"))]
    summed[, summed_deaths:= sum(deaths), by=eval(paste0(natval,",year,sex,classification"))]
    summed[, prop:= ifelse(summed_deaths==0, 0,deaths/summed_deaths)]
    setkeyv(summed, "classification")
    
    for(typeval in c("prop")){
      labelvar<-ifelse(typeval=="deaths", "Deaths", ifelse(typeval=="prop", "Proportion", "Mortality Rate"))
      
      pdf(file=paste0(main_dir, "plots/", name, "/",natval, "/", typeval, "_age_loop_class_bar.pdf"), width=14, height=8)
      for(class in unique(summed$classification)){
        image  <- bar_plot(data=summed[J(class)], 
                            yvar=typeval, 
                            fillvar="age",
                            facet_str=paste0(natval, "~sex"), 
                            title=paste("Deaths by", capitalize(name), ",", class),
                            ylabel=labelvar)
        
        print(image)
      }
      dev.off()
    }
  }
}