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

files <- c("causes", "means", "profession", "education")

for (name in files){
  print(name)
  load(paste0(main_dir, "clean/", name, ".rdata"))
  data[, sex := factor(sex, labels=c("Males", "Females"))]
  data[is.na(deaths), deaths:=0] #set nulls in kerala in 2006 to zero, for now
  
   #deaths by classification for each state over time (collapse over sex and age)
  print("plotting by classification")
  summed <- data[, list(deaths=sum(deaths)), by="state,year,classification"]
  setkeyv(summed, "state")
  
  pdf(file=paste0(main_dir, "plots/", name, "/deaths_by_state.pdf"), width=14, height=8)
  for(statename in unique(summed$state)){
  image <- ggplot(summed[J(statename)], aes(x=factor(year), y=deaths, group=classification)) +
                 geom_line(aes(color=classification), size=3) +
                 guides(fill=guide_colourbar(title=capitalize(name), barheight=20)) +
                 labs(title=paste("Deaths by", capitalize(name), ",", statename),
                      x="Year",
                      y="Deaths")
  print(image)
  }
  dev.off()
  
  #deaths by classification and sex for each state over time (collapse over age)
  print("plotting by classification and sex")
  summed <- data[, list(deaths=sum(deaths)), by="state,year,classification,sex"]
  setkeyv(summed, "state")
  
  pdf(file=paste0(main_dir, "plots/", name, "/deaths_by_state_sex.pdf"), width=14, height=8)
  for(statename in unique(summed$state)){
    image <- ggplot(summed[J(statename)], aes(x=factor(year), y=deaths, group=classification)) +
      geom_line(aes(color=classification), size=3) +
      facet_grid(.~sex) + 
      guides(fill=guide_colourbar(title=capitalize(name), barheight=20)) +
      labs(title=paste("Deaths by", capitalize(name), ",", statename),
           x="Year",
           y="Deaths")
    print(image)
  }
  dev.off()
  
  
  #deaths by age and sex for each state over time (only need to run this once)
  if (name=="causes"){
    summed <- data[, list(deaths=sum(deaths)), by="state,year,sex,age"]
    setkeyv(summed, "state")
    print("plotting by age and sex")
    
    pdf(file=paste0(main_dir, "plots/deaths_by_sex_age.pdf"), width=14, height=8)
    for(statename in unique(summed$state)){
      image <- ggplot(summed[J(statename)], aes(x=factor(year), y=deaths, group=factor(age))) +
        geom_line(aes(color=factor(age)), size=3) +
        facet_grid(.~sex) +
        guides(fill=guide_colourbar(title="Age:", barheight=20)) +
        labs(title=paste("Deaths by Age and Sex,", statename),
             x="Year",
             y="Deaths")
      print(image)
    }
    dev.off()
  }
  
  
}