####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Suicide data from the NCRB in India were formatted uniformly in the
## 'clean_data' script in this folder. Here, I make plots showcasing rates/proportions
## by cause and means side-by-side for a given age-sex.
####################################################################################

library(foreign)
library(data.table)
library(reshape2)
library(ggplot2)
library(Hmisc)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/"

files <- c("causes", "means", "profession")
source(paste0(main_dir, "../code/plotting_fns.r"))
source(paste0(main_dir, "../code/multiplot.r"))

load(paste0(main_dir, "clean/pop.rdata"))
pop <- pop[age!=0]

files <- c("causes", "means")

for (name in files){
  print(name)
  load(paste0(main_dir, "clean/", name, ".rdata"))
  #remove age=0 
  data <-data[age!=0]
  
  #collapse down to the age-sex-classification-year level, both with and without development status
  data[, geo_status:="National"]; data[, geo_type:="national"]; pop[, geo_status:="National"]; pop[, geo_type:="national"]
  national <- sumvars(data, pop, bysum="geo_status,geo_type,year,sex,agename,age,classification",
                      byprop="geo_status,geo_type,year,sex,agename,age",
                      byrate="geo_status,geo_type,year,sex,agename,age")
  data[, geo_status:=dev_status]; data[, geo_type:="dev_status"]; pop[, geo_status:=dev_status]; pop[, geo_type:="dev_status"]
  dev <- sumvars(data, pop, bysum="geo_status,geo_type,year,sex,agename,age,classification",
                 byprop="geo_status,geo_type,year,sex,agename,age",
                 byrate="geo_status,geo_type,year,sex,agename,age")
  
  summed <- rbind(national, dev)
  summed[, name:=capitalize(name)]
  data$geo_status <- NULL; data$geo_type <- NULL; pop$geo_status <- NULL; pop$geo_type <- NULL;
  
  #change names
  assign(name, data)
  assign(paste0(name, "_summed"), summed)
}
remove(data, summed, dev, national)
summed <- rbind(causes_summed, means_summed)
setkeyv(summed, c("geo_type", "sex", "agename", "name"))
remove(causes_summed, means_summed)

#create all possible values of age and sex to loop through
agesex_vals <- data.table(expand.grid(unique(summed$sex), unique(summed$agename)))
agesex_vals <- agesex_vals[order(Var1, Var2)]
agesex_strings <- do.call(paste, agesex_vals)

#begin plotting loops
for(geoval in c("national", "dev_status")){
  print(paste("working on geo_type", geoval))
  
  for(typeval in c("rate", "prop", "deaths")){
    print(typeval)
    labelvar<-ifelse(typeval=="deaths", "Death Counts", ifelse(typeval=="prop", "Death Proportion", "Mortality Rate"))
  
    pdf(paste0(main_dir, "plots/cause_means_juxta/", geoval, "/", typeval, ".pdf"), width=14, height=8)
  
    for (agesex_idx in 1:nrow(agesex_vals)){
      agesex_title <- agesex_strings[[agesex_idx]]
      print(agesex_title)
      sexval<- agesex_vals[agesex_idx]$Var1
      ageval <- agesex_vals[agesex_idx]$Var2
    
      for (nameval in unique(summed$name)){
        print(nameval)
        colors <- ifelse(nameval=="Causes", "Set2", "Dark2")
        #bar plot for props
        if (typeval=="prop"){
          image <- bar_plot(data=summed[J(geoval, sexval, ageval, nameval)],
                            yvar=typeval,
                            fillvar="classification",
                            facet_str="geo_status~name",
                            pal <- colors,
                            title=paste(labelvar, agesex_title),
                            ylabel=labelvar)
          
          assign(paste0(nameval, "_image"), image)
        }else{
          image <- line_plot(data=summed[J(geoval, sexval, ageval, nameval)],
                             yvar=typeval,
                             groupvar="classification",
                             facet_str="geo_status~name",
                             pal<- colors,
                             title=paste(labelvar, agesex_title),
                             ylabel=labelvar)
          assign(paste0(nameval, "_image"), image)
        }
      }
      multiplot(Causes_image, Means_image, cols=2)
      
    }
    dev.off()
  }
}

