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
library(grDevices)
library(RColorBrewer)
options(scipen=10)


rm(list=ls())
main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/"

#prep and format shapefils
rbPal <- colorRampPalette(c("red", "gold","dark green"))
redgreencolors <- rbPal(7)
shapefile_dir <- paste0(main_dir, "plots/shapefiles/")
load(paste0(shapefile_dir, "prepped_shapefile.rdata"))
india_map <- data.table(fortify(india_map))
india_map[, state_id := as.numeric(id)]

#read in data
files <- c("causes", "means", "profession")
source(paste0(main_dir, "../code/plotting_fns.r"))
source(paste0(main_dir, "../code/multiplot.r"))
load(paste0(main_dir, "clean/pop.rdata"))
pop[, year:=as.factor(year)]
pop[, age:=as.factor(age)]
pop[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
pop <- pop[age!=0]


###########################################################
## Plots on the year-sex-age level, NOT by classification
#########################################################
rate_per <- 100000
level_str <- ""

mylist.names <- c("deaths_line", "deaths_map", "rate_line", "rate_map" )
mylist <- vector("list", length(mylist.names))
names(mylist) <- mylist.names
idx <- 1

pal <- gg_color_hue(n=2)

for (level in c("national", "sex", "agename")){
  print(level)
  level_str<- paste0(level_str, ",", level)
  groupvar <- ifelse(level=="national", "1", "sex") 
  level <- ifelse(level=="sex", "national", level)
  
  summed <- pop[, list(deaths=sum(deaths), pop=sum(pop)), by=eval(paste0("year,state,state_id", level_str))]
  summed[, rate:=(deaths/pop)*rate_per]
  summed[, year:=as.numeric(as.character(year))]
  setkeyv(summed, level)
  
  groups <- unique(summed[[level]])
  if (level=="sex") groups <- c("National")
  
  for (groupname in groups){
    print(groupname)
    for (typeval in c("deaths", "rate")){
      print(typeval)
      labelvar <- ifelse(typeval=="deaths", "Deaths", paste0("Mortality Rate Per ", rate_per))
      
      #line plot
      print("line")
      image <- ggplot(summed[J(groupname)], aes_string(x="year", y=typeval, color=groupvar)) +
                geom_line(size=2) + 
                facet_wrap(~state, scales="free_y")+
                stat_smooth(method="lm") +
                guides(color=F)+
                scale_x_continuous(breaks=c(2001, 2006, 2010), minor_breaks=c(2002,2003,2004,2005,2007,2008,2009)) +
                labs(title = paste("Suicide", labelvar, "over Time,", groupname),
                     x="Year",
                     y=labelvar)
              
      
      #if (groupvar=="1") image <- image + guides(color=FALSE)
        
      #print(image)
      
      mylist[[paste0(typeval, "_line")]][[idx]] <- image
      
      #map
      print("map")
      mapdata <- merge(summed[J(groupname)], india_map, by="state_id", allow.cartesian=T)
      mapdata <- mapdata[year %in% seq(2001, 2009, 2)]
      mapdata <- mapdata[state!="Sikkim"]
      setkeyv(mapdata, level)
      facet_str <- ifelse(groupvar=="sex", "sex~year", "~year")
      image <- ggplot(mapdata[J(groupname)]) +
              geom_polygon(aes_string(x="long", y="lat", group="group", fill=typeval)) +
              facet_grid(as.formula(facet_str)) +
              scale_fill_gradientn(colours=rev(redgreencolors)) +
              scale_x_continuous("", breaks=NULL) +
              scale_y_continuous("", breaks=NULL) +
              coord_fixed(ratio=1) +
              guides(fill=guide_colourbar(title="", barheight=20)) +
              labs(title = paste("Suicide", labelvar, ",", groupname)) +
              theme_bw(base_size=20)
      
      mylist[[paste0(typeval, "_map")]][[idx]] <- image
      
      idx <- idx+1
    }
  }
  
} 
graphics.off()

for (myname in mylist.names){
  print(myname)
  pdf(file=paste0(main_dir, "plots/summary/state_", myname, ".pdf"), width=14, height=8)
  for (myplot in mylist[[myname]]){
    print(myplot)
  }
  graphics.off()
}


#plot showing national-level pop, deaths, and rates side-by-side
#need to multiplot to show zero in scale of deaths :/
summed <- pop[, list(Deaths=sum(deaths), Population=sum(pop)), by="year"]
summed[, Rate:=(Deaths/Population)*rate_per]
pdf(file=paste0(main_dir, "plots/summary/death_pop_rate.pdf"), width=14, height=8)
allplots <- NULL
idx <- 1
for (level in c("Deaths", "Population", "Rate")){
  image <- ggplot(summed, aes_string(x="year", y=level, group="1")) +
    geom_line(size=2)+
    scale_y_continuous(limits=c(0, max(summed[[level]])))
  allplots[[idx]] <- image
  idx <- idx+1
}
multiplot(plotlist=allplots, cols=3)
dev.off()

#compare my rats to Sandeep's
sandeep <- fread(paste0(main_dir, "raw/alt_pop/sandeep_rates.csv"))
sandeep[, year:=as.factor(Year)]
sandeep$Year <- NULL

summed <- merge(summed, sandeep, by="year", all=T)
melted <- melt(summed, id.vars="year")
melted[variable=="sandeep_rate", variable:= "Sandeep Rate"]
melted[variable=="Rate", variable:= "Amelia Rate"]
melted <- melted[!variable %in% c("Deaths", "Population")]

pdf(paste0(main_dir, "plots/summary/sandeep_rate.pdf"), width=14, height=8)
image<- ggplot(melted, aes(x=year, y=value, group=variable)) +
  geom_line(aes(color=variable),size=2) + 
  labs(title="Mortality Rate, Amelia vs Sandeep",
       x="Year",
       y="Rate per 100,000")
print(image)
dev.off()


##################################################################

###########################################################
## Plots on the year-sex-age level, by classification
#########################################################
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
    summed <- sumvars(data, pop, bysum=paste0(natval,",year,classification"), byprop=paste0(natval,",year"), byrate=paste0(natval,",year"), rate_per=rate_per)
    
    for(typeval in c("deaths", "prop")){
      labelvar<-ifelse(typeval=="deaths", "Deaths", ifelse(typeval=="prop", "Proportion", "Mortality Rate"))
      pdf(file=paste0(main_dir, "plots/", name, "/", natval, "/", typeval, "_by_class.pdf"), width=14, height=8)
      image  <- line_plot(data=summed, 
                          yvar=typeval, 
                          groupvar="classification",
                          facet_str=paste0(".~", natval), 
                          title="Suicides Over Time",
                          ylabel=labelvar)
      print(image)
      dev.off()
    }
    
    image + stat_smooth(method="lm")
    
    #change in proportions over time: simple regression
    summed[, year:=as.numeric(as.character(year))]
    regress <- lm(prop ~ year + classification, data=summed)
    fes <- regress$coefficients
    #note to self: do this by state
    
    #deaths by classification for whole country over time (collapse over age, but not sex)
    print("plotting by classification")
    summed <- sumvars(data, pop, bysum=paste0(natval,",year,classification,sex"), byprop=paste0(natval,",year,sex"), byrate=paste0(natval,",year"), rate_per=rate_per)
    
    for(typeval in c("deaths", "prop")){
      labelvar<-ifelse(typeval=="deaths", "Deaths", ifelse(typeval=="prop", "Proportion", "Mortality Rate"))
      pdf(file=paste0(main_dir, "plots/", name, "/", natval, "/", typeval, "_by_class_sex.pdf"), width=14, height=8)
      image  <- line_plot(data=summed, 
                          yvar=typeval, 
                          groupvar="classification",
                          facet_str=paste0(natval, "~sex"), 
                          title="Suicides Over Time",
                          ylabel=labelvar)
      print(image)
      dev.off()
    }
    
    #deaths by age for whole country over time (by sex and classification)
    print("plotting by classification, sex, and age")
    summed <- sumvars(data, pop, bysum=paste0(natval,",year,classification,sex,age"), byprop=paste0(natval,",year,age,sex"), byrate=paste0(natval,",year,sex,age"), rate_per=rate_per)
    setkeyv(summed, c("age", natval))
    
    for(typeval in c("deaths", "prop")){
      labelvar<-ifelse(typeval=="deaths", "Deaths", ifelse(typeval=="prop", "Proportion", "Mortality Rate"))
      
      pdf(file=paste0(main_dir, "plots/", name, "/", natval ,"/", typeval, "_class_loop_age.pdf"), width=14, height=8)
      for(ageval in unique(summed$age)){
        image  <- line_plot(data=summed[J(ageval)], 
                            yvar=typeval, 
                            groupvar="classification",
                            facet_str=paste0(natval, "~sex"), 
                            title=paste("Deaths by", capitalize(name), ",", ageval),
                            ylabel=labelvar)
        print(image)
      }
      dev.off()
    }
    
    #deaths by classification for whole country over time (by sex and age)
    print("plotting by classification, sex, and age")
    summed <- sumvars(data, pop, bysum=paste0(natval,",year,classification,sex,age"), byprop=paste0(natval,",year,sex,classification"), byrate=paste0(natval,",year,sex,age"), rate_per=rate_per)
    setkeyv(summed, "classification")
    
    for(typeval in c("deaths", "prop")){
      labelvar<-ifelse(typeval=="deaths", "Deaths", ifelse(typeval=="prop", "Proportion", "Mortality Rate"))
      
      pdf(file=paste0(main_dir, "plots/", name, "/",natval, "/", typeval, "_age_loop_class.pdf"), width=14, height=8)
      for(class in unique(summed$classification)){
        image  <- line_plot(data=summed[J(class)], 
                            yvar=typeval, 
                            groupvar="age",
                            facet_str=paste0(natval, "~sex"), 
                            title=paste("Deaths by", capitalize(name), ",", class),
                            ylabel=labelvar)
        
        print(image)
      }
      dev.off()
    }
  }
}