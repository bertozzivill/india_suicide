####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Suicide data from the NCRB in India were collected and cleaned by
## a former PHFI employee. I make some formatting changes for ease of plotting and 
## analysis. 

##The datasets are as follows (all datasets are by year and state):
## causes: deaths by cause (i.e. reason for suicide), sex, and age group (0-14, 15-29, 30-44, 45-59, 60+)
## means: deaths by means (i.e. method of suicide), sex, and age group
## profession: deaths by profession, sex, and age group
## education: deaths by education and sex
## social_status: deaths by marital status and sex.

## Sex (and age, when available) are wide in all of these datasets-- we change the
## formatting so age and sex are long.

## Additionally, we reformat the categories in each dataset ('cause', 'means', etc.)
## for both easier analysis and more beautiful plot labels.

## We save five separate .rdata files, one for each dataset-- since each of the 
## datasets is a different tabluation, they cannot be easily merged.

####################################################################################

library(foreign)
library(data.table)
library(reshape2)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/"

files <- c("causes", "education", "means", "profession", "social_status")
shared_colnames <- c("state", "year",  "category_label", "category", "classification_label", "classification")

loc<- fread(paste0(main_dir, "plots/shapefiles/loc.csv"))
agenames <- fread(paste0(main_dir, "agenames.csv"))

alldata <- lapply(files, function(name){
  print(name)
  import <- data.table(read.spss(paste0(main_dir, "raw/", name, ".sav"), to.data.frame=T))
  #remove trailing whitespace from state names
  import[, state:= gsub("\\s+$", "", state)]
  #merge on cleaned category names
  cat_names <- fread(paste0(main_dir, "raw/", name, "_convert.csv"))
  import <- merge(import, cat_names, by=intersect(names(import), names(cat_names)), all=T)
  import[[names(cat_names)[[1]]]] <- NULL #drop original, messy column name
  
  #drop state_code and ID, they're irrelevant at the moment and mess up the reshape
  import$state_code<-NULL; import$ID<-NULL;
  
  if (name %in% c("education", "social_status")){
    import <- melt(import, id.vars=c(shared_colnames), variable.name="sex", value.name="deaths")
    import <- import[sex!="total"]
    #briefly create a null 'age' column for consistency with the other dataset
    import[,age:=NA]; import[,agename:=NA]
  }else{
    #reshape age/sex long
    import <- melt(import, id.vars=c(shared_colnames), variable.name="agesex", value.name="deaths")
    import[, agesex:=gsub("_", "", tolower(agesex))]
    #drop the summation variables for agesex
    import <- import[!agesex %in% c("femaletotal", "maletotal", "totalmalefemale")]
    #extract sex 
    import[, sex:=gsub("(female|male)([A-Za-z0-9_]*)", "\\1", agesex)]
    import[, age:=gsub("(female|male)([A-Za-z0-9_]*)", "\\2", agesex)]
    # age will now be a string that either begins with the first age of the age group, or 
    # (in the case of 'upto14years') begins with the word 'up'. We convert all age groups to
    # numerics indicating the first year of the age group.
    import[, age:= ifelse(substr(age, 1,2)=="up", 0, as.numeric(substr(age, 1,2)))]
    import$agesex <-NULL
    
    #merge on nicer names
    import<-merge(import, agenames, by="age", all=T)
  }
  
  #for now, allocate zeros to those missings in kerala in 2006
  import[is.na(deaths), deaths:=0]
  
  #merge on loc-- this gives information on state_id (for mapping), development status, and state/union territory status (drop union territories)
  import <- merge(import, loc, by="state", all=T)
  import <- import[union_territory!=1]; import$union_territory <- NULL; #drop union territories
  
  #rename and order properly
  import[, sex:= ifelse(sex=="male", 1, 2)]
  import[, national:="National"]
  import[, dev_status:= ifelse(developed==1, "More Developed", "Less Developed")]
  setcolorder(import, c("national", "dev_status", "developed", "state", "state_id", "year", "sex", "age", "agename", "classification", "classification_label", "category", "category_label", "deaths"))
  import<-import[order(state, year, sex, age, classification, category)]
  
  #convert to factor
  import[, sex := factor(sex, labels=c("Males", "Females"))]
  import[, year:=as.factor(year)]
  import[, age:=as.factor(age)]
  import[, classification:=as.factor(classification)]
  
  #drop age if necessary
  if (name %in% c("social_status", "education")){
    import$age<-NULL; import$agename<-NULL
  } 
  
  #extract "total" category
  total <- import[category=="total"]
  if (name != "social_status") import <- import[category_label!="total"]
  
  #save
  data <- copy(import)
  save(data, file=paste0(main_dir, "clean/", name, ".rdata"))
  save(total, file=paste0(main_dir, "clean/", name, "_total.rdata"))
  return(import)
})

causes <- alldata[[1]]
education <- alldata[[2]]
means <- alldata[[3]]
profession <- alldata[[4]]
social_status <- alldata[[5]]



