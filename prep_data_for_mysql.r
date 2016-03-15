## convert .rdata files into appropriately named .csv's to be uploaded to mySQL server. The .rdata files are:
## means
## causes
## profession
## education
## pop
## additionally, I will make a new dataset called "states" containing state identifiers and info.

library(data.table)
library(plyr)

main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/"
in_dir <- paste0(main_dir, "clean/")
out_dir <- paste0(main_dir, "database/input_data/")

## first: import any dataset, save state-relevant columns

state_cols <- c("state_id", "state", "ag_state", "coastal", "developed")
state_newnames <- c("state_id", "name", "agricultural", "coastal", "developed")
load(paste0(in_dir, "means.rdata"))

states <- unique(data[, state_cols, with=F])
setnames(states, state_newnames)
write.csv(states, file=paste0(out_dir, "states.csv"), row.names=F)

## then: import and save the four main datasets (reason, means, profession, education)

columns <- c("state_id", "year", "sex", "age", "classification_label", "category_label", "deaths")
column_newnames <- c("state_id", "year", "sex", "age", "classification", "category", "deaths")

tables <- c("causes", "means", "profession", "education")

for (table in tables){
  print(table)
  load(paste0(in_dir, table, ".rdata"))
  if (table=="education"){
    columns <- columns[columns!="age"]
    column_newnames <- column_newnames[column_newnames!="age"]
  }
  new_table <- data[, columns, with=F]
  new_table[, sex:=mapvalues(sex, from=c("Males", "Females"), to=c(1,2))]
  setnames(new_table, column_newnames)
  out_name <- ifelse(table=="causes", "reasons", table)
  write.csv(new_table, file=paste0(out_dir, out_name, ".csv"), row.names = F)
}

#finally: do population
load(paste0(in_dir, "pop.rdata"))
pop_cols <- c("state_id", "year", "sex", "age", "pop")
pop <- pop[, pop_cols, with=F]
pop[, sex:=mapvalues(sex, from=c("Males", "Females"), to=c(1,2))]
write.csv(pop, file=paste0(out_dir, "population.csv"), row.names = F)

