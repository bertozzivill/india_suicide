####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Generate data and plot specifically for each figure of the paper. 
####################################################################################

library(foreign)
library(data.table)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(RColorBrewer)
options(scipen=10)

rm(list=ls())
main_dir <- "C:/Users/abertozz/Documents/work/repos/india_suicide/data/"

files <- c("causes", "means", "profession")
source(paste0(main_dir, "../code/plotting_fns.r"))
source(paste0(main_dir, "../code/multiplot.r"))

load(paste0(main_dir, "outputs/all_deaths.rdata"))

data[, sex:=factor(sex, labels=c("Male", "Female"))]
data[geog_status=="ag_status", geog_val:= ifelse(geog_val=="Agricultural", "More Agricultural", "Less Agricultural")]
data[!geog_status %in% c("national", "state"), geog_val:= paste(geog_val, "States")]

rate_per <- 100000
linesize <- 1
shapesize <- 2.5

pdf(paste0(main_dir, "../writing_and_papers/paper/figures/paper_figs.pdf"), width=14, height=8)

#####################################################
## Figure 1: Rates per 100,000, faceted by geography,
##          colored by sex
######################################################
all_rates <- data[data_type=="all"& geog_status!="state", list(deaths=sum(deaths), pop=sum(pop)), by="geog_status,geog_val,year,sex"]

#add in 2011-14 data
load(paste0(main_dir, "clean/deaths_2011_2014_temp.rdata"))
loc <- fread(paste0(main_dir, "clean/loc.csv"))
new_data <- merge(new_data, loc[, list(state, ag_state)], by="state", all.x=T)
new_data[, ag_status:=ifelse(ag_state==1, "More Agricultural", "Less Agricultural")]

new_data <- melt(new_data, id.vars=c("year", "sex", "deaths", "pop"), 
            measure.vars=c("national", "dev_status", "ag_status", "state"),
            variable.name="geog_status", 
            value.name="geog_val")

new_data <- new_data[, list(deaths=sum(deaths), pop=sum(pop)), by="geog_status,geog_val,year,sex"]
new_data[!geog_status %in% c("national", "state"), geog_val:= paste(geog_val, "States")]
new_data[, sex:= gsub("s", "", sex)]

#all_rates <- rbind(all_rates, new_data[geog_status!="state"])

#calculate rates
all_rates[, rate:=(deaths/pop)*rate_per]
all_rates$geog_val <- factor(all_rates$geog_val, levels=c("National", "Less Developed States", "More Developed States",
                                                          "Less Agricultural States", "More Agricultural States"))

all_rates_figure <- ggplot(all_rates, aes(x=year, y=rate, group=sex))+
            geom_line(size=linesize) +
            geom_point(aes(shape=sex), size=shapesize)+
            facet_grid(~geog_val) +
            #scale_x_continuous(breaks=c(seq(2001, 2015, 2), 2014), minor_breaks=seq(2002,2014,2)) +
            scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
            stat_smooth(method="lm", se=F, color="black", linetype=3) +
            theme(legend.title=element_blank(), 
                  axis.text.x=element_text(angle=45, hjust=1))+
            labs(title= "Figure 1",
                 x="Year",
                 y="Rate per 100,000 population")

print(all_rates_figure)

#regress
regress_all_rates <- data.table(geog_val=character(),
                                beta=numeric())
all_rates[,year_int:= year-2000]
for (geog in unique(all_rates$geog_val)){
  print(paste("regressing for", geog))
    subset <- all_rates[geog_val==geog]
    subset <- subset[, list(deaths=sum(deaths), pop=sum(pop)), by=c("year", "year_int")]
    subset[, rate:=deaths/pop * rate_per]
    out <- lm(rate~year_int, data=subset)
    beta <- out$coefficients[["year_int"]]
    regress_all_rates <- rbind(regress_all_rates, list(geog, beta))
}

regress_by_sex <- data.table(geog_val=character(),
                                sex=character(),
                                beta=numeric())
for (geog in unique(all_rates$geog_val)){
  print(paste("regressing for", geog))
  for (sexval in unique(all_rates$sex)){
    subset <- all_rates[geog_val==geog & sex==sexval]
    out <- lm(rate~year_int, data=subset)
    beta <- out$coefficients[["year_int"]]
    regress_by_sex <- rbind(regress_by_sex, list(geog, sexval, beta))
  }
}

#####################################################
## Figure S1: Rates per 100,000, faceted by state
######################################################

# state_rates <- data[data_type=="all"& geog_status=="state", list(deaths=sum(deaths), pop=sum(pop)), by="geog_status,geog_val,year,sex"]
# #state_rates <- rbind(state_rates, new_data[geog_status=="state", list(deaths=sum(deaths), pop=sum(pop)), by="geog_status,geog_val,year,sex"])
# 
# state_rates[, rate:=(deaths/pop)*rate_per]
# 
# state_rates_figure <- ggplot(state_rates, aes(x=year, y=rate, group=sex)) +
#   geom_line(aes(color=sex),size=linesize) +
#   facet_wrap(~geog_val, scales="free_y") +
#   scale_x_continuous(breaks=c(2001, 2005, 2010, 2014), minor_breaks=2001:2014) +
#   stat_smooth(method="lm", se=F, color="black") +
#   theme(legend.title=element_blank())+
#   labs(title= "Figure 2",
#        x="Year",
#        y="Rate per 100,000 population")

state_rates <- data[data_type=="all"& geog_status=="state", list(deaths=sum(deaths), pop=sum(pop)), by="geog_status,geog_val,year"]
#state_rates <- rbind(state_rates, new_data[geog_status=="state", list(deaths=sum(deaths), pop=sum(pop)), by="geog_status,geog_val,year"])

state_rates[, rate:=(deaths/pop)*rate_per]

state_rates_figure <- ggplot(state_rates, aes(x=year, y=rate, group=1)) +
  geom_line(size=2) +
  facet_wrap(~geog_val, scales="free_y") +
  scale_x_continuous(breaks=c(seq(2001, 2009,2), 2010), minor_breaks=seq(2002,2010,2)) +
  stat_smooth(method="lm", se=F, color="black", linetype=3) +
  theme(legend.title=element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title= "Figure 2",
       x="Year",
       y="Rate per 100,000 population")

print(state_rates_figure)

#regress
regress_states <- data.table(geog_val=character(),
                             beta=numeric())
state_rates[,year_int:= year-2000]
for (geog in unique(state_rates$geog_val)){
  print(paste("regressing for", geog))
  subset <- state_rates[geog_val==geog]
  subset <- subset[, list(deaths=sum(deaths), pop=sum(pop)), by=c("year", "year_int")]
  subset[, rate:=deaths/pop * rate_per]
  out <- lm(rate~year_int, data=subset)
  beta <- out$coefficients[["year_int"]]
  regress_states <- rbind(regress_states, list(geog, beta))
}


state_rates_sex <- data[data_type=="all"& geog_status=="state", list(deaths=sum(deaths), pop=sum(pop)), by="geog_status,geog_val,year,sex"]
state_rates_sex[, rate:=(deaths/pop)*rate_per]
state_rates_sex[,year_int:= year-2000]
regress_states_by_sex <- data.table(geog_val=character(),
                                    sex=character(),
                                    beta=numeric())

for (geog in unique(state_rates_sex$geog_val)){
  print(paste("regressing for", geog))
  for (sexval in unique(state_rates_sex$sex)){
    subset <- state_rates_sex[geog_val==geog & sex==sexval]
    out <- lm(rate~year_int, data=subset)
    beta <- out$coefficients[["year_int"]]
    regress_states_by_sex <- rbind(regress_states_by_sex, list(geog, sexval, beta))
  }
}

regress_states_by_sex <- dcast.data.table(regress_states_by_sex, geog_val~sex, value.var="beta")
regress_states_by_sex[, diff:= Male-Female]
regress_states_by_sex[, abs_diff:= abs(diff)]










#####################################################
## Figure 2: Proportions, faceted by geography,
##            colored by reason
######################################################

reason_props <- data[data_type=="causes"& geog_status!="state", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
reason_props[, sum_deaths:=sum(deaths), by="geog_val,year"]
reason_props[, perc:=(deaths/sum_deaths)*100]
reason_props$geog_val <- factor(reason_props$geog_val, levels=c("National", "Less Developed States", "More Developed States",
                                                                "Less Agricultural States", "More Agricultural States"))
reason_props[, classification:= factor(classification, levels=c("Personal/Social", "Health", "Economic", "Marriage", "Other", "Unknown"))]

reason_props_figure <- ggplot(reason_props, aes(x=year, y=perc, group=classification)) +
            geom_line(size=linesize) +
            geom_point(aes(shape=classification), size=shapesize)+
            facet_grid(~geog_val) +
            #scale_color_manual(values=brewer.pal(6, "Set2")) +
            scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
            theme(legend.title=element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1))+
            labs(title= "Figure 3",
                 x="Year",
                 y="Percent of all suicides")

print(reason_props_figure)

#####################################################
## Figure 3: Proportions, faceted by geography,
##            colored by means
######################################################

means_props <- data[data_type=="means"& geog_status!="state", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
means_props[, sum_deaths:=sum(deaths), by="geog_val,year"]
means_props[, perc:=(deaths/sum_deaths)*100]
means_props$geog_val <- factor(means_props$geog_val, levels=c("National", "Less Developed States", "More Developed States",
                                                              "Less Agricultural States", "More Agricultural States"))
means_props[, classification:= factor(classification, levels=c("Hanging", "Poison/Overdose", "Jumping", "Drowning", "Self-Immolation", "Other"))]


means_props_figure <- ggplot(means_props, aes(x=year, y=perc, group=classification)) +
            geom_line(size=linesize) +
            geom_point(aes(shape=classification), size=shapesize)+
            facet_grid(~geog_val) +
            #scale_color_manual(values=brewer.pal(6, "Set2")) +
            theme(legend.title=element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1))+
            scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
            labs(title= "Figure 4",
                 x="Year",
                 y="Percent of all suicides")

print(means_props_figure)

#####################################################
## Figure S2: Proportions by age for 2010, faceted by sex,
##            colored by means
######################################################

age_means_props <- data[data_type=="means"& geog_status=="national" & year==2010, list(deaths=sum(deaths)), by="sex,agename,classification"]
age_means_props[, sum_deaths:=sum(deaths), by="sex,agename"]
age_means_props[, perc:=(deaths/sum_deaths)*100]
age_means_props[, classification:= factor(classification, levels=c("Hanging", "Poison/Overdose", "Jumping", "Drowning", "Self-Immolation", "Other"))]

age_means_props_figure <- ggplot(age_means_props, aes(x=agename, y=perc, group=classification)) +
  geom_line(size=linesize) +
  geom_point(aes(shape=classification), size=shapesize)+
  facet_grid(~sex) +
  #scale_color_manual(values=brewer.pal(6, "Set2")) +
  theme(legend.title=element_blank())+
  labs(title= "Figure 5",
       x="Age group (years)",
       y="Percent of all suicides")

print(age_means_props_figure)



#####################################################
## Figure 4: Professions: national proportions
######################################################

colors <- c(brewer.pal(8, "Set2"), "#A067B1", "#31A354")
prof_colors <- c(colors[2:4], colors[1], colors[5:10])

prof_props <- data[data_type=="profession"& geog_status=="national", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
prof_props[, sum_deaths:=sum(deaths), by="geog_val,year"]
prof_props[, perc:=(deaths/sum_deaths)*100]

prof_props[, classification:= factor(as.character(classification),
                                     levels=c("Housewife", "SE: Other", "SE: Farm/Agriculture",
                                              "Other", "Salaried Employee", "Unemployed",
                                              "SE: Business", "Student", "SE: Professional", "Retired"))]


prof_props_figure <- ggplot(prof_props, aes(x=year, y=perc, group=classification, linetype=classification)) +
              geom_line(size=linesize) +
              geom_point(aes(shape=classification), size=shapesize) +
              scale_linetype_manual(values=c(1,1,1,1,1,1,1,2,3,4))+
              #scale_color_manual(values=prof_colors) +
              theme(legend.title=element_blank())+
              scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
              labs(title= "Figure 6",
                   x="Year",
                   y="Percent of all suicides")
print(prof_props_figure)


graphics.off()


#####################################################
## Figure S3: Professions: proportions by sex
######################################################

sex_prof_props <- data[data_type=="profession"& geog_status=="national", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification,sex"]
sex_prof_props[, sum_deaths:=sum(deaths), by="geog_val,year,sex"]
sex_prof_props[, perc:=(deaths/sum_deaths)*100]

sex_prof_props_figure <- ggplot(sex_prof_props, aes(x=year, y=perc, group=classification, color=as.character(classification))) +
  geom_line(size=linesize) +
  facet_grid(~sex) +
  scale_color_manual(values=prof_colors) +
  theme(legend.title=element_blank())+
  scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
  labs(title= "Figure S3",
       x="Year",
       y="Percent of all suicides")

print(sex_prof_props_figure)

#####################################################
## Figure S4: Proportions, faceted by ag status,
##            colored by profession
######################################################

ag_prof_props <- data[data_type=="profession"& geog_status=="ag_status", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
ag_prof_props[, sum_deaths:=sum(deaths), by="geog_val,year"]
ag_prof_props[, perc:=(deaths/sum_deaths)*100]

ag_prof_props_figure <- ggplot(ag_prof_props, aes(x=year, y=perc, group=classification, color=as.character(classification))) +
  geom_line(size=linesize) +
  facet_grid(~geog_val) +
  scale_color_manual(values=prof_colors) +
  scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
  theme(legend.title=element_blank())+
  labs(title= "Figure S4",
       x="Year",
       y="Percent of all suicides")

print(ag_prof_props_figure)

