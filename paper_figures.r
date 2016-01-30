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
data[!geog_status %in% c("national", "state"), geog_val:= paste(geog_val, "States")]


rate_per <- 100000
linesize <- 2

pdf(paste0(main_dir, "../writing_and_papers/paper/figures/paper_figs.pdf"), width=14, height=8)

#####################################################
## Figure 1: Rates per 100,000, faceted by geography,
##          colored by sex
######################################################
all_rates <- data[data_type=="all"& geog_status!="state", list(deaths=sum(deaths), pop=sum(pop)), by="geog_status,geog_val,year,sex"]
all_rates[, rate:=(deaths/pop)*rate_per]
all_rates$geog_val <- factor(all_rates$geog_val, levels=c("National", "Less Developed States", "More Developed States", "Agricultural States", "Non-Agricultural States"))


all_rates_figure <- ggplot(all_rates, aes(x=year, y=rate, group=sex, color=sex))+
            geom_line(size=linesize) +
            facet_grid(~geog_val) +
            scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
            stat_smooth(method="lm", se=F, color="black") +
            theme(legend.title=element_blank(), 
                  axis.text.x=element_text(angle=45, hjust=1))+
            labs(title= "Figure 1",
                 x="Year",
                 y="Rate per 100,000 population")

print(all_rates_figure)


#####################################################
## Figure 2: Proportions, faceted by geography,
##            colored by reason
######################################################

reason_props <- data[data_type=="causes"& geog_status!="state", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
reason_props[, sum_deaths:=sum(deaths), by="geog_val,year"]
reason_props[, perc:=(deaths/sum_deaths)*100]
reason_props$geog_val <- factor(reason_props$geog_val, levels=c("National", "Less Developed States", "More Developed States", "Agricultural States", "Non-Agricultural States"))
reason_props[, classification:= factor(classification, levels=c("Personal/Social", "Health", "Economic", "Marriage", "Other", "Unknown"))]

reason_props_figure <- ggplot(reason_props, aes(x=year, y=perc, group=classification, color=classification)) +
            geom_line(size=linesize) +
            facet_grid(~geog_val) +
            scale_color_manual(values=brewer.pal(6, "Set2")) +
            scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
            theme(legend.title=element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1))+
            labs(title= "Figure 2",
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
means_props$geog_val <- factor(means_props$geog_val, levels=c("National", "Less Developed States", "More Developed States", "Agricultural States", "Non-Agricultural States"))
means_props[, classification:= factor(classification, levels=c("Hanging", "Poison/Overdose", "Jumping", "Drowning", "Self-Immolation", "Other"))]

means_props_figure <- ggplot(means_props, aes(x=year, y=perc, group=classification, color=classification)) +
            geom_line(size=linesize) +
            facet_grid(~geog_val) +
            scale_color_manual(values=brewer.pal(6, "Set2")) +
            theme(legend.title=element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1))+
            scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
            labs(title= "Figure 3",
                 x="Year",
                 y="Percent of all suicides")

print(means_props_figure)


#####################################################
## Figure 4: Professions: national proportions
######################################################

colors <- c(brewer.pal(8, "Set2"), "#A067B1", "#31A354")
prof_colors <- c(colors[2:4], colors[1], colors[5:10])

prof_props <- data[data_type=="profession"& geog_status=="national", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
prof_props[, sum_deaths:=sum(deaths), by="geog_val,year"]
prof_props[, perc:=(deaths/sum_deaths)*100]

prof_props_figure <- ggplot(prof_props, aes(x=year, y=perc, group=classification, color=as.character(classification))) +
              geom_line(size=linesize) +
              scale_color_manual(values=prof_colors) +
              theme(legend.title=element_blank())+
              scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
              labs(title= "Figure 4",
                   x="Year",
                   y="Percent of all suicides")
print(prof_props_figure)


#####################################################
## Figure S1: Rates per 100,000, faceted by state
######################################################

state_rates <- data[data_type=="all"& geog_status=="state", list(deaths=sum(deaths), pop=sum(pop)), by="geog_status,geog_val,year"]
state_rates[, rate:=(deaths/pop)*rate_per]

state_rates_figure <- ggplot(state_rates, aes(x=year, y=rate, group=1)) +
  geom_line(size=linesize, color=gg_color_hue(2)[[2]]) +
  facet_wrap(~geog_val, scales="free_y") +
  scale_x_continuous(breaks=c(2001, 2006, 2010), minor_breaks=c(2002,2003,2004,2005,2007,2008,2009)) +
  stat_smooth(method="lm", se=F, color="black") +
  theme(legend.title=element_blank())+
  labs(title= "Figure S1",
       x="Year",
       y="Rate per 100,000 population")

print(state_rates_figure)

#####################################################
## Figure S2: Proportions by age for 2010, faceted by sex,
##            colored by means
######################################################

age_means_props <- data[data_type=="means"& geog_status=="national" & year==2010, list(deaths=sum(deaths)), by="sex,agename,classification"]
age_means_props[, sum_deaths:=sum(deaths), by="sex,agename"]
age_means_props[, perc:=(deaths/sum_deaths)*100]
age_means_props[, classification:= factor(classification, levels=c("Hanging", "Poison/Overdose", "Jumping", "Drowning", "Self-Immolation", "Other"))]

age_means_props_figure <- ggplot(age_means_props, aes(x=agename, y=perc, group=classification, color=classification)) +
  geom_line(size=linesize) +
  facet_grid(~sex) +
  scale_color_manual(values=brewer.pal(6, "Set2")) +
  theme(legend.title=element_blank())+
  labs(title= "Figure S2",
       x="Age group (years)",
       y="Percent of all suicides")

print(age_means_props_figure)

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

graphics.off()