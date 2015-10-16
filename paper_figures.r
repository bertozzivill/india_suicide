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

pdf(paste0(main_dir, "../writing_and_papers/paper/figures/paper_figs.pdf"), width=14, height=8)

#####################################################
## Figure 1: Rates per 100,000, faceted by geography,
##          colored by sex
######################################################
data_1 <- data[data_type=="all"& geog_status!="state", list(deaths=sum(deaths), pop=sum(pop)), by="geog_status,geog_val,year,sex"]
data_1[, rate:=(deaths/pop)*rate_per]
data_1$geog_val <- factor(data_1$geog_val, levels=c("National", "Less Developed States", "More Developed States", "Agricultural States", "Non-Agricultural States"))


figure_1 <- ggplot(data_1, aes(x=year, y=rate, group=sex, color=sex))+
            geom_line(size=3) +
            facet_grid(~geog_val) +
            scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
            stat_smooth(method="lm", se=F, color="black") +
            theme(legend.title=element_blank(), 
                  axis.text.x=element_text(angle=45, hjust=1))+
            labs(title= "Suicide rate per 100,000 population, by sex",
                 x="Year",
                 y="Rate per 100,000 population")

print(figure_1)

#####################################################
## Figure 2: Rates per 100,000, faceted by state
######################################################

data_2 <- data[data_type=="all"& geog_status=="state", list(deaths=sum(deaths), pop=sum(pop)), by="geog_status,geog_val,year"]
data_2[, rate:=(deaths/pop)*rate_per]

figure_2 <- ggplot(data_2, aes(x=year, y=rate, group=1)) +
            geom_line(size=3, color=gg_color_hue(2)[[2]]) +
            facet_wrap(~geog_val, scales="free_y") +
            scale_x_continuous(breaks=c(2001, 2006, 2010), minor_breaks=c(2002,2003,2004,2005,2007,2008,2009)) +
            stat_smooth(method="lm", se=F, color="black") +
           theme(legend.title=element_blank())+
            labs(title= "Suicide rate per 100,000 population, by state",
                 x="Year",
                 y="Rate per 100,000 population")

print(figure_2)

#####################################################
## Figure 3: Proportions, faceted by geography,
##            colored by reason
######################################################

data_3 <- data[data_type=="causes"& geog_status!="state", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
data_3[, sum_deaths:=sum(deaths), by="geog_val,year"]
data_3[, perc:=(deaths/sum_deaths)*100]
data_3$geog_val <- factor(data_3$geog_val, levels=c("National", "Less Developed States", "More Developed States", "Agricultural States", "Non-Agricultural States"))
data_3[, classification:= factor(classification, levels=c("Personal/Social", "Health", "Economic", "Marriage", "Other", "Unknown"))]

figure_3 <- ggplot(data_3, aes(x=year, y=perc, group=classification, color=classification)) +
            geom_line(size=3) +
            facet_grid(~geog_val) +
            scale_color_manual(values=brewer.pal(6, "Set2")) +
            scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
            theme(legend.title=element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1))+
            labs(title= "Reason for committing suicide",
                 x="Year",
                 y="Percent of all suicides")

print(figure_3)

#####################################################
## Figure 4: Proportions, faceted by geography,
##            colored by means
######################################################

data_4 <- data[data_type=="means"& geog_status!="state", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
data_4[, sum_deaths:=sum(deaths), by="geog_val,year"]
data_4[, perc:=(deaths/sum_deaths)*100]
data_4$geog_val <- factor(data_4$geog_val, levels=c("National", "Less Developed States", "More Developed States", "Agricultural States", "Non-Agricultural States"))
data_4[, classification:= factor(classification, levels=c("Hanging", "Poison/Overdose", "Jumping", "Drowning", "Self-Immolation", "Other"))]

figure_4 <- ggplot(data_4, aes(x=year, y=perc, group=classification, color=classification)) +
            geom_line(size=3) +
            facet_grid(~geog_val) +
            scale_color_manual(values=brewer.pal(6, "Set2")) +
            theme(legend.title=element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1))+
            scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
            labs(title= "Means of committing suicide",
                 x="Year",
                 y="Percent of all suicides")

print(figure_4)

#####################################################
## Figure 5: Proportions by age for 2010, faceted by sex,
##            colored by means
######################################################

data_5 <- data[data_type=="means"& geog_status=="national" & year==2010, list(deaths=sum(deaths)), by="sex,agename,classification"]
data_5[, sum_deaths:=sum(deaths), by="sex,agename"]
data_5[, perc:=(deaths/sum_deaths)*100]
data_5[, classification:= factor(classification, levels=c("Hanging", "Poison/Overdose", "Jumping", "Drowning", "Self-Immolation", "Other"))]

figure_5 <- ggplot(data_5, aes(x=agename, y=perc, group=classification, color=classification)) +
            geom_line(size=3) +
            facet_grid(~sex) +
            scale_color_manual(values=brewer.pal(6, "Set2")) +
            theme(legend.title=element_blank())+
            labs(title= "Means of committing suicide, by age groups and sex, 2010",
                 x="Age group (years)",
                 y="Percent of all suicides")

print(figure_5)

#####################################################
## Figure 6: Professions: national proportions
######################################################

colors <- c(brewer.pal(8, "Set2"), "#A067B1", "#31A354")
prof_colors <- c(colors[2:4], colors[1], colors[5:10])

data_6 <- data[data_type=="profession"& geog_status=="national", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
data_6[, sum_deaths:=sum(deaths), by="geog_val,year"]
data_6[, perc:=(deaths/sum_deaths)*100]

figure_6 <- ggplot(data_6, aes(x=year, y=perc, group=classification, color=as.character(classification))) +
              geom_line(size=3) +
              scale_color_manual(values=prof_colors) +
              theme(legend.title=element_blank())+
              scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
              labs(title= "Profession of those who had committed suicide. SE stands for self-employed.",
                   x="Year",
                   y="Percent of all suicides")
print(figure_6)

#####################################################
## Figure 7: Professions: proportions by sex
######################################################


data_7 <- data[data_type=="profession"& geog_status=="national", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification,sex"]
data_7[, sum_deaths:=sum(deaths), by="geog_val,year,sex"]
data_7[, perc:=(deaths/sum_deaths)*100]

figure_7 <- ggplot(data_7, aes(x=year, y=perc, group=classification, color=as.character(classification))) +
              geom_line(size=3) +
              facet_grid(~sex) +
              scale_color_manual(values=prof_colors) +
              theme(legend.title=element_blank())+
              scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
              labs(title= "Profession of those who had committed suicide, by sex. SE stands for self-employed.",
                   x="Year",
                   y="Percent of all suicides")

print(figure_7)

#####################################################
## Figure 8: Proportions, faceted by ag status,
##            colored by profession
######################################################

data_8 <- data[data_type=="profession"& geog_status=="ag_status", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
data_8[, sum_deaths:=sum(deaths), by="geog_val,year"]
data_8[, perc:=(deaths/sum_deaths)*100]

figure_8 <- ggplot(data_8, aes(x=year, y=perc, group=classification, color=as.character(classification))) +
            geom_line(size=3) +
            facet_grid(~geog_val) +
            scale_color_manual(values=prof_colors) +
            scale_x_continuous(breaks=c(seq(2001, 2009, 2), 2010), minor_breaks=seq(2002,2010,2)) +
            theme(legend.title=element_blank())+
            labs(title= "Profession of those who had committed suicide, by agricultural status. SE stands for self-employed.",
                 x="Year",
                 y="Percent of all suicides")

print(figure_8)

#####################################################
## Figure 9: Proportions, faceted by ag state,
##            colored by profession
######################################################

data_9 <- data[data_type=="profession"& geog_status=="state" & sex=="Male", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
data_9[, sum_deaths:=sum(deaths), by="geog_val,year"]
data_9[, perc:=(deaths/sum_deaths)*100]

figure_9 <- ggplot(data_9, aes(x=year, y=perc, group=classification, color=as.character(classification))) +
            geom_line(size=2) +
            facet_wrap(~geog_val, scales="free_y") +
            scale_color_manual(values=prof_colors) +
            theme(legend.title=element_blank())+
            scale_x_continuous(breaks=c(2001, 2006, 2010), minor_breaks=c(2002,2003,2004,2005,2007,2008,2009)) +
            labs(title= "Profession of those who had committed suicide among males, by state. SE stands for self-employed.",
                 x="Year",
                 y="Percent of All Suicides")

print(figure_9)

graphics.off()