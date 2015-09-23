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

rate_per <- 100000



#####################################################
## Figure 1: Rates per 100,000, faceted by geography,
##          colored by sex
######################################################
pdf(paste0(main_dir, "../writing_and_papers/paper/figures/paper_figs_1_5.pdf"), width=14, height=8)
data_1 <- data[data_type=="all"& geog_status!="state", list(deaths=sum(deaths), pop=sum(pop)), by="geog_status,geog_val,year,sex"]
data_1[, rate:=(deaths/pop)*rate_per]
data_1$geog_val <- factor(data_1$geog_val, levels=c("National", "Less Developed", "More Developed", "Agricultural", "Non-Agricultural"))


figure_1 <- ggplot(data_1, aes(x=year, y=rate, group=sex, color=sex))+
            geom_line(size=3) +
            facet_grid(~geog_val) +
            scale_x_continuous(breaks=seq(2001, 2009, 2), minor_breaks=seq(2002,2010,2)) +
            stat_smooth(method="lm", se=F, color="black") +
            theme(legend.title=element_blank())+
            labs(title= "Suicide Rate Per 100,000 by Sex and Administrative Level",
                 x="Year",
                 y="Rate per 100,000")

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
            labs(title= "Suicide Rate Per 100,000 by State",
                 x="Year",
                 y="Rate per 100,000")

print(figure_2)

#####################################################
## Figure 3: Proportions, faceted by geography,
##            colored by reason
######################################################

data_3 <- data[data_type=="causes"& geog_status!="state", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
data_3[, sum_deaths:=sum(deaths), by="geog_val,year"]
data_3[, perc:=(deaths/sum_deaths)*100]
data_3$geog_val <- factor(data_3$geog_val, levels=c("National", "Less Developed", "More Developed", "Agricultural", "Non-Agricultural"))

figure_3 <- ggplot(data_3, aes(x=year, y=perc, group=classification, color=classification)) +
            geom_line(size=3) +
            facet_grid(~geog_val) +
            scale_color_manual(values=brewer.pal(6, "Set2")) +
            scale_x_continuous(breaks=seq(2001, 2009, 2), minor_breaks=seq(2002,2010,2)) +
            theme(legend.title=element_blank())+
            labs(title= "Reason for Suicide, by Administrative Level",
                 x="Year",
                 y="Percent of All Suicides")

print(figure_3)

#####################################################
## Figure 4: Proportions, faceted by geography,
##            colored by means
######################################################

data_4 <- data[data_type=="means"& geog_status!="state", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
data_4[, sum_deaths:=sum(deaths), by="geog_val,year"]
data_4[, perc:=(deaths/sum_deaths)*100]
data_4$geog_val <- factor(data_4$geog_val, levels=c("National", "Less Developed", "More Developed", "Agricultural", "Non-Agricultural"))

figure_4 <- ggplot(data_4, aes(x=year, y=perc, group=classification, color=as.character(classification))) +
            geom_line(size=3) +
            facet_grid(~geog_val) +
            scale_color_manual(values=brewer.pal(6, "Set2")) +
            theme(legend.title=element_blank())+
            scale_x_continuous(breaks=seq(2001, 2009, 2), minor_breaks=seq(2002,2010,2)) +
            labs(title= "Means of Suicide, by Administrative Level",
                 x="Year",
                 y="Percent of All Suicides")

print(figure_4)

#####################################################
## Figure 5: Proportions by age for 2010, faceted by sex,
##            colored by means
######################################################

data_5 <- data[data_type=="means"& geog_status=="national" & year==2010, list(deaths=sum(deaths)), by="sex,agename,classification"]
data_5[, sum_deaths:=sum(deaths), by="sex,agename"]
data_5[, perc:=(deaths/sum_deaths)*100]

figure_5 <- ggplot(data_5, aes(x=agename, y=perc, group=classification, color=as.character(classification))) +
            geom_line(size=3) +
            facet_grid(~sex) +
            scale_color_manual(values=brewer.pal(6, "Set2")) +
            theme(legend.title=element_blank())+
            labs(title= "Means of Suicide, by Age and Sex, 2010",
                 x="Age",
                 y="Percent of All Suicides")

print(figure_5)

graphics.off()
#####################################################
## Figure 6: Professions, athree-part plot:
##          A: proportions, national
##          B: proportions by sex
##          C: Proportions by age & sex, 2010
######################################################
pdf(paste0(main_dir, "../writing_and_papers/paper/figures/paper_figs_6.pdf"), width=8, height=14)

colors <- c(brewer.pal(8, "Set2"), "#A067B1", "#31A354")
prof_colors <- c(colors[2:4], colors[1], colors[5:10])

data_6_a <- data[data_type=="profession"& geog_status=="national", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
data_6_a[, sum_deaths:=sum(deaths), by="geog_val,year"]
data_6_a[, perc:=(deaths/sum_deaths)*100]

figure_6_a <- ggplot(data_6_a, aes(x=year, y=perc, group=classification, color=as.character(classification))) +
              geom_line(size=3) +
              scale_color_manual(values=prof_colors) +
              scale_x_continuous(breaks=seq(2001, 2009, 2), minor_breaks=seq(2002,2010,2)) +
              theme(legend.position="none")+
              labs(title= "A: Profession of Suicide Committers",
                   x="Year",
                   y="Percent of All Suicides")

data_6_b <- data[data_type=="profession"& geog_status=="national", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification,sex"]
data_6_b[, sum_deaths:=sum(deaths), by="geog_val,year,sex"]
data_6_b[, perc:=(deaths/sum_deaths)*100]

figure_6_b <- ggplot(data_6_b, aes(x=year, y=perc, group=classification, color=as.character(classification))) +
              geom_line(size=3) +
              facet_grid(~sex) +
              scale_color_manual(values=prof_colors) +
              theme(legend.title=element_blank())+
              scale_x_continuous(breaks=seq(2001, 2009, 2), minor_breaks=seq(2002,2010,2)) +
              labs(title= "B: Profession of Suicide Committers by Sex",
                   x="Year",
                   y="Percent of All Suicides")

data_6_c <- data[data_type=="profession"& geog_status=="national" & year==2010, list(deaths=sum(deaths)), by="sex,agename,classification"]
data_6_c[, sum_deaths:=sum(deaths), by="sex,agename"]
data_6_c[, perc:=(deaths/sum_deaths)*100]

figure_6_c <- ggplot(data_6_c, aes(x=agename, y=perc, group=classification, color=as.character(classification))) +
              geom_line(size=3) +
              facet_grid(~sex) +
              scale_color_manual(values=prof_colors) +
              theme(legend.position="none")+
              labs(title= "C: Profession of Suicide Committers by Age and Sex, 2010",
                   x="Year",
                   y="Percent of All Suicides")

multiplot(figure_6_a, figure_6_b, figure_6_c, cols=1) #todo: get this to work
graphics.off()
#####################################################
## Figure 7: Proportions, faceted by ag status,
##            colored by profession
######################################################
pdf(paste0(main_dir, "../writing_and_papers/paper/figures/paper_figs_7_8.pdf"), width=14, height=8)

data_7 <- data[data_type=="profession"& geog_status=="ag_status", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
data_7[, sum_deaths:=sum(deaths), by="geog_val,year"]
data_7[, perc:=(deaths/sum_deaths)*100]

figure_7 <- ggplot(data_7, aes(x=year, y=perc, group=classification, color=as.character(classification))) +
            geom_line(size=3) +
            facet_grid(~geog_val) +
            scale_color_manual(values=prof_colors) +
            scale_x_continuous(breaks=seq(2001, 2009, 2), minor_breaks=seq(2002,2010,2)) +
            theme(legend.title=element_blank())+
            labs(title= "Means of Suicide, by Agricultural Status",
                 x="Year",
                 y="Percent of All Suicides")

print(figure_7)

#####################################################
## Figure 8: Proportions, faceted by ag state,
##            colored by profession
######################################################

data_8 <- data[data_type=="profession"& geog_status=="state" & sex=="Males", list(deaths=sum(deaths)), by="geog_status,geog_val,year,classification"]
data_8[, sum_deaths:=sum(deaths), by="geog_val,year"]
data_8[, perc:=(deaths/sum_deaths)*100]

figure_8 <- ggplot(data_8, aes(x=year, y=perc, group=classification, color=as.character(classification))) +
            geom_line(size=2) +
            facet_wrap(~geog_val, scales="free_y") +
            scale_color_manual(values=prof_colors) +
            theme(legend.title=element_blank())+
            scale_x_continuous(breaks=c(2001, 2006, 2010), minor_breaks=c(2002,2003,2004,2005,2007,2008,2009)) +
            labs(title= "Means of Suicide among Males, by State",
                 x="Year",
                 y="Percent of All Suicides")

print(figure_8)

graphics.off()