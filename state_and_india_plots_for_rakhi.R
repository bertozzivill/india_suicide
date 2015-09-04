####################################################################################
## Author: Amelia Bertozzi-Villa
## Description: Generate plots of suicide rate, cause proportions, and mean proportions
## for only India, Uttar Pradesh, Meghalaya, and Nagaland
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

load(paste0(main_dir, "outputs/all_deaths.rdata"))

#quick diversion
ag <- data[data_type=="profession" & geog_status=="ag_status"]
ag <- ag[, list(deaths=sum(deaths)), by="geog_val,year,sex,classification"]
ag[, sum_deaths:=sum(deaths), by="geog_val,year,sex"]
ag[, prop:=deaths/sum_deaths]

ggplot(ag[sex=="Males"], aes(x=year, y=prop, group=classification, color=classification)) +
        geom_line(size=3) +
        facet_grid(~geog_val) +
        stat_smooth(method="lm", se=F) +
        scale_color_manual(values=c(brewer.pal(8, "Set2"), "#A067B1", "#31A354"))


geog_list <- c("National", "Meghalaya", "Nagaland", "Uttar Pradesh")
#collapse and subset
data <- data[geog_val %in% geog_list & data_type!="profession", list(deaths=sum(deaths), pop=sum(pop)), by="data_type,geog_val,year,sex,classification"]

data$geog_val <- factor(data$geog_val, levels=geog_list)

pdf(paste0(main_dir, "plots/state_and_india_plots_for_rakhi.pdf"), width=14, height=8)
#plot rates by geography and sex
rates <- data[data_type=="all"]
rates[, rate:=(deaths/pop)*100000]

rate_plot <- ggplot(rates, aes(x=year, y=rate, group=sex, color=sex)) +
            geom_line(size=2) +
            facet_grid(~geog_val) +
            stat_smooth(method="lm", se=F) +
            scale_x_continuous(breaks=seq(2001, 2010,2), minor_breaks=seq(2002,2010,2)) +
            theme(legend.position="bottom", legend.title=element_blank()) +
            labs(title="Suicide Rate per 100,000 by Geography and Sex",
                 x="Year",
                 y="Rate per 100,000")

print(rate_plot)


#plot means and reason by proportion and sex
props <- data[data_type!="all"]
props[, sum_deaths:=sum(deaths), by="geog_val,year,sex"]
props[, prop:=deaths/sum_deaths]
setkeyv(props, "data_type")

reason_plot <- ggplot(props[J("causes")], aes(x=year, y=prop, group=classification, color=classification)) +
                      geom_line(size=2) +
                      facet_grid(sex~geog_val) +
                      scale_color_manual(values=brewer.pal(6, "Set2")) +
                      scale_x_continuous(breaks=seq(2001, 2010,2), minor_breaks=seq(2002,2010,2)) +
                      theme(legend.title=element_blank()) +
                      labs(title="Suicide Proportion by Reason, Geography and Sex",
                           x="Year",
                           y="Proportion")

print(reason_plot)

means_plot <- ggplot(props[J("means")], aes(x=year, y=prop, group=classification, color=classification)) +
                      geom_line(size=2) +
                      facet_grid(sex~geog_val) +
                      scale_color_manual(values=brewer.pal(6, "Set2")) +
                      scale_x_continuous(breaks=seq(2001, 2010,2), minor_breaks=seq(2002,2010,2)) +
                      theme(legend.title=element_blank()) +
                      labs(title="Suicide Proportion by Means, Geography and Sex",
                           x="Year",
                           y="Proportion")

print(means_plot)

graphics.off()

#save 
props$sum_deaths <- NULL
props[, metric:="proportion"]
setnames(props, "prop", "value")

rates[, metric:="rate_per_100000"]
setnames(rates, "rate", "value")

data <- rbind(rates, props)
data <- data[, list(data_type, geog_val, sex, year, classification, deaths, pop, metric, value)]
data <- data[order(data_type, geog_val, sex, year, classification)]
write.csv(data, file=paste0(main_dir, "outputs/state_and_india_data.csv"))
