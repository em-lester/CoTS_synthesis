## Make plots from Dulvy, Sweatman & Kroon  ###

## COT outbreaks on fished and protected reefs

## Load libraries

library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(MASS) # to access Animals data sets
library(scales)

# clear workspace ----
rm(list = ls())

# set working directories ----

w.dir <- "~/Repositories/CoTS_synthesis"
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
d.dir <- (paste(w.dir, "Data", sep='/'))
#dr.dir <- (paste(w.dir, "Data/Raw", sep='/'))
#s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')
#r.dir <- paste(w.dir, "rasters", sep='/')

# Load data ----

study <- "CoTS"

setwd (w.dir)
dir()

dat_Dulvey2a <- read.csv(paste(d.dir,  ("Dulvy 2a.csv"), sep = '/'), fileEncoding="UTF-8-BOM")%>% #Table 2 from paper
  #mutate_at(vars(Species, Site), list(as.factor)) %>% # make these columns as factors
  glimpse()


dat_Dulvey2b <- read.csv(paste(d.dir,  ("Dulvy_2004_b.csv"), sep = '/'), fileEncoding="UTF-8-BOM")%>% #Table 2 from paper
  #mutate_at(vars(Species, Site), list(as.factor)) %>% # make these columns as factors
  glimpse()


dat_sweatman <- read.csv(paste(d.dir, ("sweatman 2008 c.csv"), sep="/"), fileEncoding="UTF-8-BOM")%>% 
  mutate_at(vars(zoning, colour), list(as.factor)) %>% # make these columns as factors
  glimpse()

dat_kroon <- read.csv(paste(d.dir, ("Kroon_2021_data.csv"), sep="/"), fileEncoding="UTF-8-BOM")%>% 
  mutate_at(vars(zone), list(as.factor)) %>% # make these columns as factors
  glimpse()

# Make a few pretty plots ----

#Theme ----

lw_theme <- theme_minimal()+
  theme(
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line= element_line(),
    #panel.grid=element_blank(),
    axis.ticks = element_line(),
    axis.text.x=element_text(size=15),
    axis.text.y=element_text(size=15),
    plot.title=element_text(size=15, face="italic"),
    legend.title = element_text(size=15, face="bold"),
    legend.position = "right",
    legend.text= element_text(size=15)
  )


# Dulvy 2a ----


plot_Dulvey2a <- ggplot(dat_Dulvey2a, aes(x=Fishing.pressure, y=Predator.density))+
  geom_point( size=3, colour="#177e89")+
  ylim(0,40)+
  xlim(0,60)+
  xlab(bquote(paste("Fishing pressure (people ",km^1, "reef)"))) + 
  ylab(expression ("Predator density "(~gm^2))) +
  lw_theme 

plot_Dulvey2a

# Dulvy 2b ----
dat_Dulvey2b 
plot_Dulvey2b <- ggplot(dat=dat_Dulvey2b, aes(x=fishing.pressure, y=starfish.density, fill=colour, colour=colour))+
  geom_point( size=3)+
  scale_colour_manual(values=c( "#177e89", "#084c61"))+
  scale_fill_manual(values=c( "#177e89", "#084c61"))+
 # ylim(0,100000)+
  xlim(0,60)+
  geom_smooth(method=lm,   # Add linear regression lines
              data=subset(dat_Dulvey2b, colour=="black"),
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)+
  xlab(bquote(paste("Fishing pressure (people ",km^1, "reef)"))) + 
  ylab(bquote(paste ("Starfish density (no. "~km^-2,")"))) +
  lw_theme +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme(legend.position = "none")

plot_Dulvey2b

# Sweatman plot c ----

dat_sweatman

# reorder levels of factor so open is first in plot 

# Make small first
dat_sweatman$zoning<- relevel(dat_sweatman$zoning, "open")
dat_sweatman


plot_sweatman <- ggplot(dat_sweatman, aes(x=zoning, y=No.reefs, fill=zoning, colour=zoning, alpha=colour))+
  geom_bar(position="dodge", stat="identity")+
  scale_colour_manual(name= "",
                      breaks=c("open", "no-take"),
                      labels=c("Fished", "No-take"),
                      values=c("#177e89", "#db3a34"))+
  scale_fill_manual(name= "",
                    breaks=c("open", "no-take"),
                    labels=c("Fished", "No-take"),
                    values=c("#177e89", "#db3a34"))+
  scale_alpha_manual(name= "",
                    breaks=c("black", "white"),
                    labels=c("Outbreak", "No outbreak"),
                    values=c(0.8, 0.4))+
  scale_x_discrete(labels=c("Fished", "No-take"))+
  ylim(0,20)+
  #xlim(0,60)+
  xlab("GBRMP zoning") + 
  ylab("Number of reefs") +
  lw_theme + theme(legend.position="top")
  
plot_sweatman

# plot Kroon (2021) -----

dat_kroon


plot_kroon <- ggplot(dat_kroon, aes(x=zone, y=mean, fill=zone, colour=zone))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=semin, ymax=semax), width=0.2, size=1)+
  scale_colour_manual(values=c("#177e89", "#db3a34"))+
  scale_fill_manual(values=c("#177e89", "#db3a34"))+
  #ylim(0,40)+
 # xlim(0,60)+
  xlab("Reef zoning status") + 
  scale_x_discrete(labels=c("Fished", "No-take"))+
  ylab(bquote(paste("Predicted CoTS counts (2 ",mins^-1, ")"))) +
  lw_theme +
  theme(legend.position = "none")

plot_kroon


# combine plots ----

leftCol <- (plot_Dulvey2a / plot_Dulvey2b) & theme(plot.tag.position  = c(.1, 1.01))
rightCol <- (plot_sweatman/plot_kroon)  & theme(plot.tag.position  = c(.1, 1.01))

combined_plots <- wrap_plots(leftCol, rightCol) &  # or "leftCol - rightCol"   
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 16))

combined_plots

setwd (p.dir)
ggsave("CoTS_outbreak_plots.tiff", combined_plots , width=14, height=10, dpi=300)
