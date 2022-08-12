## Make plots from Dulvy, Sweatman & Kroon  ###

## COT outbreaks on fished and protected reefs

## Load libraries

library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)

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


dat_Dulvey2b <- read.csv(paste(d.dir,  ("Dulvy 2004 2b.csv"), sep = '/'), fileEncoding="UTF-8-BOM")%>% #Table 2 from paper
  #mutate_at(vars(Species, Site), list(as.factor)) %>% # make these columns as factors
  glimpse()
head(df)
str(df)

dat_sweatman <- read.csv(paste(d.dir, ("sweatman 2008 c.csv"), sep="/"), fileEncoding="UTF-8-BOM")%>% 
  mutate_at(vars(zoning, colour), list(as.factor)) %>% # make these columns as factors
  glimpse()


# Make a few pretty plots ----

#Theme ----

lw_theme <- theme_minimal()+
  theme(
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line= element_line(),
    #panel.grid=element_blank(),
    axis.ticks = element_line(),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    plot.title=element_text(size=14, face="italic"),
    legend.title = element_text(size=14, face="bold"),
    legend.position = "right",
    legend.text= element_text(size=14)
  )


# Dulvy 2a ----


plot_Dulvey2a <- ggplot(dat_Dulvey2a, aes(x=Fishing.pressure, y=Predator.density))+
  geom_point( size=2, colour="#177e89")+
  ylim(0,40)+
  xlim(0,60)+
  xlab(bquote(paste("Fishing pressure (people ",km^1, "reef)"))) + 
  ylab(expression ("Predator density "(~gm^2))) +
  lw_theme 

plot_Dulvey2a

# Dulvy 2b ----

plot_Dulvey2b <- ggplot(dat_Dulvey2b, aes(x=fishing.pressure, y=starfish.density, fill=Starfish.present, colour=Starfish.present))+
  geom_point( size=2)+
  scale_colour_manual(values=c("#084c61", "#177e89"))+
  scale_fill_manual(values=c("#084c61", "#177e89"))+
  #ylim(0,40)+
  xlim(0,60)+
  xlab(bquote(paste("Fishing pressure (people ",km^1, "reef)"))) + 
  ylab(expression ("Predator density "(~gm^2))) +
  lw_theme +
  theme(legend.position = "none")

plot_Dulvey2b

# Sweatman plot c ----

dat_sweatman

# reorder levels of factor so open is first in plot 

# Make small first
dat_sweatman$zoning<- relevel(dat_sweatman$zoning, "open")
dat_sweatman


plot_sweatman <- ggplot(dat_sweatman, aes(x=zoning, y=No.reefs, fill=colour, colour=colour))+
  geom_bar(position="dodge", stat="identity")+
  scale_colour_manual(name= "",
                      breaks=c("black", "white"),
                      labels=c("outbreak", "no outbreak"),
                      values=c("#177e89", "#ffc857"))+
  scale_fill_manual(name= "",
                    breaks=c("black", "white"),
                    labels=c("outbreak", "no outbreak"),
                    values=c("#177e89", "#ffc857"))+
  scale_x_discrete(labels=c("Fished", "No-take"))+
  ylim(0,20)+
  #xlim(0,60)+
  xlab("GBRMP zoning") + 
  ylab("Number of reefs") +
  lw_theme + theme(legend.position="top")
  
plot_sweatman
