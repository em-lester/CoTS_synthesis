## Make plots from Barley et al. (2017)  ###

## log length log weight relationships 

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

df <- read.csv(paste(d.dir,  ("all_species.csv"), sep = '/'), fileEncoding="UTF-8-BOM")%>% #Table 2 from paper
  mutate_at(vars(Species, Site), list(as.factor)) %>% # make these columns as factors
  glimpse()

head(df)
str(df)

# Make a few length weight plots ----

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


# L. gibbus ----

dat.gibbus<- df %>%
  filter(Species =="L. gibbus")%>%
  glimpse()

plot.gibbus <- ggplot(dat.gibbus, aes(x=Log.L, y=Log.W, fill=Site, colour=Site))+
  geom_point( size=2)+
  scale_fill_manual(values=c( "#adf0d1", "#00203f" ))+
  scale_colour_manual(values=c( "#adf0d1","#00203f" ))+
  xlab("Log weight") + ylab("Log length") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)+
  lw_theme 

plot.gibbus

# L. decussatus ----

dat.decussatus <- df %>%
  filter(Species =="L. decussatus")%>%
  glimpse()

plot.decussatus <- ggplot(dat.decussatus, aes(x=Log.L, y=Log.W, fill=Site, colour=Site))+
  geom_point( size=2)+
  scale_fill_manual(values=c( "#adf0d1", "#00203f" ))+
  scale_colour_manual(values=c( "#adf0d1","#00203f" ))+
  xlab("Log weight") + ylab("Log length") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)+
  lw_theme 

plot.decussatus

# L. bohar ----

dat.bohar <- df %>%
  filter(Species =="L. bohar")%>%
  glimpse()

plot.bohar <- ggplot(dat.bohar, aes(x=Log.L, y=Log.W, fill=Site, colour=Site))+
  geom_point( size=2)+
  scale_fill_manual(values=c( "#adf0d1", "#00203f" ))+
  scale_colour_manual(values=c( "#adf0d1","#00203f" ))+
  xlab("Log weight") + ylab("Log length") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)+
  lw_theme 

plot.bohar


# combine with gut contents