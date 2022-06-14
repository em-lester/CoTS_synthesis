## Make plots from Barley et al. (2017)  ###

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

dir()

df <- read.csv(paste(d.dir,  ("gut_contents.csv"), sep = '/'), fileEncoding="UTF-8-BOM")%>% #Table 2 from paper
  mutate_at(vars(Species, Site), list(as.factor)) %>% # make these columns as factors
  glimpse()
head(df)
str(df)

# switch data to long format

data_long <- gather(df, Prey, Percentage, Bivalve:squid, factor_key=TRUE)
data_long

# add in column for water column vs benthic prey




# Make a piecharts

