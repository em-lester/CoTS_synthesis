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

data_long <- gather(df, Prey, Percentage, Bivalve:Squid, factor_key=TRUE)
data_long

# add in column for water column vs benthic prey

dat <- data_long%>%
  mutate(Source= ifelse(Prey %in% c("Bivalve", "Crab", "Echinoderm", "Gatropod", "Isopod",
                                     "Marine worm", "Octopus"), "Benthic", "Water Column"))%>%
  glimpse()


 

# Make a few pie charts ----

#Theme ----

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    plot.title=element_text(size=14, face="italic"),
    legend.title = element_text(size=14, face="bold"),
    legend.text= element_text(size=13)
  )

# Colours ----
# blue colours #022B3A, #1F7A8C, #BFDBF7
# yellow colours, "F2D9BB", #F8E5A0, #F4D35E, "A9570F", #EE964B, "74401D", "3F292B"

# L. gibbus ----

dat.gibbus.scott <- dat %>%
  filter(Species =="L. gibbus")%>%
  filter(Site=="Scott Reefs")%>%
  glimpse()

plot.gibbus.scott <- ggplot(dat.gibbus.scott, aes(x="", y=Percentage, fill=Prey))+
  geom_bar(width = 1, stat = "identity")+ 
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("#F2D9BB", "#F8E5A0", "#F4D35E", "#A9570F", "#EE964B", 
                             "#74401D", "#3F292B","#022B3A", "#1F7A8C", "#BFDBF7" ))+
  ggtitle("Lutjanus gibbus")+
  guides(fill=guide_legend(title="Gut contents"))+
  blank_theme

plot.gibbus.scott


dat.gibbus.rowleys <- dat %>%
  filter(Species =="L. gibbus")%>%
  filter(Site=="Rowley Shoals")%>%
  glimpse()

plot.gibbus.rowleys <- ggplot(dat.gibbus.rowleys, aes(x="", y=Percentage, fill=Prey))+
  geom_bar(width = 1, stat = "identity")+ 
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("#F2D9BB", "#F8E5A0", "#F4D35E", "#A9570F", "#EE964B", 
                             "#74401D", "#3F292B","#022B3A", "#1F7A8C", "#BFDBF7" ))+
 # ggtitle("Lutjanus gibbus")+
  guides(fill=guide_legend(title="Gut contents"))+
  blank_theme

plot.gibbus.rowleys


# L. decussatus ----

levels(dat$Species)
dat.decussatus.scott <- dat %>%
  filter(Species =="L. decussatus ")%>%
  filter(Site=="Scott Reefs")%>%
  glimpse()

plot.decussatus.scott <- ggplot(dat.decussatus.scott, aes(x="", y=Percentage, fill=Prey))+
  geom_bar(width = 1, stat = "identity")+ 
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("#F2D9BB", "#F8E5A0", "#F4D35E", "#A9570F", "#EE964B", 
                             "#74401D", "#3F292B","#022B3A", "#1F7A8C", "#BFDBF7" ))+
  ggtitle("Lutjanus decussatus")+
  guides(fill=guide_legend(title="Gut contents"))+
  blank_theme

plot.decussatus.scott


dat.decussatus.rowleys <- dat %>%
  filter(Species =="L. decussatus ")%>%
  filter(Site=="Rowley Shoals")%>%
  glimpse()

plot.decussatus.rowleys <- ggplot(dat.decussatus.rowleys, aes(x="", y=Percentage, fill=Prey))+
  geom_bar(width = 1, stat = "identity")+ 
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("#F2D9BB", "#F8E5A0", "#F4D35E", "#A9570F", "#EE964B", 
                             "#74401D", "#3F292B","#022B3A", "#1F7A8C", "#BFDBF7" ))+
  #ggtitle("Rowley Shoals")+
  guides(fill=guide_legend(title="Gut contents"))+
  blank_theme

plot.decussatus.rowleys


# L. bohar ----

levels(dat$Species)
dat.bohar.scott <- dat %>%
  filter(Species =="L. bohar")%>%
  filter(Site=="Scott Reefs")%>%
  glimpse()

plot.bohar.scott <- ggplot(dat.bohar.scott, aes(x="", y=Percentage, fill=Prey))+
  geom_bar(width = 1, stat = "identity")+ 
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("#F2D9BB", "#F8E5A0", "#F4D35E", "#A9570F", "#EE964B", 
                             "#74401D", "#3F292B","#022B3A", "#1F7A8C", "#BFDBF7" ))+
  ggtitle("Lutjanus bohar")+
  guides(fill=guide_legend(title="Gut contents"))+
  blank_theme

plot.bohar.scott


dat.bohar.rowleys <- dat %>%
  filter(Species =="L. bohar")%>%
  filter(Site=="Rowley Shoals")%>%
  glimpse()

plot.bohar.rowleys <- ggplot(dat.bohar.rowleys, aes(x="", y=Percentage, fill=Prey))+
  geom_bar(width = 1, stat = "identity")+ 
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("#F2D9BB", "#F8E5A0", "#F4D35E", "#A9570F", "#EE964B", 
                             "#74401D", "#3F292B","#022B3A", "#1F7A8C", "#BFDBF7" ))+
  #ggtitle("Rowley Shoals")+
  guides(fill=guide_legend(title="Gut contents"))+
  blank_theme

plot.bohar.rowleys

# combine plots ----

library(patchwork)


plots <- plot.gibbus.scott + plot.gibbus.rowleys + plot.decussatus.scott +
   plot.decussatus.rowleys + plot.bohar.scott + plot.bohar.rowleys + 
   plot_annotation(tag_levels = 'A') + plot_layout(ncol=2, guides = "collect") 
plots  

setwd (p.dir)
ggsave("Gut_contents.png", plots, width=15, height=30, units="cm")
