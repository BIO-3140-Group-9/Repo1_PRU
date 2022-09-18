#Project 1, Eugene Yang

library(ggplot2)
#CPK: No need to set the wd if you're working in an R project
setwd("D:/School/College/Org Bio/Scales")
library(tidyverse)

#CPK: These data are in a "module1 folder, so you'll have indicate that to load the data. I changed this to reflect. Alos use "<-" instead of "=". It's convention.
#1 - a dat variable containing the scales dataset.
dat = read.csv("./module1/Scales.csv")

#2 - a line of code which reports the class of each column in the dataset
sapply(dat,class)

#3 - a line of code which reports the dimensions of the dataset.
dim(dat)

dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species

#4 - Code that produces a summary of the number of specimens sampled for each species.
species.n<- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n

#5 - Code that produces a summary of the number of specimens sampled for each species.
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#6 - Code that produces a PDF file containing 6 figures, one for each species that includes a boxplot of puncture force versus quadrant.

#CPK: This is unneeded, part of task done below. [-1]
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}

#CPK: PDFs should have your name in file name [-1]

pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

#CPK: This is unneeded, too[-1]
list.files(pattern=".pdf")

#CPK: Really solid work. Just be sure to include only what's need, no more or less.
