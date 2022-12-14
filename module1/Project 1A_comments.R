#Set Up
library(ggplot2)
library(tidyverse)



#1 a dat variable containing the scales dataset 
dat <- read.csv("scales.csv") 

#2 A line of code which reports the class of each column in  the dataset. 
sapply(dat,class)

#3 A line of code which reports the dimensions of the dataset.
dim(dat)

#4 Code that produces a summary of the number of scales punctured for each species.
species.n<- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n

#5 Code that produces a summary of the number of specimens sampled for each species.
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#6 Code that produces a PDF file containing 6 figures, one for each species that includes a boxplot of puncture force verus quadrant.
dat$species <- as.factor(dat$species)
species <- levels(dat$species)

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

