# Project Report Pts
library(ggplot2)
library(tidyverse)

#Report Pt 1
dat <- read.csv("scales.csv")

#Report Pt 2
sapply(dat, class)
#N=force, Quadrant=where on body (UR, UL, LR, LL)

#Report Pt 3
dim(dat)

dat$species <- as.factor(dat$species)
species <- levels(dat$species)

#Report Pt 4
species.n <- dat %>%
  group_by(species) %>%
  summarise(n=n())
species.n

#Report Pt 5
species.s <- dat %>%
  count(species, specimen) %>%
  print () %>%
  count (species, name='n.specimens')
species.s

#CPK: PDFs should have your name in file name [-1]
#Report Pt 6
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
