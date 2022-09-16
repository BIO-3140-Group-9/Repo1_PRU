library(tidyverse)
library(ggplot2)

pseed <- read.csv("pseed.fin.amps.csv")
pseed.bl <- read.csv("pseed.lengths.csv")
speeds <- read.csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

pseed.sum.max <- pseed.wide%>%
  mutate(amp.sum.mean=amp.sum/2)

stderror <- function(x) sd(x)/sqrt(length(x))

pseed.sum.max%>%
  mutate(amp.sum.se=stderror(pseed.sum.max$amp.sum))%>%
  print()





