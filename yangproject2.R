library(tidyverse)
library(ggplot2)
library(features)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

#1. Making pseed.wide

pseed2 <- pseed%>%
  left_join(speeds,by = c("speed"="vol"))%>%
  left_join(pseed.bl,by = "fish")%>%
  mutate(bl.s = cm.s/bl)

pseed.wide <- pseed2%>%
  select(-amp)%>%
  pivot_wider(names_from = fin, values_from = amp.bl)%>%
  mutate(amp.sum = L+R)

#2. Mean Maximum of the amp.sums for each specific swimming speed for each fish.

find.peaks <- function(x,y,mult = 100){
  f <- fget(features(x = x,y = y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks = round(crit.pts,0))
  return(f$peaks) 
}

pseed.max <- pseed.wide%>%
  group_by(date, speed)%>%
  mutate(peak = frame %in% find.peaks(frame, amp.sum))%>%
  filter(peak == T)
pseed.max$peak <- NULL

p.seed.max <- pseed.max%>%
  group_by(fish, speed)%>%
  summarize(amp.sum.mean = mean(amp.sum))

#3. Custom function that compares the standard error of the mean

SE <- function(x){
  sd(x)/sqrt(length(x))
}

pseed.sum.se <- pseed.max%>%
  group_by(fish, speed)%>%
  summary(amp.sum.se = SE(amp.sum))

pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.sum.se, by = c("speed", "fish"))%>%
  
  #4. Use ggplot to plot the amp.sum mean vs specific swimming speed with error bars
  
  pd <- position_dodge(0.1)

ggplot(pseed.sum.max, aes(x = speed, y = amp.sum.mean, col = fish)) +
  geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se), colour = "black", width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 2, shape = 21, fill = "white") +
  theme_classic()

#5. Read and merge pseed.met.rate with pseed.sum.max

pseed.met.rate <- read_csv("pseed.met.rate.csv")
pseed.max <- pseed.max%>%
  merge(pseed.met.rate, by = c("fish", "date", "m.s", "cm.s", "bl.s"))

pseed.mean.rate <- pseed.max%>%
  group_by(fish, speed)%>%
  summarize(amp.met.rate = mean(met.rate))

pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.mean.rate, by = c("speed", "fish"))

#6. Plot of the metabolic power output of each fish vs. mean maximum of amp.sum

ggplot(pseed.sum.max, aes(x = amp.sum.mean, y = met.rate, col = fish)) +
  geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se), colour = "black", width = .1, position = pd) +
  geom_point() + 
  theme_classic()