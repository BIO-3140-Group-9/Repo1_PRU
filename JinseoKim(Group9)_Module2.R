library(tidyverse)
library(ggplot2)
library(features)
library(lokern)

pseed <- read.csv("pseed.fin.amps.csv")
pseed.bl <- read.csv("pseed.lengths.csv")
speeds <- read.csv("pseed.calibration.csv")
pseed.rate <- read.csv("pseed.met.rate.csv")

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)

pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print()
#tibble set up

find.peaks <- function(x,y,mult=100){ 
f <- fget(features(x = x,y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}
#function to find peaks

pseed.peak<- pseed.wide%>%
  group_by(fish, speed)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)
#variable that filters peak of all amplitude of sum after being grouped by fish and speed

pseed.sum.max <- pseed.peak%>%
  group_by(fish, speed)%>%
  summarize(amp.sum.mean=mean(amp.sum))
#calculating maximum mean by fish and specific speed

stderror <- function(x) sd(x)/sqrt(length(x))
#custom function for standard error of mean

amp.sum.se <- pseed.peak %>%
  group_by(fish, speed)%>%
  summarize(amp.sum.se=stderror(amp.sum))
#calculating standard error of amp.sum

pseed.sum.max <- pseed.sum.max%>%
  left_join(amp.sum.se, by = c("speed", "fish"))
#adding standard of error to wide tibble

pseed.peak <- pseed.peak%>%
  merge(pseed.rate, by = c("fish", "date", "m.s", "cm.s", "bl.s"))

pseed.met.rate <- pseed.peak%>%
  group_by(fish, speed)%>%
  summarize(metabolic.rate=mean(met.rate))
#grouping met.rate by fish and speed. summarizing the met.rate at each maximum mean

pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.met.rate, by = c("fish", "speed"))
view(pseed.sum.max)
#final wide tibble including amp.sum.mean, amp.sum.se, and metabolic rate

pd <- position_dodge(0.1)
amp.mean <- pseed.sum.max$amp.sum.mean
speed.fish <- pseed.sum.max$speed
se <- stderror(pseed.sum.max$amp.sum.mean)
species <- pseed.sum.max$fish
M.Rate <- pseed.sum.max$metabolic.rate
#variable for each specific subset from pseed.sumn.max

ggplot(pseed.sum.max, aes(x= speed.fish, y=amp.mean, colour=species, group=species)) + 
  geom_errorbar(aes(ymin=amp.mean-se, ymax=amp.mean+se), colour="black", width=.1, position=pd) +
  geom_smooth(method="lm") +
  geom_point(position=pd, size=1, shape=21, fill="white") +
  xlab("Speed of Fish") +
  ylab("Mean of Maximum Amplitude") +
  ggtitle("Mean Maximum Amplitude based on Specific Speed")
##plot of mean max amplitude and spcific speed along with standard of error

ggplot(pseed.sum.max, aes(x= amp.mean, y=M.Rate, colour=species, group=species)) + 
  geom_point() +
  xlab("Mean of Maximum Amplitude") +
  ylab("Metabolic Rate") +
  ggtitle("Metabolic Rate at each Mean Maximum Amplitude")
#plot of mean max amplitude and m rate
