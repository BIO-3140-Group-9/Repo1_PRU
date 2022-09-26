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

#CPK: This ^^ could have been done a little more concisely. . . .

pseed.wide <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

find.peaks <- function(x,y,mult=100){ 
f <- fget(features(x = x,y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}

pseed.sum <- pseed.wide%>%
  group_by(fish, speed)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)


#CPK: But for this ^^ we needed to group by date bc cycles refer to each experiment, not across them. [-1]


pseed.sum.max <- pseed.sum%>%
  group_by(fish,speed)%>%
  mutate(amp.sum.mean=mean(amp.sum))
#calculating maximum mean

stderror <- function(x) sd(x)/sqrt(length(x))
#function for standard error of mean

pseed.sum.max <- pseed.sum.max%>%
  mutate(amp.sum.se=stderror(pseed.sum.max$amp.sum.mean))


#CPK: No need to do the SEM calculation separately. [-1] You could have define the se function earlier and computed se at the same time as mean, a la.. . 

pseed.sum.max<- pseed.sum %>%
  group_by(fish, speed) %>%
  summarize(amp.sum.mean=mean(amp.sum),amp.sum.se = stderror(amp.sum))


pseed.sum.max[-c(9,10,12)]

#CPK: Unneeded

#final data entry containing mean of maximum and standard error

pd <- position_dodge(0.1)
amp.mean <- pseed.sum.max$amp.sum.mean
speed.fish <- pseed.sum.max$speed
se <- stderror(pseed.sum.max$amp.sum.mean)
species <- pseed.sum.max$fish

#CPK: Why all this ^^^? We have that info in a table. [-1]

ggplot(pseed.sum.max, aes(x= speed.fish, y=amp.mean, colour=species, group=species)) + 
  geom_errorbar(aes(ymin=amp.mean-se, ymax=amp.mean+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=1, shape=21, fill="white")

#CPK: Best to use the pipe here^^^

pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.rate)
pseed.sum.max[-c(9,10,12)]

mean.amp <- pseed.sum.max$amp.sum.mean
M.Rate <- pseed.sum.max$met.rate
species <- pseed.sum.max$fish

ggplot(pseed.sum.max, aes(x=mean.amp, y=M.Rate, colour=species))+geom_point()

#CPK: Why hard code in the things we already calculated and are in a table. We could simply . . .

pseed.sum.max%>%
  left_join(pseed.rate%>%
              group_by(fish,bl.s)%>%
              summarize(M.Rate=mean(met.rate)))%>%
  ggplot( aes(x=amp.sum.mean, y=M.Rate, colour=fish))+geom_point()

#CPK: Pretty good work. Just need to get a handle on using the pipe for concise operations.



