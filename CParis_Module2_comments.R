library(tidyverse)
library(features)

#data needed
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
pseed.rate <- read.csv("pseed.met.rate.csv")

#adding data to one table
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

#making one row for L&R instead of two
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


pseed.sum.max <- pseed.wide%>%
  group_by(fish, speed)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)%>%
  print()

#CPK: But for this ^^ we needed to group by date bc cycles refer to each experiment, not across them. [-1]



# adding mean column
pseed.sum.max <- pseed.sum.max%>%
  mutate(amp.sum.mean=mean(amp.sum))

#making standard error of the mean function
standard.error <- function(x) {sd(x)/sqrt(length(x))}

#adding standard error to tibble
pseed.sum.max <- pseed.sum.max%>%
  mutate(amp.sum.se=standard.error(pseed.sum.max$amp.sum.mean))

#CPK: No need to do the SEM calculation separately. [-1] You could have define the se function earlier and computed se at the same time as mean, a la.. . 

pseed.sum.max<- pseed.sum %>%
  group_by(fish, speed) %>%
  summarize(amp.sum.mean=mean(amp.sum),amp.sum.se = standard.error(amp.sum))

#to make sure error doesn't overlap
pd <- position_dodge(0.1)

#defining se
se <- standard.error(pseed.sum.max$amp.sum.mean)

#plotting with error bars
ggplot(pseed.sum.max, aes(x= speed, y=amp.sum.mean, colour=fish, group=fish)) + 
  geom_errorbar(aes(ymin=amp.sum.mean-se, ymax=amp.sum.mean+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=1)

#CPK: no need to store values, just use what's been added to a tibble and use the pipe.

#adding met rate to tibble
pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.rate)

#plot of met rate and amp sum
ggplot(pseed.sum.max, aes(x=amp.sum.mean, y=met.rate, colour=fish))+geom_point()

#CPK: Best to use the pipe^^

#CPK: Really good work. Just need to get a handle on using the pipe for concise operations.

