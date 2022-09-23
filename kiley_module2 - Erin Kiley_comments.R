library(tidyverse)
library(ggplot2)
library(features)
library(lokern)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
pseed.rate <- read.csv("pseed.met.rate.csv")

#1

pseed2 <- pseed%>%
  left_join(speeds,by = c("speed"="vol"))%>%
  left_join(pseed.bl,by = "fish")%>%
  mutate(bl.s = cm.s/bl)

pseed.wide <- pseed2%>%
  select(-amp)%>%
  pivot_wider(names_from = fin, values_from = amp.bl)%>%
  mutate(amp.sum = L+R)
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
#2

find.peaks <- function(x,y,mult = 100){
  f <- fget(features(x = x,y = y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks = round(crit.pts,0))
  return(f$peaks) 
}

pseed.max <- pseed.wide%>%
  group_by(fish, speed)%>%
  mutate(peak = frame %in% find.peaks(frame, amp.sum))%>%
  filter(peak == T)

#CPK: But for this ^^ we needed to group by date bc cycles refer to each experiment, not across them. [-1]


pseed.sum.max <- pseed.max%>%
  group_by(fish, speed)%>%
  summarize(amp.sum.mean = mean(amp.sum))

#3

pseed.sum.max <- pseed.sum.max%>%
  mutate(amp.sum.se=stderror(pseed.sum.max$amp.sum.mean))
pseed.sum.max[-c(9,10,12)]
#CPK: Unneeded^

#CPK: No need to do the SEM calculation separately. [-1] You could have define the se function earlier and computed se at the same time as mean, a la.. . 

pseed.sum.max<- pseed.sum %>%
  group_by(fish, speed) %>%
  summarize(amp.sum.mean=mean(amp.sum),amp.sum.se = stderror(amp.sum))

#4. Use ggplot to plot the amp.sum mean vs specific swimming speed with error bars

pd <- position_dodge(0.1)

ggplot(pseed.sum.max, aes(x = speed, y = amp.sum.mean, col = fish)) +
  geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se), colour = "black", width = .5, position = pd) +
  geom_smooth(method = "lm") +
  geom_point(position = pd, size = 2, shape = 21, fill = "white") +
  theme_classic()

#CPK: Best to use the pipe here and color by fish, right?

#5. Read and merge pseed.met.rate with pseed.sum.max

pseed.met.rate <- read_csv("pseed.met.rate.csv")
pseed.max <- pseed.max%>%
  merge(pseed.met.rate, by = c("fish", "date", "m.s", "cm.s", "bl.s"))

pseed.mean.rate <- pseed.max%>%
  group_by(fish, speed)%>%
  summarize(amp.met.rate = mean(met.rate))

pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.mean.rate, by = c("speed", "fish"))


#CPK: No need specify "on" when there are common columns. Best to use left_join too.

#6. Plot of the metabolic power output of each fish vs. mean maximum of amp.sum

ggplot(pseed.sum.max, aes(x = amp.met.rate, y = amp.sum.mean, col = fish)) +
  geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se), colour = "black", width = .03, position = pd) +
  geom_smooth(method = "lm") +
  geom_point(position = pd, size = 2, shape = 21, fill = "white") + 
  theme_classic()


#CPK: ^^Best to use the pipe.

#CPK: Really solid work! Just need to be a little more concise and know when to use the pipe. I see you submitted over google (the old way from last year). Sorry for the confusion.
