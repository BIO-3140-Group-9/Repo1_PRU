library(tidyverse) 
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)

anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()

anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)%>%
  print ()

anole.log.PH.lm <- lm(HTotal~SVL+PH,anole.log)
anole.log.ArbPD.lm <- lm(HTotal~SVL+ArbPD,anole.log)

anole.log%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_abline(slope=coef(anole.log.PH.lm)[2],intercept=coef(anole.log.PH.lm)[1],col="blue")

anole.log%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_abline(slope=coef(anole.log.ArbPD.lm)[2],intercept=coef(anole.log.ArbPD.lm)[1],col="blue")

anole.log.res <-anole.log%>%
  mutate(res=residuals(anole.log.PH.lm))%>%
  mutate(res=residuals(anole.log.ArbPD.lm))%>%
  print()

anole.log.res%>%
  ggplot(aes(res,SVL))+geom_point()+geom_abline(slope=coef(anole.log.PH.lm)[2],intercept=coef(anole.log.PH.lm)[1],col="blue")

anole.log.res%>%
  ggplot(aes(HTotal,res))+geom_point()+geom_abline(slope=coef(anole.log.ArbPD.lm)[2],intercept=coef(anole.log.ArbPD.lm)[1],col="blue")