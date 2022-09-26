library(tidyverse) 
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)

#1 establishing the anole.log tibble
anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()

anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)%>%
  print ()

#2 Plotting two simple linear model
anole.log.PH.lm <- lm(HTotal~SVL+PH,anole.log)
anole.log.ArbPD.lm <- lm(HTotal~SVL+ArbPD,anole.log)

anole.log%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_abline(slope=coef(anole.log.PH.lm)[2],intercept=coef(anole.log.PH.lm)[1],col="red")

anole.log%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_abline(slope=coef(anole.log.ArbPD.lm)[2],intercept=coef(anole.log.ArbPD.lm)[1],col="green")

#3 Plotting res against PH and ArbPD
anole.log.res <-anole.log%>%
  mutate(res.PH =residuals(anole.log.PH.lm))%>%
  mutate(res.ArbPD =residuals(anole.log.ArbPD.lm))%>%
  print()

anole.log.res%>%
  ggplot(aes(x=Ecomorph,y=res.PH, col=Ecomorph)) +geom_boxplot()+stat_summary(fun=mean, geom="point", size=3)
anole.log.res%>%
  ggplot(aes(x=Ecomorph,y=res.ArbPD, col=Ecomorph)) +geom_boxplot()+stat_summary(fun=mean, geom="point", size=3)

#4 Constructing PGLS model
anole.tree <- read.tree("anole.tre")

pgls.BM1.PH <- gls(HTotal ~SVL+PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

pgls.BM2.ArbPD <- gls(HTotal ~SVL+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

pgls.BM3.Both <- gls(HTotal ~SVL+PH+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#5 Analyzing PGLS model using AIC analysis
anole.phylo.aic <- AICc(pgls.BM1.PH,pgls.BM2.ArbPD,pgls.BM3.Both)
aicw(anole.phylo.aic$AICc)
#According to the AIC test, both PH and ArbPD together are significant predictor

anova(pgls.BM3.Both)

#6 


