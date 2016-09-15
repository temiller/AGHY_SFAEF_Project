##
##
##
##########################################################################

library(xlsx)
library(dplyr)
library(plyr)
library(lme4)
library(bbmle)
logit<-function(x){exp(x)/(1+exp(x))}

## read in AGHY plot info
AGHY.plots<-read.xlsx("D:\\Dropbox\\Lab Documents\\Documents\\Grass\\SFAEF life history experiment\\Data\\AGHY_SFAEF_life_history_expt.xlsx",
                           sheetName="Plot-level data")

## read in AGHY agrinostics survey
AGHY.immunoblot<-read.xlsx("D:\\Dropbox\\Lab Documents\\Documents\\Grass\\SFAEF life history experiment\\Data\\AGHY_SFAEF_life_history_expt.xlsx",
                           sheetName="Endophyte Survey")

## apply function to get subplot-level frequency
AGHY<-ddply(AGHY.immunoblot, c("year_t","plot", "subplot"), summarize, 
            total = length(agri_liberal),
            E_plus_liberal = sum(agri_liberal),
            E_plus_conservative = sum(agri_conservative))


## Merge these two types of info
AGHY.merge<-merge(AGHY.plots,AGHY,by="plot")


### example analysis for 2013/2014 change
AGHY1314<-subset(AGHY.merge,year_t==2014)

## fit binomial model
endo.model0<-glmer(cbind(E_plus_liberal,(total-E_plus_liberal)) ~ target_init_freq + (target_init_freq|plot),
                   family="binomial", data=AGHY1314)
endo.model1<-glmer(cbind(E_plus_liberal,(total-E_plus_liberal)) ~ target_init_freq + water + (target_init_freq|plot),
                   family="binomial", data=AGHY1314)
endo.model2<-glmer(cbind(E_plus_liberal,(total-E_plus_liberal)) ~ target_init_freq * water + (target_init_freq|plot),
                   family="binomial", data=AGHY1314)
AICtab(endo.model0,endo.model1,endo.model2)

AGHY1314$freq_t1_liberal<-AGHY1314$E_plus_liberal / AGHY1314$total
plot(AGHY1314$target_init_freq,
     AGHY1314$freq_t1_liberal)
abline(0,1)
lines(seq(0,1,0.01),
      logit(fixef(endo.model1)[1] + fixef(endo.model1)[2]*seq(0,1,0.01)),
      lwd=4,col="blue")
lines(seq(0,1,0.01),
      logit((fixef(endo.model1)[1] + fixef(endo.model1)[3]) + fixef(endo.model1)[2]*seq(0,1,0.01)),
      lwd=4,col="red")


logit(fixef(endo.model1)[1])
logit(fixef(endo.model1)[1]+ fixef(endo.model1)[3])

plot(1:10,logit(0+0.5*(1:10)),type="l")
lines(1:10,logit(+1+0.5*(1:10)),type="l",col="red")

par(mfrow=c(2,1))
hist(AGHY1314$freq_t1_liberal[AGHY1314$water=="Add"])
hist(AGHY1314$freq_t1_liberal[AGHY1314$water=="Control"])

### 
AGHY1415<-subset(AGHY.merge,year_t==2015)
