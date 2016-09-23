## Title: AGHY SFAEF Endophyte Frequency Change
## Authors: Tom Miller and Marion Donald
## Purpose: Develop models for AGHY endophyte frequency change 
## Date Started: September 8, 2016
## Date Updated: September 15, 2016
##########################################################################

library(xlsx)
library(plyr)
library(dplyr)
library(lme4)
library(bbmle)
logit<-function(x){exp(x)/(1+exp(x))}

## (Marion) set working directory
setwd("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/AGHY_SFAEF_Project")

## (Tom) read in AGHY plot info
AGHY.plots<-read.xlsx("D:\\Dropbox\\Lab Documents\\Documents\\Grass\\SFAEF life history experiment\\Data\\AGHY_SFAEF_life_history_expt.xlsx",
                           sheetName="Plot-level data")

## (Tom) read in AGHY agrinostics survey
AGHY.immunoblot<-read.xlsx("D:\\Dropbox\\Lab Documents\\Documents\\Grass\\SFAEF life history experiment\\Data\\AGHY_SFAEF_life_history_expt.xlsx",
                           sheetName="Endophyte Survey")

## (Marion) read in AGHY plot info 
AGHY.plots<-read.xlsx("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/AGHY_SFAEF_Project/AGHY_SFAEF_life_history_expt.xlsx",
                      sheetName="Plot-level data")

## (Marion) read in AGHY agrinostics survey
AGHY.immunoblot<-read.xlsx("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/AGHY_SFAEF_Project/AGHY_SFAEF_life_history_expt.xlsx",
                           sheetName="Endophyte Survey")



## apply function to get subplot-level frequency
AGHY<-ddply(AGHY.immunoblot, c("year_t","plot", "subplot"), summarize, 
            total = length(agri_liberal),
            E_plus_liberal = sum(agri_liberal),
            E_plus_conservative = sum(agri_conservative))

## Create the frequency per subplot (Conservative/total and Liberal/total)
AGHY$con_freq<-AGHY$E_plus_conservative/AGHY$total
AGHY$lib_freq<-AGHY$E_plus_liberal/AGHY$total

## Merge these two types of info
AGHY.merge<-merge(AGHY.plots,AGHY,by="plot")

## create a unique ID of plot and subplot
AGHY.merge$ID<- paste(AGHY.merge$plot, AGHY.merge$subplot, sep ="_")

## select the relevant columns
AGHY.new<-AGHY.merge[, c(1,4,5,6,9,10,14:16)]
## copy this dataframe into a new one so that year_t1 and the frequencies can be labeled
AGHY.freq.1<-AGHY.new
## creat the year_t1 column from the year_t column
AGHY.freq.1$year_t1<-AGHY.freq.1$year_t+1
## rename the year_t frequencies
names(AGHY.freq.1)[names(AGHY.freq.1) == "con_freq"]<- "con_freq_t"
names(AGHY.freq.1)[names(AGHY.freq.1) == "lib_freq"]<- "lib_freq_t"
## assign the year t to the year t1 to match with the AGHY.freq.1 dataframe
AGHY.new$year_t1<-AGHY.new$year_t
## rename the year_t1 frequencies
names(AGHY.new)[names(AGHY.new) == "con_freq"]<- "con_freq_t1"
names(AGHY.new)[names(AGHY.new) == "lib_freq"]<- "lib_freq_t1"

## New data frame with years t and t+1 and their frequencies (2013 and 2016 as year_t are not in this -- should they be added?)
AGHY.total<-merge(AGHY.freq.1, AGHY.new[,c(7:10)], by =c("ID", "year_t1"))
AGHY.total<-AGHY.total[,c(1,3,8,4:7,2,9:12)]

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
