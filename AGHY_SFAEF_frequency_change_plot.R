## Title: AGHY SFAEF Endophyte Frequency Change
## Authors: Tom Miller and Marion Donald
## Purpose: Develop models for AGHY endophyte frequency change at the Plot level for Comparison with Tom's previous figures
## Date Started: September 8, 2016
## Date Updated: October 13, 2016
##########################################################################

library(xlsx)
library(plyr)
library(dplyr)
library(lme4)
library(bbmle)
library(R2jags)
library(mcmcplots)
invlogit<-function(x){exp(x)/(1+exp(x))}


## Tom's directory (laptop)
setwd("C:/Users/tm9/Dropbox/Research Projects/Endophytes/AGHY Nacogdoches experiment/AGHY_SFAEF_Project")

## (Tom) read in AGHY plot info
AGHY.plots<-read.xlsx("AGHY_SFAEF_life_history_expt.xlsx",
                           sheetName="Plot-level data")

## (Tom) read in AGHY agrinostics survey
AGHY.immunoblot<-read.xlsx("AGHY_SFAEF_life_history_expt.xlsx",
                           sheetName="Endophyte Survey")

## (Marion) set working directory
setwd("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/AGHY_SFAEF_Project")

## (Marion) read in AGHY plot info 
AGHY.plots<-read.xlsx("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/AGHY_SFAEF_Project/AGHY_SFAEF_life_history_expt.xlsx",
                      sheetName="Plot-level data")
## drop a few problem plots...these were planted incorrectly but we continued data collection by mistake
AGHY.plots<-AGHY.plots[AGHY.plots$plot!=143 & AGHY.plots$plot!=211,]

## (Marion) read in AGHY agrinostics survey
AGHY.immunoblot<-read.xlsx("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/AGHY_SFAEF_Project/AGHY_SFAEF_life_history_expt.xlsx",
                           sheetName="Endophyte Survey")

## match the agrinostic strip data to the year collected
AGHY.immunoblot<-AGHY.immunoblot[AGHY.immunoblot$year_t ==AGHY.immunoblot$strip,]
## check that we dropped the 2014 strips from 2015 data
which(AGHY.immunoblot$year_t != (AGHY.immunoblot$strip))

## Get the total E plus seeds scored per plot
AGHY<-ddply(AGHY.immunoblot, c("year_t","plot"), summarize, 
            total = length(agri_liberal),
            E_plus_liberal = sum(agri_liberal),
            E_plus_conservative = sum(agri_conservative))

## Create the frequency per plot
AGHY$con_freq<-AGHY$E_plus_conservative/AGHY$total
AGHY$lib_freq<-AGHY$E_plus_liberal/AGHY$total

## Merge these two types of info
AGHY.merge<-merge(AGHY.plots,AGHY,by="plot")


## select the relevant columns
AGHY.new<-AGHY.merge[, c("plot","water","target_init_freq",
                         "year_t","total","con_freq","lib_freq")]
## copy this dataframe into a new one so that year_t1 and the frequencies can be labeled
AGHY.freq.1<-AGHY.new
## create the year_t1 column from the year_t column
AGHY.freq.1$year_t1<-AGHY.freq.1$year_t+1
## rename the year_t frequencies
names(AGHY.freq.1)[names(AGHY.freq.1) == "con_freq"]<- "con_freq_t"
names(AGHY.freq.1)[names(AGHY.freq.1) == "lib_freq"]<- "lib_freq_t"
names(AGHY.freq.1)[names(AGHY.freq.1)=="total"]<-"total_scored_t"
## assign the year t to the year t1 to match with the AGHY.freq.1 dataframe (and do the same with total seeds scored)
AGHY.new$year_t1<-AGHY.new$year_t
names(AGHY.new)[names(AGHY.new) == "total"]<-"total_scored_t1"
## rename the year_t1 frequencies
names(AGHY.new)[names(AGHY.new) == "con_freq"]<- "con_freq_t1"
names(AGHY.new)[names(AGHY.new) == "lib_freq"]<- "lib_freq_t1"


## New data frame with years t and t+1 and their frequencies 
AGHY.total<-merge(AGHY.freq.1, AGHY.new[,c("plot","total_scored_t1","con_freq_t1","lib_freq_t1","year_t1")], by= c("plot", "year_t1"))
## re-organizing the columns so they make sense visually 
AGHY.total<-AGHY.total[,c(1,3:8,2,9:11)]

### Visualize 2013/14 endo freq change, fit Bayesian model to it

AGHY1314<-subset(AGHY.merge,year_t==2014)
AGHY1314$freq_t1_liberal<-AGHY1314$E_plus_liberal / AGHY1314$total
plot(AGHY1314$target_init_freq,
     AGHY1314$freq_t1_liberal)
abline(0,1)

#### Bayesian model for plot frequency change 13/14
## define data for Bayes
y<-AGHY1314$E_plus_liberal
init_freq<-AGHY1314$target_init_freq
N.obs<-length(y)
plot<-order(AGHY1314$plot)
#plots<-unique(plot)
N.plots<-length((plot))
x.levels<-seq(0,1,0.01)
N.x.levels<-length(x.levels)
N.samples<-AGHY1314$total

sink("AGHY1314_endochange.txt")
cat("
    model{
    
    ## Priors
    beta0.mean~dnorm(0,0.001)   ##hyperprior for intercept beta0
    sigma0~dunif(0,1000)
    tau.sigma0<-1/(sigma0*sigma0)
    
    beta1.mean~dnorm(0,0.001)   ##hyperprior for slope beta1
    sigma1~dunif(0,1000)
    tau.sigma1<-1/(sigma1*sigma1)
    
    for(i in 1:N.plots){      ##plot means
    beta0[i]~dnorm(beta0.mean,tau.sigma0)
    beta1[i]~dnorm(beta1.mean,tau.sigma1)
    }
    
    ## Likelihood
    for(i in 1:N.obs){
    logit(p[i])<-beta0[plot[i]]+beta1[plot[i]]*init_freq[i]
    y[i]~dbinom(p[i],N.samples[i])
    }
    
    ## Prediction
    beta0.pred~dnorm(beta0.mean,tau.sigma0)
    beta1.pred~dnorm(beta1.mean,tau.sigma1)
    
    for(i in 1:N.x.levels){
    y.pred[i]<-exp(beta0.pred+beta1.pred*x.levels[i])/(1+exp(beta0.pred+beta1.pred*x.levels[i]))
    }
    
    }##end model
    ",fill=T)
sink()


## bundle data
jag.data<-list(y=y,
               init_freq=init_freq,
               plot=plot,
               N.plots=N.plots,
               N.obs=N.obs,
               x.levels=x.levels,
               N.x.levels=N.x.levels,
               N.samples=N.samples)

## Inits function
inits<-function(){list(beta0=rnorm(N.plots,0,2),
                       beta1=rnorm(N.plots,0,2),
                       beta0.mean=rnorm(1,0,5),
                       beta1.mean=rnorm(1,0,5),
                       sigma0=rlnorm(1),
                       sigma1=rlnorm(1))}

## Params to estimate
parameters<-c("beta0.mean","beta1.mean","y.pred")


## MCMC settings
ni<-15000
nb<-5000
nt<-10
nc<-3

## run JAGS
AGHY1314.out<-jags(data=jag.data,inits=inits,parameters.to.save=parameters,model.file="AGHY1314_endochange.txt",
                      n.thin=nt,n.chains=nc,n.burnin=nb,n.iter=ni,DIC=T,working.directory=getwd())

mcmcplot(AGHY1314.out)

## plots Bayes fit against data
plot(AGHY1314$target_init_freq,
     AGHY1314$freq_t1_liberal)
abline(0,1)
lines(x.levels,
      invlogit(AGHY1314.out$BUGSoutput$mean$beta0.mean + AGHY1314.out$BUGSoutput$mean$beta1.mean*x.levels),lwd=4,col="red")
lines(x.levels,AGHY1314.out$BUGSoutput$summary[4:104,"2.5%"],type="l",lwd=2,col="red")
lines(x.levels,AGHY1314.out$BUGSoutput$summary[4:104,"97.5%"],type="l",lwd=2,col="red")

str(AGHY1314.out)

### example analysis for 2013/2014 change
#AGHY1314<-subset(AGHY.merge,year_t==2014)

## fit binomial model
#endo.model0<-glmer(cbind(E_plus_liberal,(total-E_plus_liberal)) ~ target_init_freq + (target_init_freq|plot),
#                   family="binomial", data=AGHY1314)
#endo.model1<-glmer(cbind(E_plus_liberal,(total-E_plus_liberal)) ~ target_init_freq + water + (target_init_freq|plot),
#                   family="binomial", data=AGHY1314)
#endo.model2<-glmer(cbind(E_plus_liberal,(total-E_plus_liberal)) ~ target_init_freq * water + (target_init_freq|plot),
#                   family="binomial", data=AGHY1314)
#AICtab(endo.model0,endo.model1,endo.model2,weights=T)

#AGHY1314$freq_t1_liberal<-AGHY1314$E_plus_liberal / AGHY1314$total
#plot(AGHY1314$target_init_freq,
#     AGHY1314$freq_t1_liberal)
#abline(0,1)
#lines(seq(0,1,0.01),
#      logit(fixef(endo.model1)[1] + fixef(endo.model1)[2]*seq(0,1,0.01)),
#      lwd=4,col="blue")
#lines(seq(0,1,0.01),
#      logit((fixef(endo.model1)[1] + fixef(endo.model1)[3]) + fixef(endo.model1)[2]*seq(0,1,0.01)),
#      lwd=4,col="red")


#logit(fixef(endo.model1)[1])
#logit(fixef(endo.model1)[1]+ fixef(endo.model1)[3])

#plot(1:10,logit(0+0.5*(1:10)),type="l")
#lines(1:10,logit(+1+0.5*(1:10)),type="l",col="red")

#par(mfrow=c(2,1))
#hist(AGHY1314$freq_t1_liberal[AGHY1314$water=="Add"])
#hist(AGHY1314$freq_t1_liberal[AGHY1314$water=="Control"])
#dev.off()

## select just the 2014-2015 data
AGHY1415<-subset(AGHY.total,year_t==2014)

## fit a binomial water to the E+ freq in 2015 dependent on E+ freq 2014, water, and weighted by sample size (# seeds scored) at the plot level

endo.model.14.0<-glm(lib_freq_t1 ~ lib_freq_t, weights = total_scored_t1, family = "binomial", data=AGHY1415)
endo.model.14.1<-glm(lib_freq_t1 ~ lib_freq_t + water, weights = total_scored_t1, family = "binomial", data=AGHY1415)
endo.model.14.2<-glm(lib_freq_t1 ~ lib_freq_t * water, weights = total_scored_t1, family = "binomial", data=AGHY1415)
AICtab(endo.model.14.0,endo.model.14.1,endo.model.14.2,weights=T)


plot(AGHY1415$lib_freq_t,
     AGHY1415$lib_freq_t1,type="n")
points(AGHY1415$lib_freq_t[AGHY1415$water=="Control"],
       AGHY1415$lib_freq_t1[AGHY1415$water=="Control"],pch=21,bg="red")
points(AGHY1415$lib_freq_t[AGHY1415$water=="Add"],
       AGHY1415$lib_freq_t1[AGHY1415$water=="Add"],pch=21,bg="blue")
abline(0,1)
#lines(seq(0,1,0.01),
 #     logit(fixef(endo.model.14.2)[1] + fixef(endo.model.14.2)[2]*seq(0,1,0.01)),
  #    lwd=4,col="blue")
#lines(seq(0,1,0.01),
 #     logit((fixef(endo.model.14.2)[1] + fixef(endo.model.14.2)[3]) + (fixef(endo.model.14.2)[2] + fixef(endo.model.14.2)[4])*seq(0,1,0.01)),
  #    lwd=4,col="red")


#logit(fixef(endo.model.14.2)[1])
#logit(fixef(endo.model.14.2)[1]+ fixef(endo.model.15.1)[3])

#plot(1:10,logit(0+0.5*(1:10)),type="l")
#lines(1:10,logit(+1+0.5*(1:10)),type="l",col="red")

#par(mfrow=c(2,1))
#hist(AGHY1415$lib_freq_t1[AGHY1415$water=="Add"])
#hist(AGHY1415$lib_freq_t1[AGHY1415$water=="Control"])


## fit a binomial water to the E+ freq in 2016 dependent on E+ freq 2015, water, and weighted by sample size (# seeds scored) at the plot level
AGHY1516<-subset(AGHY.total,year_t==2015)
endo.model.15.0<-glm(lib_freq_t1 ~ lib_freq_t, weights = total_scored_t1, family = "binomial", data=AGHY1516)
endo.model.15.1<-glm(lib_freq_t1 ~ lib_freq_t + water, weights = total_scored_t1, family = "binomial", data=AGHY1516)
endo.model.15.2<-glm(lib_freq_t1 ~ lib_freq_t * water, weights = total_scored_t1, family = "binomial", data=AGHY1516)
AICtab(endo.model.15.0,endo.model.15.1,endo.model.15.2)

plot(AGHY1516$lib_freq_t,
     AGHY1516$lib_freq_t1)
abline(0,1)
#lines(seq(0,1,0.01),
 #     logit(fixef(endo.model.15.1)[1] + fixef(endo.model.15.1)[2]*seq(0,1,0.01)),
  #    lwd=4,col="blue")
#lines(seq(0,1,0.01),
 #     logit((fixef(endo.model.15.1)[1] + fixef(endo.model.15.1)[3]) + fixef(endo.model.15.1)[2]*seq(0,1,0.01)),
  #    lwd=4,col="red")

plot(AGHY1516$lib_freq_t,
     AGHY1516$lib_freq_t1,type="n")
points(AGHY1516$lib_freq_t[AGHY1516$water=="Control"],
       AGHY1516$lib_freq_t1[AGHY1516$water=="Control"],pch=21,bg="red")
points(AGHY1516$lib_freq_t[AGHY1516$water=="Add"],
       AGHY1516$lib_freq_t1[AGHY1516$water=="Add"],pch=21,bg="blue")
abline(0,1)


#logit(fixef(endo.model.15.1)[1])
#logit(fixef(endo.model.15.1)[1]+ fixef(endo.model.15.1)[3])

#plot(1:10,logit(0+0.5*(1:10)),type="l")
#lines(1:10,logit(+1+0.5*(1:10)),type="l",col="red")

#par(mfrow=c(2,1))

#hist(AGHY1516$lib_freq_t1[AGHY1516$water=="Add"])
#hist(AGHY1516$lib_freq_t1[AGHY1516$water=="Control"])
#dev.off()
