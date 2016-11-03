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
library(mvtnorm)
invlogit<-function(x){exp(x)/(1+exp(x))}

##############################################################
## Tom's directory (laptop)
setwd("C:/Users/tm9/Dropbox/Research Projects/Endophytes/AGHY Nacogdoches experiment/AGHY_SFAEF_Project")
## (Tom) read in AGHY plot info
AGHY.plots<-read.xlsx("AGHY_SFAEF_life_history_expt.xlsx",
                           sheetName="Plot-level data")
## (Tom) read in AGHY agrinostics survey
AGHY.immunoblot<-read.xlsx("AGHY_SFAEF_life_history_expt.xlsx",
                           sheetName="Endophyte Survey")
#######################################################################################
## (Marion) set working directory
setwd("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/AGHY_SFAEF_Project")
## (Marion) read in AGHY plot info 
AGHY.plots<-read.xlsx("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/AGHY_SFAEF_Project/AGHY_SFAEF_life_history_expt.xlsx",
                      sheetName="Plot-level data")
## (Marion) read in AGHY agrinostics survey
AGHY.immunoblot<-read.xlsx("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/AGHY_SFAEF_Project/AGHY_SFAEF_life_history_expt.xlsx",
                           sheetName="Endophyte Survey")
####################################################################
## Once user-specific read-in is done, pick up here
## drop farm plots
AGHY.plots<-AGHY.plots[AGHY.plots$transmission!="Farm",]
## drop weird NAs
AGHY.plots<-AGHY.plots[!is.na(AGHY.plots$plot),]
## drop a few problem plots...these were planted incorrectly but we continued data collection by mistake
AGHY.plots<-AGHY.plots[AGHY.plots$plot!=143 & AGHY.plots$plot!=211,]
AGHY.plots$newplot<-order(AGHY.plots$plot)

## match the agrinostic strip data to the year collected
AGHY.immunoblot<-AGHY.immunoblot[AGHY.immunoblot$year_t ==AGHY.immunoblot$strip,]
## check that we dropped the 2014 strips from 2015 data
which(AGHY.immunoblot$year_t != (AGHY.immunoblot$strip))

## Get the total E plus seeds scored per plot
## Big change here. Last version did not track subplots
AGHY<-ddply(AGHY.immunoblot, c("year_t","plot","subplot"), summarize, 
            total = length(agri_liberal),
            E_plus_liberal = sum(agri_liberal),
            E_plus_conservative = sum(agri_conservative))
## there is one subplot labeled "extra". Drop this.
AGHY<-AGHY[-which(AGHY$subplot=="EXTRA"),]
## coerce subplot back to integer
AGHY$subplot<-as.integer(AGHY$subplot)

## Estimate frequency. Note these are by subplot.
AGHY$con_freq<-AGHY$E_plus_conservative/AGHY$total
AGHY$lib_freq<-AGHY$E_plus_liberal/AGHY$total

## Merge these two types of info
AGHY.merge<-merge(AGHY.plots,AGHY,by="plot")
AGHY.merge$lib_freq<-AGHY.merge$E_plus_liberal/AGHY.merge$total


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

## Bayesian model for endo frequency change

sink("AGHY_endochange.txt")
cat("
    model{
    ## Priors
    ## parameters for fixed effects of year and trt on intercept and slope
    for(i in 1:N.trt){
    beta0.mean.14[i]~dnorm(0,0.001)   ##hyperprior for intercept beta0
    beta1.14[i]~dnorm(0,0.001)   ##prior for slope beta1

    beta0.mean.15[i]~dnorm(0,0.001)   ##hyperprior for intercept beta0
    beta1.15[i]~dnorm(0,0.001)   ##prior for slope beta1

    beta0.mean.16[i]~dnorm(0,0.001)   ##hyperprior for intercept beta0
    beta1.16[i]~dnorm(0,0.001)   ##prior for slope beta1
    }

    ## random effect of plot, only affects intercept and consistent across years and trts
    sigma0~dunif(0,1000)
    tau.sigma0<-1/(sigma0*sigma0)

    for(i in 1:N.plots){      ##plot means
    ran.beta0.14[i]~dnorm(0,tau.sigma0)
    ran.beta0.15[i]~dnorm(0,tau.sigma0)
    ran.beta0.16[i]~dnorm(0,tau.sigma0)
    }
    
    ## Likelihood
    ## expected E+ freq for each plot
    for(i in 1:N.plots){
    logit(p.14[i])<-beta0.mean.14[water[i]]+ran.beta0.14[i]+beta1.14[water[i]]*init_freq[i]
    logit(p.15[i])<-beta0.mean.15[water[i]]+ran.beta0.15[i]+beta1.15[water[i]]*p.14[i]
    logit(p.16[i])<-beta0.mean.16[water[i]]+ran.beta0.16[i]+beta1.16[water[i]]*p.15[i]
    }

    ## sample likelihood oversubplots for 1314 transition
    for(i in 1:N.obs.14){
    y.14[i]~dbinom(p.14[y.14.plot[i]],N.samples.14[i])
    }
    ## sample likelihood oversubplots for 1415 transition
    for(i in 1:N.obs.15){
    y.15[i]~dbinom(p.15[y.15.plot[i]],N.samples.15[i])
    }
    ## sample likelihood oversubplots for 1415 transition
    for(i in 1:N.obs.16){
    y.16[i]~dbinom(p.16[y.16.plot[i]],N.samples.16[i])
    }

    ## Prediction
    for(i in 1:N.x.levels){
    Eplus14.add.pred[i]<-exp(beta0.mean.14[1]+beta1.14[1]*x.levels[i])/(1+exp(beta0.mean.14[1]+beta1.14[1]*x.levels[i]))
    Eplus14.control.pred[i]<-exp(beta0.mean.14[2]+beta1.14[2]*x.levels[i])/(1+exp(beta0.mean.14[2]+beta1.14[2]*x.levels[i]))
    Eplus15.add.pred[i]<-exp(beta0.mean.15[1]+beta1.15[1]*x.levels[i])/(1+exp(beta0.mean.15[1]+beta1.15[1]*x.levels[i]))
    Eplus15.control.pred[i]<-exp(beta0.mean.15[2]+beta1.15[2]*x.levels[i])/(1+exp(beta0.mean.15[2]+beta1.15[2]*x.levels[i]))
    Eplus16.add.pred[i]<-exp(beta0.mean.16[1]+beta1.16[1]*x.levels[i])/(1+exp(beta0.mean.16[1]+beta1.16[1]*x.levels[i]))
    Eplus16.control.pred[i]<-exp(beta0.mean.16[2]+beta1.16[2]*x.levels[i])/(1+exp(beta0.mean.16[2]+beta1.16[2]*x.levels[i]))
    }

    }##end model
    ",fill=T)
sink()


## bundle data
#### Bayesian model for plot frequency change 13/14/15
## define data for Bayes
N.yrs<-2
N.trt<-2

## here are the plot data
water<-as.integer(AGHY.plots$water[!is.na(AGHY.plots$plot)])
N.plots<-length(water)
init_freq<-AGHY.plots$target_init_freq[!is.na(AGHY.plots$plot)]

##subplots E+ scores
y.14<-AGHY.merge$E_plus_liberal[AGHY.merge$year_t==2014]
N.samples.14<-AGHY.merge$total[AGHY.merge$year_t==2014]
N.obs.14<-length(y.14)
y.14.plot<-AGHY.merge$newplot[AGHY.merge$year_t==2014]

y.15<-AGHY.merge$E_plus_liberal[AGHY.merge$year_t==2015]
N.samples.15<-AGHY.merge$total[AGHY.merge$year_t==2015]
N.obs.15<-length(y.15)
y.15.plot<-AGHY.merge$newplot[AGHY.merge$year_t==2015]

y.16<-AGHY.merge$E_plus_liberal[AGHY.merge$year_t==2016]
N.samples.16<-AGHY.merge$total[AGHY.merge$year_t==2016]
N.obs.16<-length(y.16)
y.16.plot<-AGHY.merge$newplot[AGHY.merge$year_t==2016]

x.levels<-seq(0,1,0.01)
N.x.levels<-length(x.levels)

jag.data<-list(N.trt=N.trt,
               water=water,
               N.plots=N.plots,
               init_freq=init_freq,
               y.14=y.14,
               N.samples.14=N.samples.14,
               N.obs.14=N.obs.14,
               y.14.plot=y.14.plot,
               y.15=y.15,
               N.samples.15=N.samples.15,
               N.obs.15=N.obs.15,
               y.15.plot=y.15.plot,
               y.16=y.16,
               N.samples.16=N.samples.16,
               N.obs.16=N.obs.16,
               y.16.plot=y.16.plot,
               x.levels=x.levels,
               N.x.levels=N.x.levels)

## Inits function
inits<-function(){list(beta0.mean.14=rnorm(2,0,2),
                       beta1.14=rnorm(2,0,2),
                       beta0.mean.15=rnorm(2,0,2),
                       beta1.15=rnorm(2,0,2),
                       beta0.mean.16=rnorm(2,0,2),
                       beta1.16=rnorm(2,0,2),
                       sigma0=rlnorm(1))}

## Params to estimate
parameters<-c("beta0.mean.14","beta1.14",
              "beta0.mean.15","beta1.15",
              "beta0.mean.16","beta1.16",
              "p.14","p.15","p.16",
              "Eplus14.add.pred","Eplus14.control.pred",
              "Eplus15.add.pred","Eplus15.control.pred",
              "Eplus16.add.pred","Eplus16.control.pred")


## MCMC settings
ni<-15000
nb<-5000
nt<-10
nc<-3

## run JAGS
AGHY.endochange.out<-jags(data=jag.data,inits=inits,parameters.to.save=parameters,model.file="AGHY_endochange.txt",
                      n.thin=nt,n.chains=nc,n.burnin=nb,n.iter=ni,DIC=T,working.directory=getwd())
mcmcplot(AGHY.endochange.out)

## append latent state estimates of plot frequency to AGHY.plots data frame
AGHY.plots$p.14<-AGHY.endochange.out$BUGSoutput$mean$p.14
AGHY.plots$p.15<-AGHY.endochange.out$BUGSoutput$mean$p.15
AGHY.plots$p.16<-AGHY.endochange.out$BUGSoutput$mean$p.16

par(mfrow=c(3,1))
plot(x.levels,x.levels,type="n")
lines(x.levels,AGHY.endochange.out$BUGSoutput$mean$Eplus14.add.pred,lwd=4,col="blue")
lines(x.levels,AGHY.endochange.out$BUGSoutput$summary[1:101,"2.5%"],lwd=1,col="blue")
lines(x.levels,AGHY.endochange.out$BUGSoutput$summary[1:101,"97.5%"],lwd=1,col="blue")
lines(x.levels,AGHY.endochange.out$BUGSoutput$mean$Eplus14.control.pred,lwd=4,col="red")
lines(x.levels,AGHY.endochange.out$BUGSoutput$summary[102:202,"2.5%"],lwd=1,col="red")
lines(x.levels,AGHY.endochange.out$BUGSoutput$summary[102:202,"97.5%"],lwd=1,col="red")
abline(0,1,col="gray")
points(AGHY.plots$target_init_freq[AGHY.plots$water=="Add"],
       AGHY.plots$p.14[AGHY.plots$water=="Add"],col="blue",cex=2)
points(AGHY.plots$target_init_freq[AGHY.plots$water=="Control"],
       AGHY.plots$p.14[AGHY.plots$water=="Control"],col="red",cex=2)

plot(x.levels,x.levels,type="n")
lines(x.levels,AGHY.endochange.out$BUGSoutput$mean$Eplus15.add.pred,lwd=4,col="blue")
lines(x.levels,AGHY.endochange.out$BUGSoutput$summary[203:303,"2.5%"],lwd=1,col="blue")
lines(x.levels,AGHY.endochange.out$BUGSoutput$summary[203:303,"97.5%"],lwd=1,col="blue")
lines(x.levels,AGHY.endochange.out$BUGSoutput$mean$Eplus15.control.pred,lwd=4,col="red")
lines(x.levels,AGHY.endochange.out$BUGSoutput$summary[304:404,"2.5%"],lwd=1,col="red")
lines(x.levels,AGHY.endochange.out$BUGSoutput$summary[304:404,"97.5%"],lwd=1,col="red")
abline(0,1,col="gray")
points(AGHY.plots$p.14[AGHY.plots$water=="Add"],
       AGHY.plots$p.15[AGHY.plots$water=="Add"],col="blue",cex=2)
points(AGHY.plots$p.14[AGHY.plots$water=="Control"],
       AGHY.plots$p.15[AGHY.plots$water=="Control"],col="red",cex=2)


plot(x.levels,x.levels,type="n")
lines(x.levels,AGHY.endochange.out$BUGSoutput$mean$Eplus16.add.pred,lwd=4,col="blue")
lines(x.levels,AGHY.endochange.out$BUGSoutput$summary[405:505,"2.5%"],lwd=1,col="blue")
lines(x.levels,AGHY.endochange.out$BUGSoutput$summary[405:505,"97.5%"],lwd=1,col="blue")
lines(x.levels,AGHY.endochange.out$BUGSoutput$mean$Eplus16.control.pred,lwd=4,col="red")
lines(x.levels,AGHY.endochange.out$BUGSoutput$summary[506:606,"2.5%"],lwd=1,col="red")
lines(x.levels,AGHY.endochange.out$BUGSoutput$summary[506:606,"97.5%"],lwd=1,col="red")
abline(0,1,col="gray")
points(AGHY.plots$p.15[AGHY.plots$water=="Add"],
       AGHY.plots$p.16[AGHY.plots$water=="Add"],col="blue",cex=2)
points(AGHY.plots$p.15[AGHY.plots$water=="Control"],
       AGHY.plots$p.16[AGHY.plots$water=="Control"],col="red",cex=2)












## collect observed frequencies at the plot level (which is the Bayes latent state)
AGHY.plot.freq<-ddply(merge(AGHY.immunoblot,AGHY.plots,by="plot"), c("year_t","newplot"), summarize, 
            total = length(agri_liberal),
            E_plus_liberal = sum(agri_liberal),
            E_plus_conservative = sum(agri_conservative))
AGHY.plot.freq$E_plus_freq<-AGHY.plot.freq$E_plus_liberal/AGHY.plot.freq$total

## compare the Bayes' latent state estimate to the observed plot freq for 2014 and 2015
posterior.plot.freq<-data.frame(cbind(1:N.plots,AGHY.endochange.out$BUGSoutput$mean$p.14,AGHY.endochange.out$BUGSoutput$mean$p.15,AGHY.endochange.out$BUGSoutput$mean$p.16))
names(posterior.plot.freq)<-c("newplot","p14","p15","p16")
AGHY.plot.freq<-merge(AGHY.plot.freq,posterior.plot.freq,by="newplot")

par(mfrow=c(3,1))
plot(AGHY.plot.freq$E_plus_freq[AGHY.plot.freq$year_t==2014],
     AGHY.plot.freq$p14[AGHY.plot.freq$year_t==2014])
abline(0,1)
plot(AGHY.plot.freq$E_plus_freq[AGHY.plot.freq$year_t==2015],
     AGHY.plot.freq$p15[AGHY.plot.freq$year_t==2015])
abline(0,1)
plot(AGHY.plot.freq$E_plus_freq[AGHY.plot.freq$year_t==2016],
     AGHY.plot.freq$p16[AGHY.plot.freq$year_t==2016])
abline(0,1)
## OK looks like the latent state estimation is doing a good job

mcmcplot(AGHY.endochange.out)
MLfit<-glmer(lib_freq~target_init_freq*water+(1|plot),weights=total,family="binomial",data=subset(AGHY.merge,year_t==2014))
summary(MLfit)

par(mfrow=c(1,2))
plot(seq(0,1,0.1),seq(0,1,0.1),type="n")
lines(seq(0,1,0.1),
      invlogit(AGHY.endochange.out$BUGSoutput$mean$beta0.mean.14[1] + AGHY.endochange.out$BUGSoutput$mean$beta1.14[1]*seq(0,1,0.1)),lwd=4,col="blue")
points(AGHY.merge$target_init_freq[AGHY.merge$water=="Add"],
       AGHY.merge$lib_freq[AGHY.merge$water=="Add"],col="blue")
lines(seq(0,1,0.1),
      invlogit(fixef(MLfit)[1]+fixef(MLfit)[2]*seq(0,1,0.1)))

plot(seq(0,1,0.1),seq(0,1,0.1),type="n")
lines(seq(0,1,0.1),
      invlogit(AGHY.endochange.out$BUGSoutput$mean$beta0.mean.14[2] + AGHY.endochange.out$BUGSoutput$mean$beta1.14[2]*seq(0,1,0.1)),lwd=4,col="red")
points(AGHY.merge$target_init_freq[AGHY.merge$water=="Control"],
       AGHY.merge$lib_freq[AGHY.merge$water=="Control"],col="red")
lines(seq(0,1,0.1),
      invlogit(fixef(MLfit)[1]+fixef(MLfit)[3]+(fixef(MLfit)[2]+fixef(MLfit)[4])*seq(0,1,0.1)))

par(mfrow=c(1,1))
plot(seq(0,1,0.1),seq(0,1,0.1),type="n")
lines(seq(0,1,0.1),
      invlogit(AGHY.endochange.out$BUGSoutput$mean$beta0.mean.15[1] + AGHY.endochange.out$BUGSoutput$mean$beta1.15[1]*seq(0,1,0.1)),lwd=4,col="blue")
lines(seq(0,1,0.1),
      invlogit(AGHY.endochange.out$BUGSoutput$mean$beta0.mean.15[2] + AGHY.endochange.out$BUGSoutput$mean$beta1.15[2]*seq(0,1,0.1)),lwd=4,col="red")


### Visualize 2013/14 endo freq change, fit Bayesian model to it

AGHY1314<-subset(AGHY.merge,year_t==2014)
AGHY1314$freq_t1_liberal<-AGHY1314$E_plus_liberal / AGHY1314$total

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
