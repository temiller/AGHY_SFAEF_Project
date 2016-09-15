## Title: HOBO readout plots for AGHY/AGPE, ELVI, and LOMU/LOAR
## Author: Marion Donald
## Date Started: March 25, 2015
## Purpose: Check to see if the HOBO senors are reading correctly
## Date updated: March 25, 2015


## read in HOBO data for AGHYAGPE from March 2015 to see what's going on
HOBO.AGHY<-read.csv("AGHYAGPEMarch2015.csv")
head(HOBO.AGHY)

plot(HOBO.AGHY[,1])

plot(HOBO.AGHY$Run, HOBO.AGHY$AGPE130.water, type="l", col="blue", ylim=c(0,.5))
lines(HOBO.AGHY$Run, HOBO.AGHY$AGHY129.water, col="cornflowerblue")
lines(HOBO.AGHY$Run, HOBO.AGHY$AGHY140.control, col="tomato")
lines(HOBO.AGHY$Run, HOBO.AGHY$AGPE141.control, col="firebrick")


mean(HOBO.AGHY$AGPE141.control)
mean(HOBO.AGHY$AGHY140.control)
mean(HOBO.AGHY$AGHY129.water)
mean(HOBO.AGHY$AGPE130.water)

## do the same for ELVI
HOBO.ELVI<-read.csv("ELVIMarch2015.csv")
head(HOBO.ELVI)

plot(HOBO.ELVI[,1])

plot(HOBO.ELVI$Run, HOBO.ELVI$ELVI250.water, type="l", col="blue", ylim=c(0,.5))
lines(HOBO.ELVI$Run, HOBO.ELVI$ELVI251.water, col="cornflowerblue")
lines(HOBO.ELVI$Run, HOBO.ELVI$ELVI260.control, col="tomato")
lines(HOBO.ELVI$Run, HOBO.ELVI$ELVI259.control, col="firebrick")

mean(HOBO.ELVI$ELVI259.control)
mean(HOBO.ELVI$ELVI260.control)
mean(HOBO.ELVI$ELVI251.water)
mean(HOBO.ELVI$ELVI250.water)


## do the same for LOAR/LOMU *** After the March reading the readout will change its
## order because a new sensor was installed in plot 37 - check the Methods/Design
## document for details 

HOBO.LOMU<-read.csv("LOMULOARMarch2015.csv")
head(HOBO.LOMU)

plot(HOBO.LOMU[,1])

plot(HOBO.LOMU$Run, HOBO.LOMU$LOAR53.water, type="l", col="blue", ylim=c(0,.5))
lines(HOBO.LOMU$Run, HOBO.LOMU$LOMU52.water, col="cornflowerblue")
lines(HOBO.LOMU$Run, HOBO.LOMU$LOAR38.control, col="tomato")
lines(HOBO.LOMU$Run, HOBO.LOMU$LOMU37.control, col="firebrick")

mean(HOBO.LOMU$LOAR38.control)
mean(HOBO.LOMU$LOMU37.control)
mean(HOBO.LOMU$LOAR53.water)
mean(HOBO.LOMU$LOMU52.water)
