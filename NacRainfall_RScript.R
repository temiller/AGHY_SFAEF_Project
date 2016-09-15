## Title: Nacogdoches Rainfall Data
## Author: Marion Donald
## Date Started: August 26, 2016
## Purpose: Plot the target vs actual rainfall data from Nacogdoches, TX
## Date Updated: 8/26/16

install.packages("xls")
install.packages("scales")
install.packages("gridExtra")
library(xlsx)
library(scales)
library(ggplot2)
library(grid)
library(gridExtra)

setwd("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/AGHY_SFAEF_Project")

## read in irrigation data

irrigation<-read.xlsx("SFAEF irrigation calculations.xls", 1, stringsAsFactors=F)
rainfall<-read.csv("Rainfall_adj.csv")

## Merge Month, Day and Year to make one date
rainfall$Date <-NA
rainfall$Date<- paste(rainfall$Month, rainfall$Day, rainfall$Year, sep ="/")

## Get R to recognize the date as an actual date
rainfall$Dates<-as.Date(rainfall$Date,format = "%B/%d/%Y")


## select just the data I want
#rainfall<-irrigation[c(1,3,4),]

rainfall$num<- 1:48

rainfall$Target <- rainfall$Target_precip
rainfall$Actual<-rainfall$Actual_precip

## plot all the rainfall data we have - Jan 2013 to April 2016
p.rainfall2<- ggplot(rainfall, aes(Dates)) +
  geom_line(aes (y = Target_precip, colour = "Target"), size =1.5, linetype="dashed") +
  geom_line(aes ( y = Actual_precip, colour ="Actual"), size = 1.5) +
  #scale_linetype_manual("",
   #                     values=c("Target"= 2,"Actual"= 1))+
  scale_colour_manual("",
                      values = c("Target"="cadetblue", "Actual"="blue"))+
  xlab("Dates") +
  ylab ("Amount of Precipitation (inches)") +
  scale_x_date(date_breaks = "3 months",  limits = as.Date(c('2012-12-01','2016-04-01')),
               labels = date_format("%b %Y"))+
  guides(color=guide_legend("Precipitation"))

## Plot just the rainfall data that overlaps with the HOBO data Oct 2014 to April 2016
p.rainfall1<- ggplot(rainfall, aes(Dates)) +
  geom_line(aes (y = Target_precip, colour = "Target"), size =1.5, linetype="dashed") +
  geom_line(aes ( y = Actual_precip, colour ="Actual"), size = 1.5) +
  #scale_linetype_manual("",
  #                     values=c("Target"= 2,"Actual"= 1))+
  scale_colour_manual("",
                      values = c("Target"="cadetblue", "Actual"="blue"))+
  xlab("Dates") +
  ylab ("Amount of Precipitation (inches)") +
  scale_x_date(date_breaks = "3 months",  limits = as.Date(c('2014-11-01','2016-04-01')),
               labels = date_format("%b %Y"))+
  guides(color=guide_legend("Precipitation"))


### multiplot
grid.arrange(p.rainfall1, p.Hobo.all, ncol = 2)
