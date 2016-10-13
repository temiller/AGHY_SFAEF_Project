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
library(plyr)

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

###### new figures for rainfall by season #####
rainfall$season<-NA
rainfall$season<- ifelse (rainfall$Month == "January", "cool", 
                         ifelse (rainfall$Month == "February", "cool",
                                ifelse (rainfall$Month == "March", "cool",
                                       ifelse (rainfall$Month == "April", "cool",
                                              ifelse (rainfall$Month == "May", "warm",
                                                     ifelse (rainfall$Month == "June", "warm",
                                                            ifelse (rainfall$Month == "July", "warm",
                                                                   ifelse (rainfall$Month == "August", "warm",
                                                                           ifelse(rainfall$Month == "September", "warm",
                                                                                  ifelse(rainfall$Month == "October","cool",
                                                                                         ifelse(rainfall$Month == "November", "cool",
                                                                                                ifelse(rainfall$Month == "December", "cool","other"))))))))))))


## take out Jan-April 2013
rainfall<-rainfall[-c(1:4),]
rainfall$yr_season<-NA

## create season and year associations
list1<- c("13.14","13.14","13.14","13.14","13.14","13.14","13.14","13.14","13.14","13.14","13.14","13.14",
          "14.15","14.15","14.15","14.15","14.15","14.15","14.15","14.15","14.15","14.15","14.15","14.15",
          "15.16","15.16","15.16","15.16","15.16","15.16","15.16","15.16","15.16","15.16","15.16","15.16",
          "16.17", "16.17", "16.17", "16.17", "16.17", "16.17", "16.17", "16.17")
rainfall$yr_season<-list1

## get the total amount of rainfall per season by growing season 
rainfall.season<-ddply(rainfall, c("yr_season", "season"), summarize,
                       actual = sum(Actual))
rainfall.season<-rainfall.season[-c(7:8),]

ggplot(rainfall.season, aes(yr_season, actual))+
  geom_bar(aes(fill = season), position = "dodge", stat = "identity")+
  scale_fill_manual(values = c("cornflowerblue", "firebrick"))+
  labs(x = "Growing Season", y = "Total Precipitation (in)")+
  theme_bw() + theme(panel.border = element_blank())
