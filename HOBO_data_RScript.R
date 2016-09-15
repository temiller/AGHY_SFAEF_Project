#### Title: Did we make the plots wet? -- AGHY Project
### Author: Marion Donald
### Date Started: 8/25/2015
### Purpose: Use the HOBO data to visualize what's going on in the water addition and control plots
### Date Updated: 8/25/2016

#install.packages("devtools")
#install.packages("zoo")
#install.packages("ggfortify")

library(devtools)
library(ggfortify)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape)
library(zoo)

## Need to merge the May 2016 HOBO data with the March 2016 version that goes from Nov 2014 to March 2016

## set working directory 
setwd("C:/Users/Marion Donald/Dropbox/Rice/Projects/AGHY/Nacogdoches HOBO Readouts/CSV Files")

HOBO.all <- read.csv("AGHYAGPEMarch2016.csv" )
HOBO.part <- read.csv("AGHYAGPEMay2016.csv")

## combine the two datasets to make one complete data frame that goes from Nov 2014 to May 2016
Hobo.total<- rbind(HOBO.all, HOBO.part)


## split the date and the time column 
list <- matrix(unlist(strsplit(as.character(Hobo.total$Date)," ")),nrow(Hobo.total),2,byrow = T)
df <- as.data.frame(list)
colnames(df) <- ("Date")

## remove the run and bad date columns
Hobo.total[1:2] <- list(NULL)
df[2] <- NULL
## combine the clean Hobo data with the new date column
Hobo.data <-cbind(df, Hobo.total)
## Turns it into a date -- correct format to start with
Hobo.data$Dates<- as.Date(Hobo.data$Date, format = "%m/%d/%Y")

## plot what's going on for each day multiple times a day
ggplot(Hobo.data, aes(Dates)) +
  geom_line(aes (y = AGPE130.water), colour = "cornflowerblue") +
  geom_line(aes ( y = AGHY129.water), colour ="blue") +
  geom_line(aes (y = AGHY140.control), colour = "firebrick") +
  geom_line(aes (y = AGPE141.control), colour ="red")+
  xlab("Dates") +
  ylab ("Amount of Water m^3/m^3")




## plot what's going on for each day multiple times a day (plot without legend)
ggplot(Hobo.data, aes(Dates)) +
  geom_line(aes (y = AGPE130.water), colour = "cornflowerblue") +
  geom_line(aes ( y = AGHY129.water), colour ="blue") +
  geom_line(aes (y = AGHY140.control), colour = "firebrick") +
  geom_line(aes (y = AGPE141.control), colour ="red")+
  xlab("Dates") +
  ylab(expression(paste("Amount of Water ", "(",m^3, ""/"", m^3, ")", sep=""))) +
  scale_x_date(date_labels ="%b %Y")+
  guides(color=guide_legend("Sensor"))

### plot with legend

p.Hobo.all<-ggplot(Hobo.data, aes(Dates)) +
  geom_line(aes (y = AGPE130.water, colour = "AGPE + Water")) +
  geom_line(aes ( y = AGHY129.water, colour ="AGHY + Water")) +
  geom_line(aes (y = AGHY140.control, colour = "AGHY + Control")) +
  geom_line(aes (y = AGPE141.control, colour ="AGPE + Control"))+
  scale_colour_manual("",
                      breaks= c("AGPE + Water","AGHY + Water","AGHY + Control", "AGPE + Control"),
                      values = c("red", "blue", "firebrick", "cornflowerblue"))+
  xlab("Dates") +
  ylab(expression(paste("Amount of Water ", "(",m^3, ""/"", m^3, ")", sep=""))) +
  scale_x_date(date_labels ="%b %Y")+
  guides(color=guide_legend("Sensor"))



ggplot(data = datos, aes(x = fecha)) +
  geom_line(aes(y = TempMax, colour = "TempMax")) +
  geom_line(aes(y = TempMedia, colour = "TempMedia")) +
  geom_line(aes(y = TempMin, colour = "TempMin")) +
  scale_colour_manual("", 
                      breaks = c("TempMax", "TempMedia", "TempMin"),
                      values = c("red", "green", "blue")) +
  xlab(" ") +
  scale_y_continuous("Temperatura (C)", limits = c(-10,40)) + 
  labs(title="TITULO")



list1 <-matrix(unlist(strsplit(as.character(Hobo.data$Date),"/")), nrow(Hobo.data), 3, byrow=T)
df1 <- as.data.frame(list1)
colnames(df1) <- (c("Month", "Day", "Year"))



df1$Jan<- ifelse(df1$Month == 1, 01, 0)
df1$Feb <- ifelse(df1$Month == 2, 02, 0)
df1$Mar <- ifelse(df1$Month == 3, 03, 0)
df1$Apr <- ifelse(df1$Month == 4, 04, 0)
df1$May <- ifelse(df1$Month == 5, 05, 0)
df1$Jun <- ifelse(df1$Month == 6, 06, 0)
df1$Jul <- ifelse(df1$Month == 7, 07, 0)
df1$Aug <- ifelse(df1$Month == 8, 08, 0)
df1$Sep <- ifelse(df1$Month == 9, 08, 0)
df1$Oct <- ifelse(df1$Month == 10, 10, 0)
df1$Noc <- ifelse(df1$Month == 11, 11, 0)
df1$Dec <- ifelse(df1$Month == 12, 12, 0)

df1$Month_new<- rowSums(df1[,4:15])

## remove Day
df1[2] <- NULL

## merge Month and Year
df1$y_m <- paste(df1$Year, df1$Month, sep ="/")
## just keep this new column
df1[1:2]<-list(NULL)

Hobo.month<- cbind(df1, Hobo.data)
Hobo.month[2]<- NULL

Hobo.month$YM<- read.zoo(text = Hobo.month$y_m, FUN =as.yearmon)

#Hobo.month$date<-as.Date(Hobo.month$y_m)

## squish this down to months
Hobo.months <- ddply(Hobo.month, "y_m", summarize, 
                     AGPE130.water = mean(AGPE130.water),
                     AGHY129.water = mean(AGHY129.water),
                     AGHY140.control = mean(AGHY140.control),
                     AGPE141.control = mean(AGPE141.control))



order<-c(1,2,3,12,13,14,4,5,6,7,8,9,10,11,15,16,17,18,19)
order<-as.data.frame(order)
new<-cbind(order, Hobo.months)
ordered<-new[order(order),]

#dates<-Hobo.months$y_m
#date<-as.Date(dates, format="%m/%Y")

##Hobo.months1<-cbind(date, Hobo.date<-as.Date(dates, "%Y/%m"))
##Hobo.months$Date<- as.Date(Hobo.months$y_m, "%y/%m")




## Combine order with Hobo.months
#Hobo.months1<- cbind(df3, Hobo.months)
#month.data<- Hobo.months1[order(Hobo.months1$order),]
#month.data[2]<-NULL
#melted<- melt(month.data, id="order")

#colorder<- c("blue", "lightblue", "red", "firebrick")
#ggplot(melted, aes(x=order, y=value, colour = variable, group = 4)) +
 # geom_point() +
  #geom_line() +
  #scale_x_date(date_labels = "%b %Y") +
   #xlab("") +
  #ylab ("Amount of Water m^3/m^3")

## line plot of the months -- need to figure out how to get dates on the x-axis
ggplot(ordered, aes(order)) +
  geom_line(aes (y = AGPE130.water), colour = "blue") +
  geom_line(aes ( y = AGHY129.water), colour ="lightblue") +
  geom_line(aes (y = AGHY140.control), colour = "red") +
  geom_line(aes (y = AGPE141.control), colour ="firebrick")+
  xlab("Months from Nov 2014 to May 2016") +
  ylab(expression(paste("Amount of Water ", "(",m^3, ""/"", m^3, ")", sep=""))) 

## Scatter plot of the months -- need to figure out how to get dates on the x-axis
ggplot(ordered, aes(order)) +
  geom_point(aes (y = AGPE130.water), colour = "blue") +
  geom_point(aes ( y = AGHY129.water), colour ="lightblue") +
  geom_point(aes (y = AGHY140.control), colour = "red") +
  geom_point(aes (y = AGPE141.control), colour ="firebrick")+
  xlab("Months from Nov 2014 to May 2016") +
  ylab(expression(paste("Amount of Water ", "(",m^3, ""/"", m^3, ")", sep=""))) 


