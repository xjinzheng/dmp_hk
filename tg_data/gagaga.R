setwd("~/Documents/Github/decentralizedcommplatform/tg_data")

library(lubridate)
library(ggplot2)
library(scales)


# fact check plots
fc_data<- read.csv('factcheck.csv')
fc_data$Date <- as.Date(fc_data$Date)
fc_data$Time <- as.POSIXct(fc_data$Time, format = "%H:%M")
fc_data$Views <- as.numeric(fc_data$Views)

fc_daily_plot <- ggplot(fc_data, aes(x=Date, y=Views)) + ggtitle('Fact Check Channel Daily Views') 
fc_daily_plot + geom_line(alpha = 0.5, colour="black") + geom_point(size=.4, alpha =0.8, colour="black") + 
  scale_x_date(breaks=date_breaks("2 weeks")) + scale_y_continuous(breaks=seq(0,60000,5000)) 

fc_minute_plot <-  ggplot(fc_data, aes(x=Time, y=Views, group=Date)) + ggtitle('Fact Check Channel Minutely Views')
fc_minute_plot + geom_point(size=.5, alpha = 0.5, colour="black") + 
  scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) + 
  scale_y_continuous(breaks=seq(0,60000,5000))


# weather plots

w_data<- read.csv('weather.csv')
w_data$Date <- as.Date(w_data$Date)
w_data$Time <- as.POSIXct(w_data$Time, format = "%H:%M")
w_data$Views <- as.numeric(w_data$Views)

w_daily_plot <- ggplot(w_data, aes(x=Date, y=Views)) + ggtitle('Weather Channel Daily Views') 
w_daily_plot + geom_line(alpha=0.5, colour="blue") + geom_point(size=.4, alpha =0.8, colour="blue4") + 
  scale_x_date(breaks=date_breaks("2 weeks")) + scale_y_continuous(breaks=c(0,50,500,5000,10000,50000,100000,150000,200000)) 

w_minute_plot <-  ggplot(w_data, aes(x=Time, y=Views, group=Date)) + ggtitle('Weather Channel Minutely Views')
w_minute_plot + geom_point(size=.5, alpha = 0.5, colour="blue2") + 
  scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) + 
  scale_y_continuous(breaks=c(0,50,500,5000,10000,50000,100000,150000,200000), limits = c(0,100000))


# SilverHair

sh_data<- read.csv('silverhair.csv')
sh_data$Date <- as.Date(sh_data$Date)
sh_data$Time <- as.POSIXct(sh_data$Time, format = "%H:%M")
sh_data$Views <- as.numeric(sh_data$Views)

sh_daily_plot <- ggplot(sh_data, aes(x=Date, y=Views)) + ggtitle('Silver Hair Channel Daily Views') 
sh_daily_plot + geom_line(colour="darkgoldenrod2") + geom_point(size=.4, alpha =0.8, colour="darkorange2") + 
  scale_x_date(breaks=date_breaks("2 weeks")) + scale_y_continuous(breaks=seq(500,70000,5000)) 

sh_minute_plot <-  ggplot(sh_data, aes(x=Time, y=Views, group=Date)) + ggtitle('Silver Hair Channel Minutely Views')
sh_minute_plot + geom_point(size=.5, alpha = 0.5, colour="darkorange") + 
  scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) + 
  scale_y_continuous(breaks=seq(500,50000,5000), limits = c(0,45500))


# Adolescent

al_data<- read.csv('adolescent.csv')
al_data$Date <- as.Date(al_data$Date)
al_data$Time <- as.POSIXct(al_data$Time, format = "%H:%M")
al_data$Views <- as.numeric(al_data$Views)

al_daily_plot <- ggplot(al_data, aes(x=Date, y=Views)) + ggtitle('Middle School Students Channel Daily Views') 
al_daily_plot + geom_line(alpha = 0.3, colour="maroon2") + geom_point(size=.4, alpha =0.8, colour="violetred3") + 
  scale_x_date(breaks=date_breaks("2 weeks")) + scale_y_continuous(breaks=seq(0,125000,5000)) 

al_minute_plot <-  ggplot(al_data, aes(x=Time, y=Views, group=Date)) + ggtitle('Middle School Students Channel Minutely Views')
al_minute_plot + geom_point(size=.5, alpha = 0.5, colour="hotpink1") + 
  scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) + 
  scale_y_continuous(breaks=seq(0,1250000,30000))


# StandWithJournalists

swj_data<- read.csv('standwithjournalists.csv')
swj_data$Date <- as.Date(swj_data$Date)
swj_data$Time <- as.POSIXct(swj_data$Time, format = "%H:%M")
swj_data$Views <- as.numeric(swj_data$Views)

swj_daily_plot <- ggplot(swj_data, aes(x=Date, y=Views)) + ggtitle('Stand with Journalists Channel Daily Views') 
swj_daily_plot + geom_line(alpha = 0.6, colour="springgreen3") + geom_point(size=.4, alpha =0.8, colour="green4") + 
  scale_x_date(breaks=date_breaks("4 days")) + scale_y_continuous(breaks=seq(1000,65000,5000)) 

swj_minute_plot <-  ggplot(swj_data, aes(x=Time, y=Views, group=Date)) + ggtitle('Stand with Journalists Channel Minutely Views')
swj_minute_plot + geom_point(size=.5, alpha = 1, colour="limegreen") + 
  scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) + 
  scale_y_continuous(breaks=seq(0,1250000,5000))
