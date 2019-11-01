setwd("~/Documents/Github/decentralizedcommplatform/tg_data")

library(lubridate)
library(ggplot2)
library(scales)

fc_data<- read.csv('factcheck_data.csv')
fc_data$Date <- as.Date(fc_data$Date)
fc_data$Time <- as.POSIXct(fc_data$Time, format = "%H:%M")
fc_data$Views <- as.numeric(fc_data$Views)

fc_daily_plot <- ggplot(fc_data, aes(x=Date, y=Views))
fc_daily_plot + geom_line(colour="blue") + geom_point(colour="blue4") + scale_x_date(breaks=date_breaks("2 weeks")) + scale_y_continuous(breaks=seq(0,60,5))

fc_minute_plot <-  ggplot(fc_data, aes(x=Time, y=Views, group=Date))
fc_minute_plot + geom_point(size=.5, alpha = 0.6, colour="blue") + 
  scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) + 
  scale_y_continuous(breaks=seq(0,60,5))




