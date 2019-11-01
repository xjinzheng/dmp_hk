setwd("~/Users/ginzheng/Desktop/Research/tech&protest")

library(lubridate)
library(ggplot2)
library(scales)

channel_data <- read.csv('telegram_channel_data.csv')
channel_data$Date <- as.Date(channel_data$Date)
channel_data$Time <- as.POSIXct(channel_data$Time, format = "%H:%M")
channel_data$Views <- as.numeric(channel_data$Views)

daily_plot <- ggplot(channel_data, aes(x=Date, y=Views))
daily_plot + geom_bar(stat="identity") + scale_x_date(breaks=date_breaks("2 weeks"))
daily_plot + geom_line(colour="blue") + geom_point(colour="blue4") + scale_x_date(breaks=date_breaks("2 weeks"))

ggplot(channel_data, aes(x=Time, y=Views, group=Date)) + 
  geom_path(size=.5, alpha = 0.1, colour="blue") + 
  scale_x_datetime(breaks=date_breaks("2 hour") +
  scale_y_continuous(breaks = c(0, ))                  , labels=date_format("%H:%M"))




