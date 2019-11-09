setwd("~/Documents/Github/dmp_hk/tg_data")
Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
install.packages('ggpubr')
install.packages('tidyverse')
install.packages('Hmisc')
install.packages('corrplot')
install.packages('magrittr')

library(lubridate)
library(ggplot2)
library(scales)
library(ggpubr)

kw_data <- read.csv('multiTimeline_telegram反送中.csv')
kw_data$Day <- as.Date(kw_data$Day)
kw_data$Telegram <- as.integer(kw_data$telegram...Hong.Kong.)
kw_data$反送中 <- as.numeric(levels(kw_data$反.送.中...Hong.Kong.))[kw_data$反.送.中...Hong.Kong.]


ggplot() + geom_line(data=kw_data, aes(x=Day, y=Telegram, color="Telegram")) + 
  ggtitle('Google Search Interest on Keywords over Time') + scale_x_date(breaks=date_breaks("1 month")) +
  geom_line(data=kw_data, aes(x=Day, y=反送中, color="Anti-extradition to China")) +
  scale_colour_manual("",values = c("Anti-extradition to China" = "red","Telegram" = "blue"))+
  xlab("Date")+ylab("Relative Search Interest Scale")

kw_data <- na.omit(kw_data)

cor(kw_data$Telegram, kw_data$反送中,  method = "pearson", use = "complete.obs")
cor.test(kw_data$Telegram, kw_data$反送中,  method = "spearman")



ggscatter(kw_data, y = 'Telegram', x = '反送中',
          add = "reg.line", color = 'black', size = 1.2, conf.int = TRUE,
          #add.params = list(color = "blue", size = 1.5, fill = "gray"),
          cor.coef = TRUE, cor.method = "spearman",
          ylab = "Telegram", xlab = "Anti-extradition to China")


