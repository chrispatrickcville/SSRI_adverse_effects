library(ggplot2)
library(reshape)
library(scales)
library(ddply)

dat <- read.table(text = "    Sex Age Weight
Good   138489  97708 58514
Missing   6221  45601 84629
Problematic   937  2338 2504",sep = "",header = TRUE)
dat$Sex <- dat$Sex/145647
dat$Age <- dat$Age/145647
dat$Weight <- dat$Weight/145647
dat <- round(dat, 4)
datm <- melt(cbind(dat, Category = rownames(dat)), id.vars = c('Category'))
pos <- dat
pos$Sex <- c(pos$Sex[1]/2, pos$Sex[1]+pos$Sex[2]/2, 1-pos$Sex[3]/2)
pos$Age <- c(pos$Age[1]/2, pos$Age[1]+pos$Age[2]/2, 1-pos$Age[3]/2)
pos$Weight <- c(pos$Weight[1]/2, pos$Weight[1]+pos$Weight[2]/2, 1-pos$Weight[3]/2)
posm <- melt(cbind(pos, Category = rownames(pos)), id.vars = c('Category'))

cbind(dat, pos)

ggplot(datm,aes(x = variable, y = value,fill = Category)) + 
  geom_bar(position = "fill",stat = "identity") + 
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels = percent_format())+
  geom_text(data=posm, aes(x = variable, y = value, 
  label = paste(datm$value*100,"%")), size=6)+ ggtitle("Variable Distribution(%)")
