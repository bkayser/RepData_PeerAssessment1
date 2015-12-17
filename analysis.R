library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
unzip('activity.zip', overwrite=F)

data <- tbl_df(read.csv('activity.csv', stringsAsFactors = T))
data$min <-  parse_date_time(sprintf('%04d', data$interval), '%H%M')

steps <- group_by(data, date) %>% 
    summarize(total=sum(steps)) %>%
    filter(!is.na(total))

ggplot(steps) + aes(x=day,y=total) + geom_bar(stat='identity') +
    ggtitle("Total Number of Steps Taken Each Day") +
    xlab(NULL) + ylab('Total Steps') 

ggplot(steps) + aes(x=total) + geom_histogram(binwidth=1000)
rbind(Median=median(steps$total), Mean=mean(steps$total))

by_interval <- group_by(data, interval, min) %>% summarize(steps=mean(steps,na.rm=T))
qplot(by_interval$min, by_interval$steps) + geom_line()

by_interval$interval[by_interval$steps == max(by_interval$steps)]
