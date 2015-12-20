library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
unzip('activity.zip', overwrite=F)

data <- tbl_df(read.csv('activity.csv', stringsAsFactors = T))
data$min <-  parse_date_time(sprintf('%04d', data$interval), '%H%M')
data$day <- as.factor(ifelse(weekdays(ymd(data$date)) %in% c('Saturday', 'Sunday'), 'weekend', 'weekday'))

by_day <- group_by(data, date) %>% 
    summarize(total=sum(steps)) %>%
    filter(!is.na(total))

qplot(by_day$total, binwidth=1000)
summary_values <- c(Median=median(by_day$total), Mean=mean(by_day$total), Total=sum(by_day$total))

by_interval <- group_by(data, interval, min) %>% summarize(steps=mean(steps,na.rm=T))
ggplot(by_interval) +
    aes(x=min, y=steps) + 
    geom_line() +
    scale_x_datetime(breaks=date_breaks("120 min"),
                     labels = date_format("%H:%M")) +
    labs(x='Time', y='Steps', title='Step Count by Time of Day' )

by_interval$interval[by_interval$steps == max(by_interval$steps)]

sum(is.na(data$steps))

imputed_data <- data
for (i in 1:nrow(data)) {
    if (is.na(data$steps[i])) {
        interval_mean <- by_interval$steps[which(by_interval$interval == data$interval[i])]
        imputed_data$steps[i] <- interval_mean
    }
}

imputed_by_day <- group_by(imputed_data, date) %>% 
    summarize(total=sum(steps)) %>%
    filter(!is.na(total))

qplot(imputed_by_day$total, binwidth=1000)

sum(is.na(imputed_data$steps))

imputed_summary_values <- c(Median=median(imputed_by_day$total), 
                            Mean=mean(imputed_by_day$total), 
                            Total=sum(imputed_by_day$total))

cbind(Original=summary_values, Imputed=imputed_summary_values)

by_interval_and_day <- group_by(data, day, interval, min) %>% summarize(steps=mean(steps,na.rm=T))
ggplot(by_interval_and_day) +
    aes(x=min, y=steps) + geom_line() +
    scale_x_datetime(breaks=date_breaks("120 min"),
                     labels = date_format("%H:%M")) +
    labs(x='Time', y='Steps', title='Step Count by Time of Day and Weekend/Weekday' ) +
    facet_grid(day ~ .)

