---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
---



## Loading and preprocessing the data
library(dplyr)
library(ggplot2)
library(lubridate)

activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date)

## What is mean total number of steps taken per day?
daily_steps <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)

hist(daily_steps$steps,
main = "Histogram of Total Steps per Day",
xlab = "Total steps per day")

mean(daily_steps$steps)
median(daily_steps$steps)

## What is the average daily activity pattern?
interval_avg <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)

plot(interval_avg$interval, interval_avg$steps,
type="l",
xlab="Interval",
ylab="Average steps")

interval_avg$interval[which.max(interval_avg$steps)]

## Imputing missing values
sum(is.na(activity$steps))

activity2 <- activity
na <- is.na(activity2$steps)
activity2$steps[na] <- interval_avg$steps[match(activity2$interval[na], interval_avg$interval)]

daily2 <- aggregate(steps ~ date, activity2, sum)
hist(daily2$steps,
main="Histogram of Steps (Imputed)",
xlab="Steps")
mean(daily2$steps)
median(daily2$steps)


## Are there differences in activity patterns between weekdays and weekends?
activity2$day <- ifelse(weekdays(activity2$date) %in% c("Saturday","Sunday"),
"weekend","weekday")

by_interval <- aggregate(steps ~ interval + day, activity2, mean)

library(ggplot2)
ggplot(by_interval, aes(interval, steps)) +
geom_line() +
facet_wrap(~day)

