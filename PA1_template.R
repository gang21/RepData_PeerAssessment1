---
title: "Reproducible Research Week 2 Project"
author: "Gabriella Ang"
date: "8/7/2020"
output: html_document
---

## Loading and preprocessing the data

```{r}
setwd("C:/Users/gabri/Desktop/Coursera")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
unzip("activity.zip")
data <- read.csv("activity.csv", header = TRUE)
```
## What is mean total number of steps taken per day?
```{r}
library(ggplot2)
stepsdata <- tapply(data$steps, data$date, sum, na.rm=TRUE)
qplot(stepsdata, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
mean(stepsdata, na.rm = TRUE)
median(stepsdata, na.rm = TRUE)
```

## What is the average daily activity pattern?

```{r}
averages <- aggregate(x = list(steps = data$steps), by = list(interval = data$interval), 
    FUN = mean, na.rm = TRUE)
ggplot(data = averages, aes(x = interval, y = steps)) + geom_line() + xlab("5-Minute Interval") + 
    ylab("Average Number of Steps Taken")
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
averages[which.max(averages$steps), ]
```

## Imputing missing values

Total Number of Missing Values:
```{r}
length(which(is.na(data$steps)))
```

The mean of the 5 minute interval will replace all missing values
```{r}
fillvalue <- function(steps, interval) {
    fill <- NA
    if (!is.na(steps)) 
        fill <- c(steps) else fill <- (averages[averages$interval == interval, "steps"])
    return(fill)
}
filldata <- data
filldata$steps <- mapply(fillvalue, filldata$steps, filldata$interval)
```

Making a histogram with the newly filled data
```{r}
totalsteps <- tapply(filldata$steps, filldata$date, FUN = sum)
qplot(totalsteps, binwidth = 1000, xlab = "Total Number of Steps Taken Each Day")
mean(totalsteps)
median(totalsteps)
```

### Do these values differ from the estimates from the first part of the assignment? 

Yes, the filled data has a higher mean and median than the previous data

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

Inputing missing data increases the total daily number of steps mean and median


## Are there differences in activity patterns between weekdays and weekends?

```{r}
weekdayOrEnd <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
        return("weekend") else stop("invalid date")
}
filldata$date <- as.Date(filldata$date)
filldata$day <- sapply(filldata$date, FUN = weekdayOrEnd)

averages <- aggregate(steps ~ interval + day, data = filldata, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5-Minute Interval") + ylab("Number of Steps")
```



