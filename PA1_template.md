# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

First, the date is read from file, and then split by dates.

```r
activity <- read.csv2(unz('activity.zip', 'activity.csv'), sep=',')
activityByDate <- split(activity, activity$date)
```

## What is mean total number of steps taken per day?

Using a little utility function here, which takes one day as an argument.


```r
stepsPerDay <- function(day) {
  return(sum(day$steps, na.rm=TRUE))
}
stepsPerDate <- sapply(activityByDate, stepsPerDay)

hist(stepsPerDate, main="Histogram of total steps per day", xlab="Steps per day")
```

![plot of chunk meanTotalStepsPerDay](figure/meanTotalStepsPerDay.png) 

The mean number of steps in the data is 9354.2295 while the median 10395.

## What is the average daily activity pattern?


```r
stepsPerInterval <- function(interval) {
  return(sum(interval$steps, na.rm=TRUE))
}

avgStepsPerInterval <- function(interval) {
  return(mean(interval$steps, na.rm=TRUE))
}

activityPerInterval <- split(activity, activity$interval)
plot(sapply(activityPerInterval, avgStepsPerInterval), type='l', main="Activity per interval", xlab="Interval number", ylab="Steps")
```

![plot of chunk avgDailyPattern](figure/avgDailyPattern.png) 

The most active interval on average is the interval number 104.

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
