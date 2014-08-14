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

The mean number of steps in the data is 9354.2295 while the median 10395, with standard deviation of 5405.8951.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
