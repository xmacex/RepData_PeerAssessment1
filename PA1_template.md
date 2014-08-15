# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

First, the date is read from a zip file, and then create two versions
split two ways; by *date* and by *interval*.

```r
activity <- read.csv2(unz('activity.zip', 'activity.csv'), sep=',')
activityByDate <- split(activity, activity$date)
activityPerInterval <- split(activity, activity$interval)
```

## What is mean total number of steps taken per day?

Using a little utility function here, which takes one day as an argument.


```r
stepsPerDay <- function(day) {
  return(sum(day$steps, na.rm=TRUE))
}
stepsPerDate <- sapply(activityByDate, stepsPerDay)

hist(stepsPerDate,
     main="Histogram of total steps per day",
     xlab="Steps per day")
```

![plot of chunk meanTotalStepsPerDay](figure/meanTotalStepsPerDay.png) 

The mean number of steps in the data is 9354.2295 while
the median 10395.

## What is the average daily activity pattern?


```r
stepsPerInterval <- function(interval) {
  return(sum(interval$steps, na.rm=TRUE))
}

avgStepsPerInterval <- function(interval) {
  return(mean(interval$steps, na.rm=TRUE))
}

plot(sapply(activityPerInterval, avgStepsPerInterval),
     type='l',
     main="Activity per interval",
     xlab="Interval number",
     ylab="Steps")
```

![plot of chunk avgDailyPattern](figure/avgDailyPattern.png) 

The most active interval on average is the interval
number 104.

## Imputing missing values

The data has total 2304 NA's. A new data set
will now be created. This solution was found online
from [StackOverflow](http://stackoverflow.com/questions/9322773/how-to-replace-na-with-mean-by-subset-in-r-impute-with-plyr).


```r
activityNoNas <- activity
for (i in which(sapply(activityNoNas, is.numeric))) {
    activityNoNas[is.na(activityNoNas[, i]), i] <- mean(activityNoNas[, i], na.rm = TRUE)
}

activityByDateNoNas <- split(activityNoNas, activity$date)
stepsPerDateNoNas <- sapply(activityByDateNoNas, stepsPerDay)
hist(stepsPerDateNoNas,
     main="Histogram of total steps per day (NA's converted)",
     xlab="Steps per day")
```

![plot of chunk handleNas](figure/handleNas.png) 

The mean number of steps in the data now, with NA's replaced with means
is 1.0766 &times; 10<sup>4</sup> while the median 1.0766 &times; 10<sup>4</sup>.
As can be witnessed from the historam here, replacing all unknown data
points (NA) with mean values creates bias and is not such a great strategy
for imputing. It moved low values around the mean. Something like
interpolation, *k* nearest neighbours, or some random variables which have
the same standard variance as the non-NA data would be a better idea.

It would be intersting to calculate how much error was introduced by thus
method.

## Are there differences in activity patterns between weekdays and weekends?


```r
# create a new column to indicate weekends (is it rude to manipulate data?)
weekendDays <- c('lauantai', 'sunnuntai')
activityNoNas$weekend <- weekdays(as.Date(activityNoNas$date)) %in% weekendDays

# temporary data frame
df <- aggregate(activityNoNas$steps,
                list(activityNoNas$weekend, activity$interval),
                mean)

# plot it
par(mfrow=c(2:1))
plot(df$x[df$Group.1 == FALSE],
     type='l', main="Weekdays",
     ylab="Activity",
     ylim=c(0, 250))
plot(df$x[df$Group.1 == TRUE],
     type='l',
     main="Weekend",
     ylab="Activity",
     ylim=c(0, 250))
```

![plot of chunk weekends](figure/weekends.png) 

The data shows on the weekend the person sleeps longer, and is on the move
thoughtout the day, more evenly than during the week. Perhaps the peak
during the week at around 10 is lunchtime?
