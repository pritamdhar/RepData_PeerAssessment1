# Reproducible Research: Peer Assessment 1
R Markdown file meant for peer evaluation


## Loading and preprocessing the data

Simply Unzip and load the csv into a dataset


```r
unzip("activity.zip")
actData <- read.csv("activity.csv")
```



## What is mean total number of steps taken per day?

#### 1. Make a histogram of the total number of steps taken each day


```r
steps.date <- aggregate(steps ~ date, data = actData, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, main = c("Total number of steps taken each day"), 
    xlab = "Date", ylab = "Steps", font.lab = 2)
```

![plot of chunk makeHist](figure/makeHist.png) 


#### 2. Calculate and report the mean and median total number of steps taken per day


```r
mean(steps.date$steps)
```

```
## [1] 10766
```

```r
median(steps.date$steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

#### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps.interval <- aggregate(steps ~ interval, data = actData, FUN = mean)
plot(steps.interval, type = "l", main = c("Plot of 5-minute interval \n and the average number of steps taken, \n averaged across all days"), 
    font.lab = 2)
```

![plot of chunk timeSeriesPlot](figure/timeSeriesPlot.png) 


#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```



## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(actData))
```

```
## [1] 2304
```


#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The means for the 5-minute intervals could be used to replace the missing values in the data set.

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
actCleanData <- merge(actData, steps.interval, by = "interval", suffixes = c("", 
    ".y"))
nas <- is.na(actCleanData$steps)
actCleanData$steps[nas] <- actCleanData$steps.y[nas]
actCleanData <- actCleanData[, c(1:3)]
```


#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps.date <- aggregate(steps ~ date, data = actCleanData, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, main = c("Total number of steps taken each day"), 
    xlab = "date", ylab = "steps", font.lab = 2)
```

![plot of chunk NewHist](figure/NewHist.png) 

```r
mean(steps.date$steps)
```

```
## [1] 10766
```

```r
median(steps.date$steps)
```

```
## [1] 10766
```


The impact of the missing data is low, considering the total number of steps per day.


## Are there differences in activity patterns between weekdays and weekends?

#### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
dayType <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}

actCleanData$dayType <- as.factor(sapply(actCleanData$date, dayType))
```


#### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data


```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = actCleanData, subset = actCleanData$dayType == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type, font.lab = 2)
}
```

![plot of chunk panelPlot](figure/panelPlot.png) 

