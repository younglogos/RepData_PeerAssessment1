---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activity <- unzip("./activity.zip")
actData <- read.csv(activity, sep = ",")
str(actData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
actData["meanSteps_allDays"] = tapply(actData$steps, actData$interval, mean, na.rm = TRUE)
```


## What is mean total number of steps taken per day?

```r
library(ggplot2)
ggplot(data = actData, aes(x = steps))+
        geom_histogram()
```

![](PA1_template_files/figure-html/Make a histogram of the total number of steps taken each day-1.png)<!-- -->


```r
meanTotalStep <- with(actData, mean(steps, na.rm = TRUE))
medianTotalStep <- with(actData, median(steps, na.rm = TRUE))
print(c(paste("mean total number of steps:", meanTotalStep), paste("median total number of steps:", medianTotalStep)))
```

```
## [1] "mean total number of steps: 37.3825995807128"
## [2] "median total number of steps: 0"
```


## What is the average daily activity pattern?

```r
ggplot(data = actData, aes(x = interval, y = meanSteps_allDays))+
        geom_line()+
        labs(y = "average steps across all days")
```

![](PA1_template_files/figure-html/make a time series plot of the 5-minute interval and the average number of steps taken averaged across all days-1.png)<!-- -->


```r
interval_maxSteps <- unique(actData$interval[which(actData$meanSteps_allDays == max(actData$meanSteps_allDays))])
print(paste("interval with the max steps averaged across all days:",interval_maxSteps))
```

```
## [1] "interval with the max steps averaged across all days: 835"
```

## Imputing missing values

```r
totalMiss <- sum(is.na(actData))
print(paste("total number of missing values:", totalMiss))
```

```
## [1] "total number of missing values: 2304"
```


```r
fillData <- actData
missIn <- which(is.na(actData$steps))
fillData$steps[missIn] <- fillData$meanSteps_allDays[missIn]
```


```r
ggplot(data = fillData, aes(x = steps))+
        geom_histogram()
```

![](PA1_template_files/figure-html/make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps-1.png)<!-- -->

```r
meanFillTotalStep <- with(fillData, mean(steps, na.rm = TRUE))
medianFillTotalStep <- with(fillData, median(steps, na.rm = TRUE))
print(c(paste("mean total number of steps after filling in missing values:", meanFillTotalStep), paste("median total number of steps after filling in missing values:", medianFillTotalStep)))
```

```
## [1] "mean total number of steps after filling in missing values: 37.3825995807128"
## [2] "median total number of steps after filling in missing values: 0"
```
There is no significant difference on mean and median compared with first part of assignment. It indicated that there was little impact of imputing missing data.


## Are there differences in activity patterns between weekdays and weekends?

```r
fillData["week"] <- weekdays(as.Date(fillData$date))
fillData$week[fillData$week != "星期六" & fillData$week != "星期日"] <- "weekday"
fillData$week[fillData$week == "星期六" | fillData$week == "星期日"] <- "weekend"
fillData$week <- as.factor(fillData$week)
str(fillData$week)
```

```
##  Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
ggplot(data = subset(fillData), aes(x = interval, y = meanSteps_allDays))+
  geom_line()+
  facet_grid(.~week)+
  labs(y = "average steps across all weekdays or weekends")
```

![](PA1_template_files/figure-html/make a time series plot of the the average number of steps averaged across all weekday days or weekend days-1.png)<!-- -->
