Reproducible Research: Peer Assessment 1
========================================


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
meanSteps_allDays <- tapply(actData$steps, actData$interval, mean, na.rm = TRUE)
stepsInterval <- data.frame(interval = as.numeric(as.character(names(meanSteps_allDays))), steps = as.vector(meanSteps_allDays))
totalStepsPerD1 <- tapply(actData$steps, actData$date, sum)
stepsPerday1 <- data.frame(date = names(totalStepsPerD1), steps = as.vector(totalStepsPerD1))
```


## What is mean total number of steps taken per day?

```r
library(ggplot2)
ggplot(data = stepsPerday1, aes(x = steps))+
        geom_histogram()
```
![](https://github.com/younglogos/RepData_PeerAssessment1/blob/master/figure/histogram1.png)


```r
meanTotalStep <- with(stepsPerday1, mean(steps, na.rm = TRUE))
medianTotalStep <- with(stepsPerday1, median(steps, na.rm = TRUE))
print(c(paste("mean total number of steps:", meanTotalStep), paste("median total number of steps:", medianTotalStep)))
```

```
## [1] "mean total number of steps: 10766.1886792453"
## [2] "median total number of steps: 10765"
```


## What is the average daily activity pattern?

```r
ggplot(data = stepsInterval, aes(x = interval, y = steps))+
        geom_line()+
        labs(y = "average steps across all days")
```
![](https://github.com/younglogos/RepData_PeerAssessment1/blob/master/figure/time_series1.png)


```r
interval_maxSteps <- stepsInterval$interval[which(stepsInterval$steps == max(stepsInterval$steps))]
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
missInd <- which(is.na(actData$steps))
IndInertval <- actData$interval[missInd]
fillData$steps[missInd] <- stepsInterval$steps[which(stepsInterval$interval == IndInertval)]
totalStepsPerD2 <- tapply(fillData$steps, fillData$date, sum)
stepsPerday2 <- data.frame(date = names(totalStepsPerD2), steps = as.vector(totalStepsPerD2))
str(fillData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
ggplot(data = stepsPerday2, aes(x = steps))+
        geom_histogram()
```

```
## Warning: Removed 7 rows containing non-finite values (stat_bin).
```
![](https://github.com/younglogos/RepData_PeerAssessment1/blob/master/figure/histogram2.png)


```r
meanFillTotalStep <- with(stepsPerday2, mean(steps, na.rm = TRUE))
medianFillTotalStep <- with(stepsPerday2, median(steps, na.rm = TRUE))
print(c(paste("mean total number of steps after filling in missing values:", meanFillTotalStep), paste("median total number of steps after filling in missing values:", medianFillTotalStep)))
```

```
## [1] "mean total number of steps after filling in missing values: 10766.1886792453"  
## [2] "median total number of steps after filling in missing values: 10765.5943396226"
```

There is no significant difference on mean and median compared with first part of assignment. It indicated that there was little impact of imputing missing data.


## Are there differences in activity patterns between weekdays and weekends?

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.6.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
fillData["week"] <- weekdays(as.Date(fillData$date))
fillData$week[fillData$week != "星期六" & fillData$week != "星期日"] <- "weekday"
fillData$week[fillData$week == "星期六" | fillData$week == "星期日"] <- "weekend"
fillData$week <- as.factor(fillData$week)
weekdaySteps <- with(subset(fillData, week == "weekday"), tapply(steps,interval, mean, na.rm = TRUE))
weekendSteps <- with(subset(fillData, week == "weekend"), tapply(steps,interval, mean, na.rm = TRUE))
weekdayMeanSteps <- data.frame(interval = as.numeric(as.character(names(weekdaySteps))), steps = as.vector(weekdaySteps), week = rep("weekday", dim(weekdaySteps)[1]))
weekendMeanSteps <- data.frame(interval = as.numeric(as.character(names(weekendSteps))), steps = as.vector(weekendSteps), week = rep("weekend", dim(weekendSteps)[1]))
weekMeanSteps <- rbind(weekdayMeanSteps, weekendMeanSteps)
str(fillData$week)
```

```
##  Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```



```r
ggplot(data = weekMeanSteps, aes(x = interval, y = steps))+
  geom_line()+
  facet_grid(week~.)+
  labs(y = "average steps")
```
![](https://github.com/younglogos/RepData_PeerAssessment1/blob/master/figure/time_series2.png)
