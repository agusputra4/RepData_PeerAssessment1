---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
To load the data into R, first we unzip the data using `unzip` function. To assign the data to a variable we used the `read.csv` function.

```r
unzip("activity.zip")
dt <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
To count the total steps by date we will used the `dplyr` package to process the data and creating the histogram using `hist` function.

```r
library(dplyr)
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
dtbydate <- dt %>% select(date, steps) %>% group_by(date) %>%
        summarise(totsteps = sum(steps)) %>% na.omit()
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(dtbydate$totsteps, xlab = "Total Daily Steps",
     main = "Histogram of Total Steps Each Day", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The mean and median of total number of steps taken per day is

```r
mean(dtbydate$totsteps)
```

```
## [1] 10766.19
```

```r
median(dtbydate$totsteps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
To see the pattern of the data we need to create the time series plot of the steps by interval. Here we create the plot using `ggolot2` plotting system.

```r
library(ggplot2)
dtbyinterv <- dt %>% select(interval, steps) %>% na.omit() %>%
        group_by(interval) %>% summarise(avgsteps = mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(dtbyinterv, aes(x = interval, y = avgsteps)) + geom_line() +
        labs(title="Time Series Plot of The 5-minute Interval and The Number Of Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The maximum number of steps on average across all days is on interval

```r
dtbyinterv[which(dtbyinterv$avgsteps == max(dtbyinterv$avgsteps)), ]
```

```
## # A tibble: 1 x 2
##   interval avgsteps
##      <int>    <dbl>
## 1      835     206.
```

## Imputing missing values
First we need to count the number of missing value

```r
misval <- sum(is.na(dt));misval
```

```
## [1] 2304
```
We will use the mean for 5-minute interval to replace all the missing values in the data. Then, we will check if all the missing value have been replaced.

```r
replwmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meandt <- dt %>% group_by(interval) %>% mutate(steps= replwmean(steps))
head(meandt)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [6]
##    steps date       interval
##    <dbl> <chr>         <int>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
## 6 2.09   2012-10-01       25
```
The new data set with the missing data filled in is created using these code

```r
fulldt <- aggregate(meandt$steps, by = list(meandt$date), sum)
names(fulldt) <- c("date","totalsteps")
```
Histogram, the mean and the median of the data is created using these code

```r
hist(fulldt$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps",
     breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
summary(fulldt)
```

```
##      date             totalsteps   
##  Length:61          Min.   :   41  
##  Class :character   1st Qu.: 9819  
##  Mode  :character   Median :10766  
##                     Mean   :10766  
##                     3rd Qu.:12811  
##                     Max.   :21194
```
From the above output, we see that `summary` function didn't report any missing value(s), which mean all missing values are filled.  

The comparison of each old vs new mean and median is shown using these code

```r
compr <- c(mean(dtbydate$totsteps, na.rm = T), mean(fulldt$totalsteps),
           median(dtbydate$totsteps, na.rm = T), median(fulldt$totalsteps))
names(compr) <- c("oldmean", "newmean", "oldmedian", "newmedian"); compr
```

```
##   oldmean   newmean oldmedian newmedian 
##  10766.19  10766.19  10765.00  10766.19
```
The value of the mean is the same because we are using the mean to filled up the missing data. While the median slightly change from `10765.00` to `10766.19`.

## Are there differences in activity patterns between weekdays and weekends?
To see the difference between the two, first we need to create new factor variable in the data indicating whether the date is a weekday or weekend.

```r
meandt$date <- as.Date(meandt$date)
meandt$weekday <- weekdays(meandt$date)
meandt$weekend <- ifelse(meandt$weekday == "Saturday" |
                                 meandt$weekday == "Sunday", "Weekend", "Weekday" )

meandtwkendwday <- aggregate(meandt$steps , by = list(meandt$weekend,
                                                      meandt$interval), na.omit(mean))
names(meandtwkendwday) <- c("weekend", "interval", "steps")

ggplot(meandtwkendwday, aes(x = interval, y = steps, color = weekend)) + geom_line() +
        facet_grid(weekend ~ .) + xlab("Interval") + ylab("Mean of Steps") +
        ggtitle("Comparison of Average Number of Steps in Each Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

There is a slight difference between weekdays and weekends, where the maximum step taken occurs on weekdays at interval 835.
