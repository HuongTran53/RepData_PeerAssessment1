---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data:

```r
df <- read.csv("data/activity.csv", sep = ",")
```
Looking at the data:

```r
head(df)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
There are 3 varialbes: steps, data and interval.



## What is mean total number of steps taken per day?

```r
steps.date <- with(df, tapply(steps, date, sum, na.rm = TRUE))
steps.date <- data.frame(date = names(steps.date), steps = steps.date)

cat("The mean of total number of steps taken per day is: ", mean(steps.date$steps))
```

```
## The mean of total number of steps taken per day is:  9354.23
```

```r
cat("The median of total number of steps taken per day is: ", median(steps.date$steps) )
```

```
## The median of total number of steps taken per day is:  10395
```

```r
library(ggplot2)
g <- ggplot(steps.date, aes(steps))
g + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



## What is the average daily activity pattern?

```r
# calculate the average of steps per day:
steps.interval <- with(df, tapply(steps, interval, mean, na.rm = TRUE))
steps.interval <- data.frame(interval = names(steps.interval), steps = steps.interval)
plot(steps.interval$interval,
     steps.interval$steps, 
     type = "l",
     xlab = "5-minute interval",
     ylab = "averaged steps across all days",
     main = "Average daily activity pattern"
     )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
index <- which(steps.interval$steps == max(steps.interval$steps)) 
max.interval <- steps.interval$interval[index]
cat("The interval contains the maximum number of steps is: ", max.interval)
```

```
## The interval contains the maximum number of steps is:  835
```





## Imputing missing values


```r
# missing value:
df1 <- df
num.missing = apply(df1, 2, function(x) sum(is.na(x)))

print(num.missing)
```

```
##    steps     date interval 
##     2304        0        0
```

```r
# fill missing value: 
na.index <- which(is.na(df1$steps) == TRUE)
for (i  in na.index) {
  interval <- df1$interval[i]
  tem <- which(steps.interval$interval == 5)
  m <- steps.interval$steps[tem]
  df1$steps[i] <- m
}

# make histogram:
steps.date1 <- with(df1, tapply(steps, date, sum))
steps.date1 <- data.frame(date = names(steps.date1), steps = steps.date1)

h <- ggplot(steps.date1, aes(steps))
h + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
cat("The mean of total number of steps taken per day is: ", mean(steps.date1$steps))
```

```
## The mean of total number of steps taken per day is:  9367.057
```

```r
cat("The median of total number of steps taken per day is: ", median(steps.date1$steps))
```

```
## The median of total number of steps taken per day is:  10395
```


## Are there differences in activity patterns between weekdays and weekends?

```r
# we will use df1 -  no missing value for this part.
df1$date <- as.Date(df1$date)

library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
df1$day <- wday(df1$date, label = TRUE)


df1$day <- gsub("Mon|Tue|Wed|Thu|Fri", "weekday", df1$day)
df1$day <- gsub("Sat|Sun", "weekend", df1$day)

# calculate sum steps according to interval and day
steps.interval1 <- tapply(df1$steps, list(df1$interval, df1$day), FUN = sum)
steps.interval1 <- data.frame(interval = row.names(steps.interval1), steps = steps.interval1)



library(tidyr)
steps.interval1 <- gather(steps.interval1, key = "day", value = "steps", -interval)
steps.interval1$interval <- as.numeric(steps.interval1$interval)


k <- ggplot(steps.interval1, aes(interval, steps))
k + geom_line() +  facet_grid(rows = vars(day))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

