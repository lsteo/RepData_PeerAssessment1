---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document: 
keep_md: true
---
# Reproducible Research: Peer Assessment 1


```r
opts_chunk$set(echo = TRUE)
```


```r
library(knitr)
library(ggplot2)
library(chron)
library(plyr)
```


```r
Sys.setenv(TZ='GMT')
```

## Loading and preprocessing the data

```r
## Create subfolder "temp" in working directory if it does not exists
if (!file.exists("./activity.csv")) {
        unzip("./activity.zip")
        }
## readcsv data file
ds <- read.csv("./activity.csv")

## set column 'date' to date format
ds$date <- as.Date(ds$date, format = "%Y-%m-%d")

## set column 'interval' to time format %H:%M:%S
ds$interval <- sprintf("%04d", ds$interval)
ds$interval <- gsub("^(.{2})(.*)$", "\\1:\\2:00", ds$interval)
ds$interval <- times(ds$interval)
```


## What is mean total number of steps taken per day?

```r
## tabulate total number of steps taken each day
total <- aggregate(ds$steps, by = list(ds$date), "sum", na.rm = TRUE)
colnames(total) <- c("date", "total_steps")

## plot histogram of the total number of steps taken per day using base system 
par(ps = 12)
hist(total$total_steps, xlab = "Total Steps", main = "Histogram of Total Steps")
```

![plot of chunk totalsteps](figure/totalsteps-1.png) 

```r
## calculate mean and median total number of steps taken per day
mean(total$total_steps)
```

```
## [1] 9354.23
```

```r
median(total$total_steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
## tablulate average steps taken for each interval 
average.ds <- aggregate(ds$steps, by = list(ds$interval), "mean", na.rm = TRUE)
colnames(average.ds) <- c("interval", "average_steps")

## plot time-series graph of interval against average number of steps across
## all days
g <- ggplot(data = average.ds, aes(x = interval, y = average_steps))
g + geom_line(aes(group = 1), colour = "blue") + labs(x = "Time Interval") + labs(y = "Average Number of Steps") +labs(title = "Average Number of Steps at each Time Interval") + scale_x_chron(format = "%H:%M")
```

![plot of chunk averageActivity](figure/averageActivity-1.png) 

```r
which.interval <- as.character(average.ds[average.ds$average_steps == max(average.ds$average_steps), ]$interval)
```
08:35:00 contains the maximum average number of steps across all the days in the dataset.

## Imputing missing values

```r
## find the number of rows with NA in column 'steps'
missing.rows <- which(is.na(ds$steps))
missing <- length(missing.rows)
```
The total number of rows with NA is 2304


```r
## append the average steps across all days for each interval to the
## data.frame ds.new
ds.new <- join(ds, average.ds)
```

```
## Joining by: interval
```

```r
ds.new$steps <-  ifelse(is.na(ds.new$steps), ds.new$average_steps, ds.new$steps)
ds.new <- subset(ds.new, select = (-average_steps))
```


```r
total.new <- aggregate(ds.new$steps, by = list(ds.new$date), "sum", na.rm = TRUE)
colnames(total.new) <- c("date", "total_steps")
par(ps = 12)
hist(total.new$total_steps, xlab = "Total Steps", main = "Histogram of Total Steps")
```

![plot of chunk histForReplaceNA](figure/histForReplaceNA-1.png) 

```r
mean(total.new$total_steps)
```

```
## [1] 10766.19
```

```r
median(total.new$total_steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
ds.new$day <- ifelse(weekdays(ds.new$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
ds.new$day <- factor(ds.new$day)

average.ds.day <- aggregate(ds.new$steps, by = list(ds.new$interval, ds.new$day), "mean", na.rm = TRUE)
colnames(average.ds.day) <- c("interval", "day", "average_steps")

g <- ggplot(data = average.ds.day, aes(x = interval, y = average_steps))
g + geom_line(aes(group = 1), colour = "blue") + facet_grid(day ~ .) + labs(x = "Time Interval") + labs(y = "Average Number of Steps") +labs(titlee = "Average Number of Steps at each Time Interval") + scale_x_chron(format = "%H:%M")
```

![plot of chunk activityPatterns](figure/activityPatterns-1.png) 
