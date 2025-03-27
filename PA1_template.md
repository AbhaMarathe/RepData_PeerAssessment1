---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
#Loading Data
activity_data<-read.csv("activity.csv")

#Processing Data
activity_data$date<-as.Date(activity_data$date)
```


## What is mean total number of steps taken per day?

```r
#Mean

total_steps_day<-aggregate(steps~date,activity_data,sum,na.rm=T)
mstep=mean(total_steps_day$steps,na.rm=T)

#Median
medstep=median(total_steps_day$steps,na.rm=T)

#Histogram
hist(total_steps_day$steps,main="Total Steps taken each Day",xlab="Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## What is the average daily activity pattern?

```r
avg_steps<-aggregate(steps~interval,activity_data,mean,na.rm=T)
#Plot
plot(avg_steps$interval,avg_steps$steps,type="l",col="red",xlab="5min interval",ylab="average number of steps",main="Average Daily activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxstep<-max(avg_steps$interval)
cat("\nMaximum interval:",maxstep)
```

```
## 
## Maximum interval: 2355
```


## Imputing missing values

```r
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 
missing_values<-sum(is.na(activity_data))
cat("\nNumber of missing values:",missing_values)
```

```
## 
## Number of missing values: 2304
```

```r
#imputing the missing values by mean

activity_data$steps[is.na(activity_data$steps)]=mean(activity_data$steps,na.rm=T)

#Histogram with missing values imputed
total_steps_day1<-aggregate(steps~date,activity_data,sum)
hist(total_steps_day1$steps,main="Total Steps taken each Day with NA imputation",xlab="Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#Mean and Median after imputation
mstep_imputed=mean(total_steps_day1$steps)
medstep_imputed=median(total_steps_day1$steps)

# The values of mean before and after imputations are same as the impitation is done by Mean imputation. There is very slight difference in median after and before values.
```


## Are there differences in activity patterns between weekdays and weekends?

```r
#factor variable in the dataset with two levels – “weekday” and “weekend”
activity_data$date<-as.Date(activity_data$date)
activity_data$day_type<-ifelse(weekdays(activity_data$date) %in% c("Saturday","Sunday"),"weekend","weekday")
activity_data$day_type<-as.factor(activity_data$day_type)
table(activity_data$day_type)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
head(activity_data)
```

```
##     steps       date interval day_type
## 1 37.3826 2012-10-01        0  weekday
## 2 37.3826 2012-10-01        5  weekday
## 3 37.3826 2012-10-01       10  weekday
## 4 37.3826 2012-10-01       15  weekday
## 5 37.3826 2012-10-01       20  weekday
## 6 37.3826 2012-10-01       25  weekday
```

```r
#panel plot containing a time series plot.
library(lattice)
interval_day_type<-aggregate(steps~interval+day_type,data=activity_data,mean)
xyplot(steps~interval|day_type,data=interval_day_type,type="l",layout=c(1,2),main="Average Daily activity Pattern by day type",xlab="5min interval", ylab="Avergae number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
