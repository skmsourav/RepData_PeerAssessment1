# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

##### 1. Load the data (i.e. read.csv())

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
baseData <- read.csv('activity.csv')
```

##### 2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
# change date type to date class 
baseData$date <- as.Date(as.character(baseData$date))
# create NA logical vector
baseDataNA <- is.na(baseData$steps)
# create clean base for later mean calculations
cleanBase <- baseData[!baseDataNA,]
```

## What is mean total number of steps taken per day?

##### 1. Calculate the total number of steps taken per day

```r
# aggregate clean non NA steps per day (SUM)
SummedDataByDay <- aggregate(baseData$steps, by=list(baseData$date), sum)
# adjust column names
names(SummedDataByDay)[1] ="date"
names(SummedDataByDay)[2] ="totalsteps"
# top 15 of Summed Steps by day
head(SummedDataByDay,15)
```

```
##          date totalsteps
## 1  2012-10-01         NA
## 2  2012-10-02        126
## 3  2012-10-03      11352
## 4  2012-10-04      12116
## 5  2012-10-05      13294
## 6  2012-10-06      15420
## 7  2012-10-07      11015
## 8  2012-10-08         NA
## 9  2012-10-09      12811
## 10 2012-10-10       9900
## 11 2012-10-11      10304
## 12 2012-10-12      17382
## 13 2012-10-13      12426
## 14 2012-10-14      15098
## 15 2012-10-15      10139
```

##### 2. Make a histogram of the total number of steps taken each day

```r
ggplot(SummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

##### 3. Calculate and report the mean and median total number of steps taken per day

```r
mean(SummedDataByDay$totalsteps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(SummedDataByDay$totalsteps,na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

##### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
nonNASubset <- baseData[!baseDataNA,]
MeanDataByInterval <- aggregate(nonNASubset$steps, by=list(nonNASubset$interval), mean)
# set the column names
names(MeanDataByInterval)[1] ="interval"
names(MeanDataByInterval)[2] ="steps"

ggplot(MeanDataByInterval, aes(x = interval, y=steps)) +
  labs(title = "Sum of Steps by Interval", x = "interval", y = "steps")+
  geom_line(color="red") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxInterval <- MeanDataByInterval[which.max(MeanDataByInterval$steps),]
maxInterval
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset 

```r
missingVals <- sum(baseDataNA)
missingVals
```

```
## [1] 2304
```

##### 2. Devise a strategy for filling in all of the missing values in the dataset.
##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# new dataset
baseData2 <- baseData
# NA's in dataset
# dataset minus NA's for Mean calculation
NABase2 <- baseData2[is.na(baseData2$steps),]
cleanBase2 <- baseData2[!is.na(baseData2$steps),]

# generate Mean Data2 by interval
MeanData2ByInterval <- aggregate(cleanBase2$steps, by=list(cleanBase2$interval), sum)
names(MeanData2ByInterval)[1] ="interval"
names(MeanData2ByInterval)[2] ="steps"

# IMPUT METHOD
baseData2 <- baseData
missingData <- is.na(baseData2$steps)
meanVals <- tapply(cleanBase$steps, cleanBase$interval, mean, na.rm=TRUE, simplify=TRUE)
baseData2$steps[missingData] <- meanVals[as.character(baseData2$interval[missingData])]

# original missing data
sum(missingData)
```

```
## [1] 2304
```

```r
#count of all NA values
sum(is.na(baseData2$steps))
```

```
## [1] 0
```

##### 4. Make a histogram of the total number of steps taken each day 

```r
FullSummedDataByDay <- aggregate(baseData2$steps, by=list(baseData2$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)
```

```
##          date totalsteps
## 1  2012-10-01   10766.19
## 2  2012-10-02     126.00
## 3  2012-10-03   11352.00
## 4  2012-10-04   12116.00
## 5  2012-10-05   13294.00
## 6  2012-10-06   15420.00
## 7  2012-10-07   11015.00
## 8  2012-10-08   10766.19
## 9  2012-10-09   12811.00
## 10 2012-10-10    9900.00
## 11 2012-10-11   10304.00
## 12 2012-10-12   17382.00
## 13 2012-10-13   12426.00
## 14 2012-10-14   15098.00
## 15 2012-10-15   10139.00
```

```r
ggplot(FullSummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##### ... and Calculate and report the mean and median total number of steps taken per day. 

```r
mean(FullSummedDataByDay$totalsteps)
```

```
## [1] 10766.19
```

```r
median(FullSummedDataByDay$totalsteps)
```

```
## [1] 10766.19
```

##### Do these values differ from the estimates from the first part of the assignment?
Yes, he mean is the same but the median has risen 1.19 steps.

##### What is the impact of imputing missing data on the estimates of the total daily number of steps?
The effect of using mean data per interval as a data impute method for missing values seems to push overall data towards the mean.

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
baseData2$weekday <- weekdays(baseData2$date)
baseData2$weekend <- ifelse (baseData2$weekday == "Saturday" | baseData2$weekday == "Sunday", "Weekend", "Weekday")
#baseData2$weekend <- as.factor(baseData2$weekend)
head(baseData2,5)
```

```
##       steps       date interval weekday weekend
## 1 1.7169811 2012-10-01        0  Monday Weekday
## 2 0.3396226 2012-10-01        5  Monday Weekday
## 3 0.1320755 2012-10-01       10  Monday Weekday
## 4 0.1509434 2012-10-01       15  Monday Weekday
## 5 0.0754717 2012-10-01       20  Monday Weekday
```

##### 2. Make a panel plot containing a time series plot


```r
MeanDataWeekendWeekday <- aggregate(baseData2$steps, by=list(baseData2$weekend, baseData2$interval), mean)
names(MeanDataWeekendWeekday)[1] ="weekend"
names(MeanDataWeekendWeekday)[2] ="interval"
names(MeanDataWeekendWeekday)[3] ="steps"

ggplot(MeanDataWeekendWeekday, aes(x = interval, y=steps, color=weekend)) +
  geom_line() +
  facet_grid(weekend ~ .) +
  labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

There seems to be variation in the beginning of the day during weekdays, likely due to workplace activities. There seems to be an overall slightly larger incidence of steps during the weekends.
