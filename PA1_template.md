# Reproducible Research: Peer Assessment 1
By CEV  
September 14, 2015  
## Introduction
This project required a series of steps to analyze personal fitness data comprising the number of steps taken during 5-minute intervals over a 61-day period. The number of steps included NA values that were imputed for some of the analysis. The headings and numbered questions and instructions below were from the assignment.

## Loading and preprocessing the data
1. Load the data

The data was imported as a data frame and converted to a data table, with data.table functions used to summarize the data for much of the assignment.

```r
library("curl") # used with download.file occasionally
library("data.table")
library("ggplot2")
library("scales")

# Download data (also found in related GitHub directory)
# url1 <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# download.file(url1, destfile = "./data/repdata data activity.zip", method="libcurl")
# unzip("./data/repdata data activity.zip", exdir = "./data") # result is: "./data/activity.csv" file

# Load data
act1  <- read.table("./data/activity.csv", header=TRUE, stringsAsFactors = FALSE, sep = ",")
act1 <- data.table(act1)
act1 # view head & tail
```

```
##        steps       date interval
##     1:    NA 2012-10-01        0
##     2:    NA 2012-10-01        5
##     3:    NA 2012-10-01       10
##     4:    NA 2012-10-01       15
##     5:    NA 2012-10-01       20
##    ---                          
## 17564:    NA 2012-11-30     2335
## 17565:    NA 2012-11-30     2340
## 17566:    NA 2012-11-30     2345
## 17567:    NA 2012-11-30     2350
## 17568:    NA 2012-11-30     2355
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

Key steps in preparing the data included (1) converting the numeric time interval to a date time; (2) creating a datetime variable used later in selects; and (3) creating the variable typeDay as either "weekend" or "weekday". While the typeDay variable was required for a later question, it was calculated at the start because it was used when imputing NA values.

```r
act1$time <- sprintf("%04d", act1$interval) # pad with leading 0's for 4-digit character number
act1$time <- paste0(substr(act1$time, 1, 2), ":", substr(act1$time, 3, 4), ":00") # create time
act1 <- within(act1, { datetime=as.POSIXct(strptime(paste(date, time), "%Y-%m-%d %H:%M:%S")) }) # create datetime
# Create typeDay = weekend or weekday
act1$typeDay <- c("weekend", "weekday")[(weekdays(as.Date(act1$date), abbreviate = TRUE) %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))+1]
act1 # view head & tail
```

```
##        steps       date interval     time            datetime typeDay
##     1:    NA 2012-10-01        0 00:00:00 2012-10-01 00:00:00 weekday
##     2:    NA 2012-10-01        5 00:05:00 2012-10-01 00:05:00 weekday
##     3:    NA 2012-10-01       10 00:10:00 2012-10-01 00:10:00 weekday
##     4:    NA 2012-10-01       15 00:15:00 2012-10-01 00:15:00 weekday
##     5:    NA 2012-10-01       20 00:20:00 2012-10-01 00:20:00 weekday
##    ---                                                               
## 17564:    NA 2012-11-30     2335 23:35:00 2012-11-30 23:35:00 weekday
## 17565:    NA 2012-11-30     2340 23:40:00 2012-11-30 23:40:00 weekday
## 17566:    NA 2012-11-30     2345 23:45:00 2012-11-30 23:45:00 weekday
## 17567:    NA 2012-11-30     2350 23:50:00 2012-11-30 23:50:00 weekday
## 17568:    NA 2012-11-30     2355 23:55:00 2012-11-30 23:55:00 weekday
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

The keyStats function made it easier to calculate multiple summary statistics within data.table's .SD summary function, and was used to summarize data throughout this analysis.

The total number of steps taken per day was stored in data table stepsByDay.


```r
# keyStats function used in all summaries
keyStats <- function(x) list(sum = sum(x, na.rm=TRUE), mean = mean(x, na.rm=TRUE))

# Create summary data table with steps.sum and steps.mean by date
stepsByDay <- act1[, as.list(unlist(lapply(.SD, keyStats))), by = date, .SDcols=steps]
head(stepsByDay)
```

```
##          date steps.sum steps.mean
## 1: 2012-10-01         0        NaN
## 2: 2012-10-02       126    0.43750
## 3: 2012-10-03     11352   39.41667
## 4: 2012-10-04     12116   42.06944
## 5: 2012-10-05     13294   46.15972
## 6: 2012-10-06     15420   53.54167
```

2. Make a histogram of the total number of steps taken each day

This histogram is based on the original data that included NA step values.

```r
plot1 <- qplot(steps.sum, data=stepsByDay, geom="histogram") +
        geom_histogram(colour = "white", fill = "blue") +
        ggtitle("Histogram of Total Number of Steps Taken Each Day") +
        labs(x="Total Number of Steps", y="Frequency (Number of Days)") +
        scale_y_continuous(breaks = seq(0, 20, 2)) +
        scale_x_continuous(limits=c(0, 25000), breaks=seq(0,25000,5000))
suppressMessages(print(plot1))
```

![](PA1_template_files/figure-html/fig1-histogram-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day

```r
writeLines(paste0("Mean steps per day: ", format(mean(stepsByDay$steps.sum), big.mark=",", trim=TRUE)))
```

```
## Mean steps per day: 9,354.23
```

```r
writeLines(paste0("Median steps per day: ", format(median(stepsByDay$steps.sum), big.mark=",", trim=TRUE)))
```

```
## Median steps per day: 10,395
```

## What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

The data table stepsByTime was created to summarize the original data by 5-minute time intervals. The variable steps.mean is the mean number of steps taken during each 5-minute time interval during the 61-day reporting period.

In order to produce the time series plot, the POSIXct date variable "time" was used for the x-axis in place of the numeric variable "interval." Since step.mean was the average for 5-minute time intervals during the 61-day reporting period, the original dates associated with time intervals were not included in (and not relevant to) the summary table stepsByTime. The date portion of the variable "time" used the current date as a substitute and that date is not shown on the time series plot (only the 24-hour time intervals are shown).
 

```r
# Create summary data table with steps.sum and steps.mean by time (i.e., by interval)
# Note that current date will be substituted for original date since it's ignored in graph (time used)
stepsByTime <- act1[, as.list(unlist(lapply(.SD, keyStats))), by = time, .SDcols=steps]
stepsByTime$time <- as.POSIXct(strptime(stepsByTime$time, "%H:%M:%S", "GMT"))
stepsByTime # view head & tail
```

```
##                     time steps.sum steps.mean
##   1: 2015-09-14 00:00:00        91  1.7169811
##   2: 2015-09-14 00:05:00        18  0.3396226
##   3: 2015-09-14 00:10:00         7  0.1320755
##   4: 2015-09-14 00:15:00         8  0.1509434
##   5: 2015-09-14 00:20:00         4  0.0754717
##  ---                                         
## 284: 2015-09-14 23:35:00       249  4.6981132
## 285: 2015-09-14 23:40:00       175  3.3018868
## 286: 2015-09-14 23:45:00        34  0.6415094
## 287: 2015-09-14 23:50:00        12  0.2264151
## 288: 2015-09-14 23:55:00        57  1.0754717
```

```r
# Time series plot of 5-min interval (x-axis) and average number of steps taken averaged across 61 days (y-axis)
lower <- min(stepsByTime$time)
upper <- max(stepsByTime$time)-1
limits = c(lower,upper)
plot2 <- ggplot() +
        geom_line(data = stepsByTime, aes(x = time, y = steps.mean), colour = "blue") +
        scale_x_datetime(limits=limits, breaks=("2 hour"), minor_breaks=("1 hour"), labels=date_format("%H:%M")) +
        ggtitle("Average Number of Steps Taken in 5-Minute Time Intervals") +
        labs(x="Time (Hours:Minutes)", y="Average Number of Steps")
suppressMessages(print(plot2))
```

![](PA1_template_files/figure-html/fig2-timeseries-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The 5-minute time (in date and numeric format) containing the maximum value of steps.mean is reported below. The maximum value of steps.mean is provided for reference. See the previous plot for further reference.

```r
writeLines(paste0("Maximum average steps for an interval: ", format(max(stepsByTime$steps.mean), big.mark=",", trim=TRUE)))
```

```
## Maximum average steps for an interval: 206.1698
```

```r
intervalTime <- strftime(stepsByTime$time[stepsByTime$steps.mean == max(stepsByTime$steps.mean)], "%H:%M:%S", "GMT")
interval <- as.numeric(gsub("\\D", "", substr(intervalTime, 1, 5)))
writeLines(paste0("Interval time for maximum average steps for an interval: ", intervalTime, " (Interval = ", interval, ")"))
```

```
## Interval time for maximum average steps for an interval: 08:35:00 (Interval = 835)
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e., the total number of rows with NA's)

All of the missing values applied to the steps variable. The date and interval variables had no missing data. The calculations below show the steps used to make this determination.

```r
length(which(is.na(act1))) #Total number of rows with missing values (coded as NA), including all columns
```

```
## [1] 2304
```

```r
length(which(is.na(act1$steps))) # 2,404 NA's
```

```
## [1] 2304
```

```r
length(which(is.na(act1$date))) # 0 NA's
```

```
## [1] 0
```

```r
length(which(is.na(act1$interval))) # 0 NA's
```

```
## [1] 0
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy used to impute missing values was to replace NA values with the average number of steps during the same time period on the same type of day (weekend or weekday).

```r
# 1. Create summary data table with steps.sum and steps.mean by time by type of day (weekend or weekday)
stepsByTimeByTypeDay <- act1[, as.list(unlist(lapply(.SD, keyStats))), by = c("interval", "time", "typeDay"), .SDcols=steps]
stepsByTimeByTypeDay # view head & tail
```

```
##      interval     time typeDay steps.sum steps.mean
##   1:        0 00:00:00 weekday        91  2.3333333
##   2:        5 00:05:00 weekday        18  0.4615385
##   3:       10 00:10:00 weekday         7  0.1794872
##   4:       15 00:15:00 weekday         8  0.2051282
##   5:       20 00:20:00 weekday         4  0.1025641
##  ---                                               
## 572:     2335 23:35:00 weekend       176 12.5714286
## 573:     2340 23:40:00 weekend        94  6.7142857
## 574:     2345 23:45:00 weekend        26  1.8571429
## 575:     2350 23:50:00 weekend         0  0.0000000
## 576:     2355 23:55:00 weekend         0  0.0000000
```

```r
# 2. Use merge to replace steps=NA with mean steps for the same time and type of day (weekend or weekday)
stepsByTimeByTypeDay <- stepsByTimeByTypeDay[, c("time", "typeDay", "steps.mean"), with = FALSE] # omit unwanted columns
stepsByTimeByTypeDay # view head & tail
```

```
##          time typeDay steps.mean
##   1: 00:00:00 weekday  2.3333333
##   2: 00:05:00 weekday  0.4615385
##   3: 00:10:00 weekday  0.1794872
##   4: 00:15:00 weekday  0.2051282
##   5: 00:20:00 weekday  0.1025641
##  ---                            
## 572: 23:35:00 weekend 12.5714286
## 573: 23:40:00 weekend  6.7142857
## 574: 23:45:00 weekend  1.8571429
## 575: 23:50:00 weekend  0.0000000
## 576: 23:55:00 weekend  0.0000000
```

```r
act2 <- merge(act1, stepsByTimeByTypeDay, by = c("time", "typeDay"), all.x = TRUE) # column of mean steps matching time
act2$steps[is.na(act2$steps)] <- act2$steps.mean[is.na(act2$steps)] # replace steps=NA with steps.mean
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

The following dataset matches the original data with the exception that missing data (where steps = NA) was replaced with imputed values.

```r
act2 <- act2[, c("steps", "date", "interval"), with=FALSE][order(date, interval)]
act2 # view head & tail with imputed values
```

```
##            steps       date interval
##     1: 2.3333333 2012-10-01        0
##     2: 0.4615385 2012-10-01        5
##     3: 0.1794872 2012-10-01       10
##     4: 0.2051282 2012-10-01       15
##     5: 0.1025641 2012-10-01       20
##    ---                              
## 17564: 1.8717949 2012-11-30     2335
## 17565: 2.0769231 2012-11-30     2340
## 17566: 0.2051282 2012-11-30     2345
## 17567: 0.3076923 2012-11-30     2350
## 17568: 1.4615385 2012-11-30     2355
```

4a. Make a histogram of the total number of steps taken each day

In order to make a histogram of the total number of steps taken each day, the data had to be summarized and some of the time variables were re-calculated based on the new imputed data that replaced steps with NA values.

```r
# Create summary data table with steps.sum and steps.mean by date
stepsByDay2 <- act2[, as.list(unlist(lapply(.SD, keyStats))), by = date, .SDcols=steps]
head(stepsByDay2)
```

```
##          date steps.sum steps.mean
## 1: 2012-10-01  10177.33   35.33796
## 2: 2012-10-02    126.00    0.43750
## 3: 2012-10-03  11352.00   39.41667
## 4: 2012-10-04  12116.00   42.06944
## 5: 2012-10-05  13294.00   46.15972
## 6: 2012-10-06  15420.00   53.54167
```

```r
# Make a histogram of total steps taken each day (now that NA values have been replaced)
plot3 <- qplot(steps.sum, data=stepsByDay2, geom="histogram") +
        geom_histogram(colour = "white", fill = "blue") +
        ggtitle("Histogram of Total Number of Steps Taken Each Day") +
        labs(x="Total Number of Steps", y="Frequency (Number of Days)") +
        scale_y_continuous(breaks = seq(0, 16, 2)) +
        scale_x_continuous(limits=c(0, 25000), breaks=seq(0,25000,5000))
suppressMessages(print(plot3))
```

![](PA1_template_files/figure-html/fig3-histogram-1.png) 

4b. Calculate and report the mean and median total number of steps taken per day

```r
writeLines(paste0("Mean steps per day (with imputed values): ", format(mean(stepsByDay2$steps.sum), big.mark=",", trim=TRUE)))
```

```
## Mean steps per day (with imputed values): 10,762.05
```

```r
writeLines(paste0("Median steps per day (with imputed values): ", format(median(stepsByDay2$steps.sum), big.mark=",", trim=TRUE)))
```

```
## Median steps per day (with imputed values): 10,571
```

4c. Do these values differ from the estimates from the first part of the assignment?

Yes. The mean and median total steps per day calculated with NA values was 9,354.23 and 10,395, respectively. The mean and median total steps per day calculated with imputed values was 10,762.05 and 10,571, respectively. Note that the means and medians before and after imputing values were shown in these sentences using inline code.

4d. What is the impact of imputing missing data on the estimates of the total daily number of steps?

The impact of imputing missing data on the estimates of the total daily number of steps, as seen in a comparison of the histograms created with and without imputed values, was to (1) normalize the data, without the peak at steps equal to zero; and (2) create a higher peak near the mean. The mean and median increased as a result.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

The following steps prepared the factor variable for "weekend" and "weekday" and also re-formatted dates with the revised data, since the dates will be used for the related graph.

```r
act2$time <- sprintf("%04d", act1$interval) # pad with leading 0's for 4-digit character number
act2$time <- paste0(substr(act1$time, 1, 2), ":", substr(act1$time, 3, 4), ":00") # create time
act2 <- within(act1, { datetime=as.POSIXct(strptime(paste(date, time), "%Y-%m-%d %H:%M:%S")) }) # create datetime
act2$typeDay <- c("weekend", "weekday")[(weekdays(as.Date(act2$date), abbreviate = TRUE) %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))+1]
#stepsByTimeByTypeDay3 <- act2[, as.list(unlist(lapply(.SD, keyStats))), by = c("interval", "time", "typeDay"), .SDcols=steps]

# Create summary data table with steps.sum and steps.mean by time (i.e., by interval)
# Note that current date will be substituted for original date since it's ignored in graph (time used)
stepsByTime2 <- act2[, as.list(unlist(lapply(.SD, keyStats))), by = c("time", "typeDay"), .SDcols=steps]
stepsByTime2$time <- as.POSIXct(strptime(stepsByTime2$time, "%H:%M:%S", "GMT"))
stepsByTime2 # view head & tail
```

```
##                     time typeDay steps.sum steps.mean
##   1: 2015-09-14 00:00:00 weekday        91  2.3333333
##   2: 2015-09-14 00:05:00 weekday        18  0.4615385
##   3: 2015-09-14 00:10:00 weekday         7  0.1794872
##   4: 2015-09-14 00:15:00 weekday         8  0.2051282
##   5: 2015-09-14 00:20:00 weekday         4  0.1025641
##  ---                                                 
## 572: 2015-09-14 23:35:00 weekend       176 12.5714286
## 573: 2015-09-14 23:40:00 weekend        94  6.7142857
## 574: 2015-09-14 23:45:00 weekend        26  1.8571429
## 575: 2015-09-14 23:50:00 weekend         0  0.0000000
## 576: 2015-09-14 23:55:00 weekend         0  0.0000000
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
lower <- min(stepsByTime2$time)
upper <- max(stepsByTime2$time)-1
limits = c(lower,upper)
plot4 <- ggplot() +
        geom_line(data = stepsByTime2, aes(x = time, y = steps.mean), colour = "blue") +
        facet_wrap(~typeDay, nrow=2) +
        scale_x_datetime(limits=limits, breaks=("2 hour"), minor_breaks=("1 hour"), labels=date_format("%H:%M")) +
        ggtitle("Average Number of Steps Taken in 5-Minute Time Intervals\nBy Weekday & Weekend") +
        labs(x="Time (Hours:Minutes)", y="Average Number of Steps")
suppressMessages(print(plot4))
```

![](PA1_template_files/figure-html/fig4-timeseries-panel-1.png) 

## Submitting the Assignment
1. Commit completed PA1_template.Rmd
2. a. Commit PA1_template.md url
2. b. Commit PA1_template.html
3. Commit figure directory
4. Push master branch to GitHub
5. a. Submit URL to GitHub repository
5. b. Submit SHA-1 hash

### Final Step
As a final step, the objects created during this session were removed.

```r
rm(list = ls())
```
