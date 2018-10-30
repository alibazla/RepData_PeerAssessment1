Loading and preprocessing the data
----------------------------------

    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    library(ggplot2)
    activity <- read.csv("activity.csv")
    dim(activity)

    ## [1] 17568     3

    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    names(activity)

    ## [1] "steps"    "date"     "interval"

    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    length(unique(activity$date))

    ## [1] 61

What is mean total number of steps taken per day?
-------------------------------------------------

    activity_total_steps <- with(activity, aggregate(steps, by= list(date), FUN = sum, na.rm = TRUE))
    names(activity_total_steps) <- c("date", "steps")
    hist(activity_total_steps$steps, main = "Total number of steps per day", xlab = "Number of steps", col = "red", ylim = c(0, 20))

![](PA1_RRProject1_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    png("plot1.png")
    mean(activity_total_steps$steps)

    ## [1] 9354.23

    median(activity_total_steps$steps)

    ## [1] 10395

What is the average daily pattern
---------------------------------

    average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN = mean, na.rm = TRUE)
    names(average_daily_activity) <- c("interval", "mean")
    plot(average_daily_activity$interval, average_daily_activity$mean, type = "l", col = "red", lwd = 2, xlab = "Interval", ylab = "Number of steps", main = "Average Number of Steps per Interval")

![](PA1_RRProject1_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    average_daily_activity[which.max(average_daily_activity$mean),]$interval

    ## [1] 835

Imputing Missing Values
-----------------------

    sum(is.na(activity$steps))

    ## [1] 2304

    imputed_steps <- average_daily_activity$mean [match(activity$interval, average_daily_activity$interval)]
    activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))
    total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
    names(total_steps_imputed) <- c("date", "daily_steps")
    hist(total_steps_imputed$daily_steps, col = "red", xlab = "Number of Steps per Day", ylim = c(0, 30), main = "Total Number of Steps per Day")

![](PA1_RRProject1_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    mean(total_steps_imputed$daily_steps)

    ## [1] 10766.19

    median(total_steps_imputed$daily_steps)

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    activity$date <- as.Date(strptime(activity$date, format = "%Y-%m-%d"))

    ## Warning in strptime(activity$date, format = "%Y-%m-%d"): unknown timezone
    ## 'zone/tz/2018e.1.0/zoneinfo/America/New_York'

    activity$datetype <- sapply(activity$date, function(x) {
      if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
      {y <- "Weekend"} else {y <- "Weekday"} 
      y
    })
    activity_by_date <- aggregate(steps~interval + datetype, activity, mean, type = "l", na.rm = TRUE)
    plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
      geom_line() +
      labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
      facet_wrap(~datetype, ncol = 1, nrow=2)
    print(plot)

![](PA1_RRProject1_files/figure-markdown_strict/unnamed-chunk-5-1.png)
