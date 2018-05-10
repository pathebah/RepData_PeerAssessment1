Activity Monitoring
================
Pathe Bah
May 11, 2018

``` r
library(knitr)
library(dplyr) # For data wrangling
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2) # For plotting nice graphs
```

``` r
# Imporing activity data
actData <- read.csv("activity.csv", header = TRUE, sep = ",")
```

### 1. What is the mean and median number of steps taken per day?

``` r
#  Calculate total number of steps taken per day
dailySteps <- actData %>% group_by(date) %>% summarise(numSteps  = sum(steps))
# Histogram of number steps taken each day
ggplot(data = dailySteps, aes(x = numSteps)) + geom_histogram(fill = "blue", bins = 8) + xlab("Number of Steps") + theme_minimal() + ggtitle("Number of steps taken each day")
```

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](PA1_template_files/figure-markdown_github/Number%20of%20steps%20per%20day-1.png)

``` r
# Mean and Median of number of steps
meanSteps <- mean(na.omit(dailySteps$numSteps))
medSteps <- median(na.omit(dailySteps$numSteps))
```

-   The mean number of steps taken each day is 10766.
-   The median number steps taken each day is 10765.

### 2. What is the average daily activity pattern?

``` r
# Calculate average steps for each interval accross all days
avData <- actData %>% group_by(interval) %>% summarise(avgSteps  = mean(na.omit(steps)))
# Plot time series of daily activity pattern
maxSteps <- which.max(avData$avgSteps)
maxInterval <- avData[maxSteps,1]
ggplot(data = avData, aes(x = interval, y = avgSteps)) + geom_line(color = "red") + xlab("Interval") + ylab("Number of steps") + ggtitle("Average daily activity pattern") + geom_vline(aes(xintercept = maxInterval), color = "blue") + geom_text(aes(maxInterval,0,label = maxInterval, hjust = 0.5, vjust = 0.5)) + theme_minimal() + ggtitle("Average number of steps taken daily per 5 minute interval")
```

    ## Don't know how to automatically pick scale for object of type tbl_df/tbl/data.frame. Defaulting to continuous.

![](PA1_template_files/figure-markdown_github/Daily%20activity%20pattern-1.png)

The 5-minute interval with the maximum average number of steps across all days is 835. This is indicated by the blue line in the graph above.

3. Imputing missing values
--------------------------

``` r
# Retreive the indexes of all row with NA
naIndex <- which(is.na(actData$steps))
numNA <- length(naIndex)
```

The number of missing values is 2304

``` r
# Add a column containing the average steps for each interval
noNAData <- inner_join(actData,avData, by = "interval")
# Replace missing values with average for the corresponding 5-minute interval.
noNAData <- noNAData %>% mutate(steps = ifelse(is.na(steps),avgSteps,steps))
dailyNoNA <- noNAData %>% group_by(date) %>% summarize(steps = sum(steps))
# Plot histogram of daily steps after removing all NA's from the dataset
ggplot(data = dailyNoNA, aes(x = steps)) + geom_histogram(fill = "blue", bins = 8) + xlab("Number of Steps") + ggtitle("Number of steps taken each day without missing values")
```

![](PA1_template_files/figure-markdown_github/Fill%20in%20missing%20values-1.png)

``` r
# Compute mean and median of steps with no missing values
meanStepsNoNA <- mean(dailyNoNA$steps)
medStepsNoNA <- median(dailyNoNA$steps)
```

-   The mean number of steps is 10766.
-   The median number steps is 10766.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

``` r
# Process data for the charts
wdData <- noNAData %>% mutate(wd = weekdays(as.Date(date)))
wdData <- wdData %>% mutate(wknd = ifelse(wd %in% c("Saturday","Sunday"),"weekend","weekday"))
wdata <- wdData %>% group_by(interval, wknd) %>%  summarize(steps = mean(steps))
# Plot activity patterns
ggplot(data = wdata, aes(x = interval, y = steps, color = wknd)) + geom_line() +facet_wrap(~wknd, nrow = 2) + ggtitle("Differences in activity pattern between weekdays and weekends")
```

![](PA1_template_files/figure-markdown_github/Differences%20in%20activity%20patterns-1.png)

There seems to be difference in activity pattern between interval 500 to 750 and between 1000 and 2000. People are less active in the early intervals in weekends but more active throughout the rest of the day.
