# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data



```r

setwd("C:/Users/Dan/Desktop/Coursera/5. Reproducible Research/Proj1")  # set working directory

if (!file.exists("activity.zip")) stop("Zip file missing")  # check if file exists  
activ_dat <- read.csv(unz("activity.zip", "activity.csv"), header = T)  # read in data
activ_dat_no_na <- activ_dat[!is.na(activ_dat[, 1]), ]  # removing obs. with missing no.of steps
```



## What is mean total number of steps taken per day?


```r

steps_day <- aggregate(steps ~ date, data = activ_dat_no_na, sum)  # aggregate steps by day
hist(steps_day$steps, breaks = 10, main = "Daily Number of Steps (Histogram)", 
    xlab = "Number of Steps", ylab = "Frequency of Daily Number of Steps")  # create histogram
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r

steps_day_med <- round(median(steps_day$steps), 0)  # median rounded to integer value
steps_day_mean <- round(mean(steps_day$steps), 0)  # mean rounded to integer value
```


- The average number of steps per day (October-November) reported for the given individual was 
1.0766 &times; 10<sup>4</sup> while the median number of steps was 1.0765 &times; 10<sup>4</sup>.


```r

## Export plot to png

dev.copy(png, "fig1_hist_days.png", width = 800, height = 600)
dev.off()
```



## What is the average daily activity pattern?


```r

steps_interval <- aggregate(steps ~ interval, data = activ_dat_no_na, mean)  # aggregate steps by interval
plot(steps_interval$interval, steps_interval$steps, type = "l", main = "Average Number of Steps per 5-min Interval", 
    xlab = "5-min Interval", ylab = "Average Number of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r

steps_interval_max <- steps_interval[steps_interval$steps == max(steps_interval$steps), 
    1]
```


- On average, the 5-min time interval with the highest mean average number of steps was 835 -- 840.


```r

## Export plot to png

dev.copy(png, "fig2_tseries_interval.png", width = 800, height = 600)
dev.off()
```


## Imputing missing values


```r

activ_dat2 <- read.csv(unz("activity.zip", "activity.csv"), header = T)  # re-read in data in new file 
na_count <- sum(is.na(activ_dat2[, 1]))  ## count obs. with missing steps
na_index <- which(is.na(activ_dat2[, 1]) == TRUE)  ## retain index of obs with missing steps

for (i in 1:na_count) {
    activ_dat2[na_index[i], 1] <- steps_interval[(steps_interval$interval == 
        activ_dat2[na_index[i], 3]), 2]
    ## assign obs with missing steps the corresponding mean interval step values
    ## over the timeframe
}

## Data with filled missing information

steps_day2 <- aggregate(steps ~ date, data = activ_dat2, sum)  # aggregate steps by day
hist(steps_day2$steps, breaks = 10, main = "Modified - Daily Number of Steps (Histogram)", 
    xlab = "Number of Steps", ylab = "Frequency of Daily Number of Steps")  # create histogram
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r

steps_day_med2 <- round(median(steps_day2$steps), 0)  # median rounded to integer value
steps_day_mean2 <- round(mean(steps_day2$steps), 0)  # mean rounded to integer value
```


- The average number of steps per day (October-November) when missing data is filled in and that was reported for the given individual was 1.0766 &times; 10<sup>4</sup> while the median number of steps was 1.0766 &times; 10<sup>4</sup>. The no-missing data mean was 1.0766 &times; 10<sup>4</sup> and the medin was 1.0765 &times; 10<sup>4</sup>. Adding in mean values for the missing steps has no significant inpact on the daily distribution of steps other than increasing the frequency of the very centre of the distribution (due to adding days with totals that fall straight in the centre of the distribution). The mean does not change at all while the median changes ever so slightly, getting even closer to the mean.


```r

## Export plot to png

dev.copy(png, "fig3_hist_interval_nomiss.png", width = 800, height = 600)
dev.off()
```


## Are there differences in activity patterns between weekdays and weekends?


```r
# Note: Original data is used; na's (steps) are removed

activ_dat[, 4] <- weekdays(as.Date(activ_dat$date))  # create column of day names based on date column
names(activ_dat)[4] <- "DayName"
activ_dat_no_na <- activ_dat[!is.na(activ_dat[, 1]), ]  # removing obs. with missing no.of steps

activ_dat_no_na[activ_dat_no_na$DayName %in% c("Monday", "Tuesday", "Wednesday", 
    "Thursday", "Friday"), 5] <- "weekday"
activ_dat_no_na[activ_dat_no_na$DayName %in% c("Saturday", "Sunday"), 5] <- "weekend"  # split data by weekday, weekend
names(activ_dat_no_na)[5] <- "TypeOfDay"  # creata type of day variable and assign it a name 


## Aggrgate and plot

totals <- aggregate(steps ~ interval + TypeOfDay, data = activ_dat_no_na, FUN = mean)

library(ggplot2)  ## initiate library

## initiate ggplot object and add in features

gplot <- ggplot(totals, aes(interval, steps, group = TypeOfDay))
gplot <- gplot + facet_grid(TypeOfDay ~ .) + geom_line(color = "blue", size = 1)
gplot <- gplot + labs(title = "Average Number of Steps Taken Across Days")
gplot <- gplot + xlab("Interval") + ylab("Average Number of Steps")

print(gplot)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 



```r


## Export plot to png

dev.copy(png, "fig4_panel_days.png", width = 800, height = 600)
dev.off()
```

