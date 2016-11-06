---
title: "DS_C5_W2_assign: Markdown Practice - Personal Movement (Activity) Analysis"
output: html_document
keep_md: TRUE
---



### 1. Reading the dataset

```r
temp <- tempfile()
download.file("https://github.com/atreshchev/RepData_PeerAssessment1/raw/master/activity.zip", temp)
con <- unz(temp, "activity.csv")

DF <- read.table(con, sep = ",", header = TRUE, na.strings = "NA")
DF$date <- as.Date(DF$date)

unlink(temp)
```

### 2. Evaluating & drawing the total number of steps taken each day 

```r
library(plyr)
total_activity <- ddply(DF, .(date), summarize, Sum = sum(steps, na.rm = TRUE))

plot(total_activity, type = "b", pch = 19, xaxt = "n",
     main = "Total number of steps taken each day\n in October-November",
     ylab = "Total number of steps", xlab = "Date")
  axis.Date(side = 1, at = seq(min(DF$date),
                               max(DF$date), by = "days"), format = "%m/%d\n (%a)", cex.axis = .7)
```

<img src="PA1_template_files/figure-html/total_steps_drawing-1.png" width="960" />

### 3. Evaluating & listing mean and median numbers of steps taken each day

```r
mean_activity <- ddply(DF, .(date), summarize, Mean = mean(steps, na.rm = TRUE))
median_activity <- ddply(DF, .(date), summarize, Median = median(steps, na.rm = TRUE))
actDF <- data.frame(Date = unique(as.character(DF$date)), 
                    Mean = ceiling(mean_activity$Mean), 
                    Median = ceiling(median_activity$Median))
actDF
```

```
##          Date Mean Median
## 1  2012-10-01  NaN     NA
## 2  2012-10-02    1      0
## 3  2012-10-03   40      0
## 4  2012-10-04   43      0
## 5  2012-10-05   47      0
## 6  2012-10-06   54      0
## 7  2012-10-07   39      0
## 8  2012-10-08  NaN     NA
## 9  2012-10-09   45      0
## 10 2012-10-10   35      0
## 11 2012-10-11   36      0
## 12 2012-10-12   61      0
## 13 2012-10-13   44      0
## 14 2012-10-14   53      0
## 15 2012-10-15   36      0
## 16 2012-10-16   53      0
## 17 2012-10-17   47      0
## 18 2012-10-18   35      0
## 19 2012-10-19   42      0
## 20 2012-10-20   37      0
## 21 2012-10-21   31      0
## 22 2012-10-22   47      0
## 23 2012-10-23   31      0
## 24 2012-10-24   30      0
## 25 2012-10-25    9      0
## 26 2012-10-26   24      0
## 27 2012-10-27   36      0
## 28 2012-10-28   40      0
## 29 2012-10-29   18      0
## 30 2012-10-30   35      0
## 31 2012-10-31   54      0
## 32 2012-11-01  NaN     NA
## 33 2012-11-02   37      0
## 34 2012-11-03   37      0
## 35 2012-11-04  NaN     NA
## 36 2012-11-05   37      0
## 37 2012-11-06   29      0
## 38 2012-11-07   45      0
## 39 2012-11-08   12      0
## 40 2012-11-09  NaN     NA
## 41 2012-11-10  NaN     NA
## 42 2012-11-11   44      0
## 43 2012-11-12   38      0
## 44 2012-11-13   26      0
## 45 2012-11-14  NaN     NA
## 46 2012-11-15    1      0
## 47 2012-11-16   19      0
## 48 2012-11-17   50      0
## 49 2012-11-18   53      0
## 50 2012-11-19   31      0
## 51 2012-11-20   16      0
## 52 2012-11-21   45      0
## 53 2012-11-22   71      0
## 54 2012-11-23   74      0
## 55 2012-11-24   51      0
## 56 2012-11-25   42      0
## 57 2012-11-26   39      0
## 58 2012-11-27   48      0
## 59 2012-11-28   36      0
## 60 2012-11-29   25      0
## 61 2012-11-30  NaN     NA
```

### 4. Drawing average (mean) and median numbers of steps time series

```r
plot(mean_activity, type = "b", pch = 19, xaxt = "n", col = "darkblue",
     main = "Mean and median numbers of steps taken each day\n in October-November",
     ylab = "Mean and median numbers of steps", xlab = "Date")
  points(median_activity, type = "b", pch = 19, col = "darkgreen")
  axis.Date(side = 1, at = seq(min(DF$date),
                               max(DF$date), by = "days"), format = "%m/%d\n (%a)", cex.axis = .7)
  legend("topleft", pch = 19, col = c("darkblue", "darkgreen"),
         legend = c("steps mean", "steps median"))
```

<img src="PA1_template_files/figure-html/mean_median_steps_drawing-1.png" width="960" />

### 5. Evaluate the 5-minute interval that, on average, contains the maximum number of steps

```r
DF[which.max(DF$steps), ]
```

```
##       steps       date interval
## 16492   806 2012-11-27      615
```

### 6. Describing and showing a strategy for imputing missing data

Exploratory analysis showed that NA values presented in no single value but as large NA-sequences - most likely corresponding to the time when the traction device was discharged.

In this case, the most appropriate way is to restore NA values by analyzing the number of steps equivalent to the values in time intervals of all other days for which the data values are not skipped.

Suchlike approach can be automatically implemented by the 'nearest neighbor averaging' (kNN) algorithm and can be used out of the 'VIM' CRAN-package, for example.

```r
NAvect <- is.na(DF$steps)
plot(NAvect, type = "l", yaxt = "n", xaxt = "n",
     main = "8 days with 5-min intervals NA-values\n in October-November",
     ylab = "NA-value indicator", xlab = "Intervals mapped to days")
  axis(side = 2, at = 0:1, labels = c("FALSE", "TRUE"), cex.axis = .7)  
  axis(1, at = seq(0, 17568-1, 288), labels = format(unique(DF$date), "%m/%d\n (%a)"), cex.axis = .7)
```

<img src="PA1_template_files/figure-html/days_with_NA_val-1.png" width="960" />


```r
library(VIM)
DFrecovery <- kNN(DF, variable = c("steps"), k = 2,
                  dist_var = c("date", "interval"))
```

### 7. Drawing the histogram of the total number of steps taken each day after missing values are imputed

```r
plot(DFrecovery$steps, type = "l", col = "blue",
     main = "The total number of steps per 5-min interval\n (incl. imputed values based on kNN)",
     ylab = "Number of Steps", xlab = "5-min Interval Observations")
  lines(DF$steps)
  legend("topleft", pch = 19, col = c("blue"), legend = c("imputed values"))
```

<img src="PA1_template_files/figure-html/imp_data_hist-1.png" width="960" />

### 8. Drawing the Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
library(dplyr)
DFtmp <- cbind(DF, wday = as.POSIXlt(DF$date)$wday) # using because of dplyr::mutate conflicts with 'knit' evaluating
DFtmp <- cbind(DFtmp, wend = ifelse(DFtmp$wday %in% 1:5, "Weekday", "Weekend"))
DFwend <- DFtmp %>% group_by(interval, wend) %>% summarize(meansteps = mean(steps, na.rm = TRUE)) 
DFwday <- DFtmp %>% group_by(interval, wday) %>% summarize(meansteps = mean(steps, na.rm = TRUE)) 

library(lattice)
xyplot(meansteps ~ interval | wend, DFwend, type = "l", 
       main = "Average Activity at Weekdays and Weekends",
       xlab = "Intervals", ylab = "Average Number of Steps",
       layout = c(1, 2), as.table = TRUE
)
```

<img src="PA1_template_files/figure-html/compare_num_steps-1.png" width="960" />

```r
xyplot(meansteps ~ interval | wday, DFwday, type = "l", 
       main = "Average Activity per Days",
       xlab = "Intervals", ylab = "Average Number of Steps",
       layout = c(1, 7), as.table = TRUE, strip = FALSE,    # draw top-to-bottom panels (as.table = TRUE) & without panels' labels (strip = FALSE)
       panel = function(x = interval, y = meansteps, ...) {
         panel.xyplot(x, y,...)
         panel.text(0, 0.85*max(DFwday$meansteps), 
                    labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                               "Friday", "Saturday")[which.packet()])
       })
```

<img src="PA1_template_files/figure-html/compare_num_steps-2.png" width="960" />

### 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

```r
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# 1. Reading the dataset
temp <- tempfile()
download.file("https://github.com/atreshchev/RepData_PeerAssessment1/raw/master/activity.zip", temp)
con <- unz(temp, "activity.csv")

DF <- read.table(con, sep = ",", header = TRUE, na.strings = "NA")
DF$date <- as.Date(DF$date)

unlink(temp)

# 2. Evaluating & drawing the total number of steps taken each day 
library(plyr)
total_activity <- ddply(DF, .(date), summarize, Sum = sum(steps, na.rm = TRUE))

plot(total_activity, type = "b", pch = 19, xaxt = "n",
     main = "Total number of steps taken each day\n in October-November",
     ylab = "Total number of steps", xlab = "Date")
  axis.Date(side = 1, at = seq(min(DF$date),
                               max(DF$date), by = "days"), format = "%m/%d\n (%a)", cex.axis = .7)

# 3. Evaluating & listing mean and median numbers of steps taken each day
mean_activity <- ddply(DF, .(date), summarize, Mean = mean(steps, na.rm = TRUE))
median_activity <- ddply(DF, .(date), summarize, Median = median(steps, na.rm = TRUE))
actDF <- data.frame(Date = unique(as.character(DF$date)), 
                    Mean = ceiling(mean_activity$Mean), 
                    Median = ceiling(median_activity$Median))
actDF

# 4. Drawing average (mean) and median numbers of steps time series
plot(mean_activity, type = "b", pch = 19, xaxt = "n", col = "darkblue",
     main = "Mean and median numbers of steps taken each day\n in October-November",
     ylab = "Mean and median numbers of steps", xlab = "Date")
  points(median_activity, type = "b", pch = 19, col = "darkgreen")
  axis.Date(side = 1, at = seq(min(DF$date),
                               max(DF$date), by = "days"), format = "%m/%d\n (%a)", cex.axis = .7)
  legend("topleft", pch = 19, col = c("darkblue", "darkgreen"),
         legend = c("steps mean", "steps median"))

# 5. Evaluate the 5-minute interval that, on average, contains the maximum number of steps
DF[which.max(DF$steps), ]

# 6. Describing and showing a strategy for imputing missing data
NAvect <- is.na(DF$steps)
plot(NAvect, type = "l", yaxt = "n", xaxt = "n",
     main = "8 days with 5-min intervals NA-values\n in October-November",
     ylab = "NA-value indicator", xlab = "Intervals mapped to days")
  axis(side = 2, at = 0:1, labels = c("FALSE", "TRUE"), cex.axis = .7)  
  axis(1, at = seq(0, 17568-1, 288), labels = format(unique(DF$date), "%m/%d\n (%a)"), cex.axis = .7)

library(VIM)
DFrecovery <- kNN(DF, variable = c("steps"), k = 2,
                  dist_var = c("date", "interval"))

# 7. Drawing the histogram of the total number of steps taken each day after missing values are imputed
plot(DFrecovery$steps, type = "l", col = "blue",
     main = "The total number of steps per 5-min interval\n (incl. imputed values based on kNN)",
     ylab = "Number of Steps", xlab = "5-min Interval Observations")
  lines(DF$steps)
  legend("topleft", pch = 19, col = c("blue"), legend = c("imputed values"))

# 8. Drawing the Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
library(dplyr)
DFtmp <- cbind(DF, wday = as.POSIXlt(DF$date)$wday) # using because of dplyr::mutate conflicts with 'knit' evaluating
DFtmp <- cbind(DFtmp, wend = ifelse(DFtmp$wday %in% 1:5, "Weekday", "Weekend"))
DFwend <- DFtmp %>% group_by(interval, wend) %>% summarize(meansteps = mean(steps, na.rm = TRUE)) 
DFwday <- DFtmp %>% group_by(interval, wday) %>% summarize(meansteps = mean(steps, na.rm = TRUE)) 

library(lattice)
xyplot(meansteps ~ interval | wend, DFwend, type = "l", 
       main = "Average Activity at Weekdays and Weekends",
       xlab = "Intervals", ylab = "Average Number of Steps",
       layout = c(1, 2), as.table = TRUE
)

xyplot(meansteps ~ interval | wday, DFwday, type = "l", 
       main = "Average Activity per Days",
       xlab = "Intervals", ylab = "Average Number of Steps",
       layout = c(1, 7), as.table = TRUE, strip = FALSE,    # draw top-to-bottom panels (as.table = TRUE) & without panels' labels (strip = FALSE)
       panel = function(x = interval, y = meansteps, ...) {
         panel.xyplot(x, y,...)
         panel.text(0, 0.85*max(DFwday$meansteps), 
                    labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                               "Friday", "Saturday")[which.packet()])
       })
```
