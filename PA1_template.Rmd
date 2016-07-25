---
title: "Reproductible Research"
author: "LDS"
date: "25 juillet 2016"
output: html_document
---

#Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:
**Dataset**: Activity monitoring data [52K]

The variables included in this dataset are:  
1.**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
2.**date**: The date on which the measurement was taken in YYYY-MM-DD format  
3.**interval**: Identifier for the 5-minute interval in which measurement was taken  

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

#------------------------------------------
##Load and process the data
#------------------------------------------
1 - Reading the data from the "activity.csv"

```{r,echo=TRUE}
DataActivity <- read.table("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
```

2 - Some preliminary viewing :

```{r,echo=TRUE}
summary(DataActivity)
str(DataActivity)
```

```{r,echo=TRUE}
head(DataActivity)
```
3. In this data set, dates are factors, they must be converted into date format, and intervals into factors to perform plottings

```{r,echo=TRUE}
DataActivity$date <- as.Date(DataActivity$date, format = "%Y-%m-%d")
DataActivity$interval <- factor(DataActivity$interval)
```

#------------------------------------------
##What is mean total number of steps taken per day?
#------------------------------------------
For this part of the assignment, you can ignore the missing values in the dataset.


1. Ignoring missing values : creating a new dataset without NAs in steps variable

```{r,echo=TRUE}
DataActivity_noNA <- DataActivity[!is.na(as.character(DataActivity$steps)),]
head(DataActivity_noNA)
```
2. Calculating total number of steps per day
```{r,echo=TRUE}
#New dataframe with total of steps per day
StepsperDay <- steps_each_day <- aggregate(steps ~ date, data = DataActivity_noNA, sum)
#Column names
colnames(StepsperDay) <- c("date", "steps")
```
3. Make a histogram of the total number of steps taken each day

```{r,echo=TRUE}
hist(as.numeric(StepsperDay$steps), breaks = 20, col = "blue", xlab = "Number of Steps", main= "Total number of steps taken each day")
```

4.Calculate and report the mean and median of the total number of steps taken per day

```{r,echo=TRUE}
#Mean
mean(StepsperDay$steps)
```

```{r,echo=TRUE}
#Median
median(StepsperDay$steps)
```
#------------------------------------------
##What is the average daily activity pattern?
#------------------------------------------
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r,echo=TRUE}
#Average of number os steps per interval
StepsMeanInterval <- aggregate(steps ~ interval, data = DataActivity_noNA, mean)

#Adding columns names
colnames(StepsMeanInterval) <- c("interval", "AverageSteps")

#ploting the average daily activity pattern 
plot(as.integer(levels(StepsMeanInterval$interval)), StepsMeanInterval$AverageSteps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="green")
```

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r,echo=TRUE}
MaxSteps <- max(StepsMeanInterval$AverageSteps)
MaxSteps


#The 5-minute interval that contains the maximum number of steps
MaxInterval<-StepsMeanInterval[which.max(StepsMeanInterval$AverageSteps),]$interval
MaxInterval
```
Interval number *835* contains the maximum average number of steps *206,16 steps*

#------------------------------------------
##Imputing missing values
#------------------------------------------
1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

For the "steps" variable:
```{r,echo=TRUE}
sum(is.na(as.character(DataActivity$steps)))
```
For the "date" variable:
```{r,echo=TRUE}
sum(is.na(as.character(DataActivity$date)))
```
For the "interval" variable:
```{r,echo=TRUE}
sum(is.na(as.character(DataActivity$interval)))
```  

2.Devise a strategy for filling in all. Stategy : give the average step for the considered interval

```{r,echo=TRUE}
#Index of missing values (NAs)
NA_index <- which(is.na(as.character(DataActivity$steps)))
AllData <- DataActivity

#Imputing missing values and creating a new dataset
AllData[NA_index, ]$steps<-unlist(lapply(NA_index, FUN=function(NA_index){
                StepsMeanInterval[DataActivity[NA_index,]$interval==StepsMeanInterval$interval,]$AverageSteps
                }))
```                 

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.   

```{r,echo=TRUE}
#New data frame with the extrapoled data
StepsperDayALL <- aggregate(steps ~ date, data = AllData, sum)
#Column names
colnames(StepsperDayALL) <- c("date", "steps")
#histogram of the total number of steps taken each day
hist(as.numeric(StepsperDayALL$steps), breaks = 20, col = "blue", xlab = "Number of Steps", main= "Total number of steps taken each day_all data")
``` 

Do these values differ from the estimates from the first part of the assignment?  

```{r,echo=TRUE}
#Mean
mean(StepsperDayALL$steps)
```

```{r,echo=TRUE}
#Median
median(StepsperDayALL$steps)
```  

What is the impact of imputing missing data on the estimates of the total daily number of steps?
The estimates only differ slightly for the median.
The means are identical.


#------------------------------------------
##Differences in activity patterns between weekdays and weekends?
#------------------------------------------


1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r,echo=TRUE}
#Factor variable "day" to store the day of the week:
AllData$day <- as.factor(weekdays(AllData$date))

#New variable "type" classifying the days between "Weekday" or "Weekend":
AllData$type <- ifelse(!(AllData$day %in% c("samedi","dimanche")), "Weekday", "Weekend") 
``` 

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r,echo=TRUE}
SetpsIntervalType <- aggregate(steps ~ interval*type, data = AllData, mean)
names(SetpsIntervalType) <- c("interval", "type", "StepsAverage")
library(lattice)

xyplot(StepsAverage ~ interval | type, SetpsIntervalType, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")

```
