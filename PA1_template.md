---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
  pdf_document: default
---

Executive Summary
-----------------
This project analyzes a dataset from apersonal activity monitoring device. 
The device collects data at 5 minute intervals through out the day. 
The data consists of two months of data from an anonymous individual collected 
during the months of October and November, 2012 and include the number of 
steps taken in 5 minute intervals each day.

The data was downloaded from the original source. R was used to process and visualize the data.
Three additional libraries were loaded: ```dplyr```, ```ggplot2```,  and ```timeDate```. 
Some entries in the dataset contain ```NA```. They were filled with the mean values of the 
specific date.  Results did not change significantly after the replacement. 

The results also showed differences in the steps made during the time interval, weekday and weekend.

Libraries used in this project
------------------------------

```{r echo=TRUE}
library(dplyr)
library(ggplot2)
library(timeDate)
```

1. Code for reading in the dataset and/or processing the data
--------------------------------------------------------------

```{r echo=TRUE}
download.file(
   url = 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip', 
   destfile = 'activity.zip',
   method = 'auto'
   )
unzip(
   'activity.zip', 
   exdir='inputdir'
   )
input.raw.data <- read.csv(
   'inputdir/activity.csv', 
   sep = ',',
   header = TRUE
   )
```

2. Histogram of the total number of steps taken each day
--------------------------------------------------------

```{r echo=TRUE}
grouped.data <- input.raw.data %>% group_by(date) 
grouped.data <- grouped.data %>% summarise(steps=sum(steps))
```

Checking the content of the data and its dimension, and determine
the total number of steps in each day:

```{r echo=TRUE}
head(grouped.data)
dim(grouped.data)
```

Plotting the histogram:

```{r echo=TRUE, tidy=FALSE}
processed.data <- data.frame(
   date = grouped.data$date,
   total.steps = grouped.data$steps
   )
ggplot(processed.data, 
   aes(total.steps)
   ) + 
   geom_histogram() +
   xlab('Total Number of Steps') +
   ylab('Frequency') 
```

![Plot 1]('figures/plot1.png')

3. Mean and median number of steps taken each day
-------------------------------------------------

From the object processed.data:

```{r echo=TRUE}
mean.value <-  mean(processed.data$total.steps, na.rm=TRUE)
median.value <-  median(processed.data$total.steps, na.rm=TRUE)

print(mean.value)
print(median.value)

```

4. Time series plot of the average number of steps taken
--------------------------------------------------------

```{r echo=TRUE, tidy=FALSE}
interval.data <- aggregate(steps ~ interval, input.raw.data, mean)
ggplot(
   interval.data, aes(x=interval, y=steps)
   ) + geom_line() +
   xlab('Time Interval') + ylab('Average Number of Steps')
```

![Plot 2]('figures/plot2.png')

5. The 5-minute interval that, on average, contains the maximum number of steps
-------------------------------------------------------------------------------

By looking the previous plot, the highest peak is around the 800 range.

```{r echo=TRUE, tidy=FALSE}
output <- interval.data[which.max(interval.data$steps),]
print(paste('Interval ', output[1,1], sep = ' '))
print(paste('Average Number of Steps ', output[1,2], sep = ' '))
```

6. Code to describe and show a strategy for imputing missing data
-----------------------------------------------------------------

One possibility is to remove all the ```NA``` from the dataset:

```{r echo=TRUE, tidy=FALSE}
clean.raw.data <- input.raw.data[complete.cases(input.raw.data),]
head(clean.raw.data)
```

Another possibility is to fill them in with a value such as the mean or median.
Choosing the mean:

```{r echo=TRUE, tidy=FALSE}
clean.raw.data <- transform(input.raw.data, 
   steps = ifelse(is.na(steps), 
      ave(steps, interval, FUN = function(x) mean(x, na.rm = TRUE)), 
      steps)
   )
```

7. Histogram of the total number of steps taken each day after missing values are imputed
-----------------------------------------------------------------------------------------

Plotting the histogram after cleaning the data:

```{r echo=TRUE, tidy=FALSE}
grouped.data <- clean.raw.data %>% group_by(date) 
grouped.data <- grouped.data %>% summarise(steps=sum(steps))
processed.data <- data.frame(
   date = grouped.data$date,
   total.steps = grouped.data$steps
   )
ggplot(processed.data, 
   aes(total.steps)
   ) + 
   geom_histogram() +
   xlab('Total Number of Steps') +
   ylab('Frequency') 
```

![Plot 3]('figures/plot3.png')

Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily number of steps?

No change with respect to the mean but it slightly changed the value of the median.

```{r echo=TRUE}
mean.value <-  mean(processed.data$total.steps, na.rm=TRUE)
median.value <-  median(processed.data$total.steps, na.rm=TRUE)

print(mean.value)
print(median.value)
```

8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
------------------------------------------------------------------------------------------------------------

Start from input raw data:

```{r echo=TRUE, tidy=FALSE}
clean.raw.data <- transform(input.raw.data, 
   steps = ifelse(is.na(steps), 
      ave(steps, interval, FUN = function(x) mean(x, na.rm = TRUE)), 
      steps
      )
   )
head(clean.raw.data)
```

Determine which row is a weekday (TRUE or FALSE):

```{r echo=TRUE, tidy=FALSE}
weekday.list <- mutate(
   clean.raw.data, 
   weekday.checker = isWeekday(clean.raw.data$date, wday=1:5)
   )
aggregated.data <- aggregate(
   steps ~ interval + weekday.checker, 
   weekday.list,  
   mean
   )

head(aggregated.data)

transformed.data <- transform(
   aggregated.data, 
   weekend.checker = ifelse(
      aggregated.data$weekday.checker == 'FALSE',
      'Weekend',
      'Weekday'
      )
   )

head(transformed.data)

```

Making a panel plot:

```{r echo=TRUE, tidy=FALSE}
ggplot(transformed.data, aes(x=interval, y=steps)) + 
   geom_line() +
   xlab('Time Interval') + ylab('Average Number of Steps') +  
   facet_grid(weekend.checker ~ .)
```

![Plot 4]('figures/plot4.png')

9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
------------------------------------------------------------------------------------------

```
# Loading Library:
library(dplyr)
library(ggplot2)
library(timeDate)

# Dowloading the dataset and loading to the console:
download.file(
   url = 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip', 
   destfile = 'activity.zip',
   method = 'auto'
   )
unzip(
   'activity.zip', 
   exdir='inputdir'
   )
input.raw.data <- read.csv(
   'inputdir/activity.csv', 
   sep = ',',
   header = TRUE
   )


# First Histogram:
grouped.data <- input.raw.data %>% group_by(date) 
grouped.data <- grouped.data %>% summarise(steps=sum(steps))
processed.data <- data.frame(
   date = grouped.data$date,
   total.steps = grouped.data$steps
   )
ggplot(processed.data, 
   aes(total.steps)
   ) + 
   geom_histogram() +
   xlab('Total Number of Steps') +
   ylab('Frequency')

mean.value <-  mean(processed.data$total.steps, na.rm=TRUE)
median.value <-  median(processed.data$total.steps, na.rm=TRUE)
print(mean.value)
print(median.value)


interval.data <- aggregate(steps ~ interval, input.raw.data, mean)
ggplot(
   interval.data, aes(x=interval, y=steps)
   ) + geom_line() +
   xlab('Time Interval') + ylab('Average Number of Steps')


# Processing the NA:
clean.raw.data <- transform(input.raw.data, 
   steps = ifelse(is.na(steps), 
      ave(steps, interval, FUN = function(x) mean(x, na.rm = TRUE)), 
      steps)
   )

grouped.data <- clean.raw.data %>% group_by(date) 
grouped.data <- grouped.data %>% summarise(steps=sum(steps))
processed.data <- data.frame(
   date = grouped.data$date,
   total.steps = grouped.data$steps
   )
ggplot(processed.data, 
   aes(total.steps)
   ) + 
   geom_histogram() +
   xlab('Total Number of Steps') +
   ylab('Frequency') 

mean.value <-  mean(processed.data$total.steps, na.rm=TRUE)
median.value <-  median(processed.data$total.steps, na.rm=TRUE)

# Making the Panel Plot:

clean.raw.data <- transform(input.raw.data, 
   steps = ifelse(is.na(steps), 
      ave(steps, interval, FUN = function(x) mean(x, na.rm = TRUE)), 
      steps
      )
   )

# Determine which row is a weekday (TRUE or FALSE):
weekday.list <- mutate(
   clean.raw.data, 
   weekday.checker = isWeekday(clean.raw.data$date, wday=1:5)
   )
aggregated.data <- aggregate(
   steps ~ interval + weekday.checker, 
   weekday.list,  
   mean
   )

transformed.data <- transform(
   aggregated.data, 
   weekend.checker = ifelse(
      aggregated.data$weekday.checker == 'FALSE',
      'Weekend',
      'Weekday'
      )
   )

ggplot(transformed.data, aes(x=interval, y=steps)) + 
   geom_line() +
   xlab('Time Interval') + ylab('Average Number of Steps') +  
   facet_grid(weekend.checker ~ .)
 
```

