
# Needed libraries
require(knitr)
require(ggplot2)
require(dplyr)
library(scales)

## Loading and preprocessing the data
url_training <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url_training, destfile="activity.zip")
unzip(zipfile="activity.zip")

activity_data = read.csv("activity.csv")

## What is mean total number of steps taken per day?

# Calculate the total number of steps taken per day
steps_per_day <- activity_data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps))
 
# Make a histogram of the total number of steps taken each day
hist(steps_per_day$steps, main = "Total number of steps taken each day", col="blue", xlab="Number of Steps", breaks=20)

# Calculate and report the mean and median of the total number of steps taken per day
mean_steps <- mean(steps_per_day$steps)
median_steps <- median(steps_per_day$steps)

# The mean of the total number of steps taken per day
mean_steps

# The median of the total number of steps taken per day
median_steps

## What is the average daily activity pattern?

interval_data <- activity_data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
  
ggplot(interval_data, aes(x=interval_data, y=steps)) + geom_line(color = "blue")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
interval_data[which.max(interval_data$steps),]


## Imputing missing values

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity_data$steps))

# Create a new dataset 
data_new <- activity_data

# Replace NA values with mean for the 5-minute interval
naVar <- is.na(data_new$steps)
avgValue <- tapply(data_new$steps, data_new$interval, mean, na.rm=TRUE, simplify=TRUE)
data_new$steps[naVar] <- avgValue[as.character(data_new$interval[naVar])]

steps_per_day2 <- data_new %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps))
 
# Make a histogram of the total number of steps taken each day
hist(steps_per_day2$steps, main = "Total number of steps taken each day", col="blue", xlab="Number of Steps", breaks=20)

mean_steps2 <- mean(steps_per_day2$steps)
median_steps2 <- median(steps_per_day2$steps)

# The mean of the total number of steps taken per day
mean_steps2

# The median of the total number of steps taken per day
median_steps2

## Are there differences in activity patterns between weekdays and weekends?

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
data_new <- mutate(data_new, weektype = ifelse(isWeekday(data_new$date, wday=1:5), "weekday", "weekend"))
data_new$weektype <- as.factor(data_new$weektype)

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
interval_full <- data_new %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))

s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)