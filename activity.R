##Unzip and read file in##


if (!file.exists('activity')) 
  unzip('activity.zip')

  
activity <- read.csv('activity.csv') 

##Ignoring missing values, sum the total steps taken per day##

steps_day <- aggregate(steps ~ date, activity, sum)

##Create histogram of steps taken per day##

hist(steps_day$steps, main = "Histogram of Steps Taken Per Day", xlab = "Number of Steps", col = "green")

##Calculate Mean and Median steps taken per day##

stepmean <- mean(steps_day$steps)
stepmedian <- median(steps_day$ste)

##Average Daily Pattern##
##Time series plot of the 5 minute interval and average steps taken across all days##

steps_interval <- aggregate(steps ~ interval, activity, mean)

plot(steps_interval$interval, steps_interval$steps, type = "l", main = "Average Steps Taken (All Days) by Interval", xlab = "Interval", ylab = "Steps")

##Which interval has maximum number off steps##

intervalmax <- steps_interval[which.max(steps_interval$steps),1]

##Count number of missing values##

missing <- sum(!complete.cases(activity))

##Impute missing values with average steps of interval##

imputed <- transform(activity, steps = ifelse(is.na(activity$steps), steps_interval$steps[match(activity$interval, steps_interval$interval)], activity$steps))


##Recalculate previous processes with imputed data##
##Sum total steps taken per day##

steps_day_i <- aggregate(steps ~ date, imputed, sum)

##Create histogram of steps taken per day##

hist(steps_day_i$steps, main = "Histogram of Steps Taken Per Day", xlab = "Number of Steps", col = "green")

##Calculate Mean and Median steps taken per day##

stepmean_i <- mean(steps_day_i$steps)
stepmedian_i <- median(steps_day_i$steps)

##Did imputing values create differences in mean and median steps?##

mean_diff <- stepmean - stepmean_i
median_diff <- stepmedian - stepmedian_i

##Create a factor variable for weekend versus weekday##

weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed$day = as.factor(ifelse(is.element(weekdays(as.Date(imputed$date)),weekday), "Weekday", "Weekend"))


steps_interval_i <- aggregate(steps ~ interval + day, imputed, mean)

library(lattice)

xyplot(steps_interval_i$steps ~ steps_interval_i$interval|steps_interval_i$day, main="Average Steps Taken (All Days) by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")

