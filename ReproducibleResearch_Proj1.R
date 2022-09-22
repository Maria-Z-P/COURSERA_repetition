setwd("C:/Maja/M/R Programming_Coursera")

#-----
# Loading and preprocessing the data

dane <- read.csv('activity.csv')
class(dane)
head(dane)
str(dane)

View(dane)

#---
d <- as.Date(dane$date)
class(d)

library(stringr)
library(tidyr)
#library(dplyr)
library(lubridate)

dane$ho <- str_sub(dane$interval, -4, -3)  # minus (-) znaczy od konca
dane$min <- str_sub(dane$interval, -2, -1)
dane$sec <- rep("00", dim(dane)[1])        # dim(dane)[1] znaczy ostatnia obs.
                                           # dim(dane)[2] znaczy ostatnia zmienna
dane$ho <- as.numeric(dane$ho)
dane$ho <- as.character(dane$ho)

dane[is.na(dane$ho),"ho"] <- "00"   # NA is replaced with zero

mydata <- unite(dane, "time", c("ho", "min", "sec"), sep = ':')
# unite {tidyr} laczy kolumny

head(mydata)
tail(mydata)
str(mydata)

hms(mydata$time[25]) # ms {lubridate} transforms a character or numeric vector 
# into a period object with the specified number of hours, minutes, and seconds. 

View(mydata)
#---

#-----
# What is mean total number of steps taken per day?

# (For this part of the assignment, you can ignore the missing values in the 
#  dataset.)

# 1. Calculate the total number of steps taken per day
# 2. Make a histogram of the total number of steps taken each day
# 3. Calculate and report the mean and median of the total number of steps taken
#    per day

steps_per_d <- tapply(dane$steps, dane$date, sum)
hist(steps_per_d, col = "green")
mean_steps_per_d <- mean(steps_per_d, na.rm = TRUE)
median_steps_per_d <- median(steps_per_d, na.rm = TRUE)
print(mean_steps_per_d)
print(median_steps_per_d)

#--- 
dim(steps_per_d)
steps_per_d[3]   # 3.10.2012 (np.)
dane$steps[577:864]
sum(dane$steps[577:864])
hist(dane$steps[577:864])
#---

#-----
# What is the average daily activity pattern?

# 1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") 
#    of the 5-minute interval (x-axis) and the average number of steps taken, 
#    averaged across all days (y-axis)
# 2. Which 5-minute interval, on average across all the days in the dataset, 
#    contains the maximum number of steps?

rob <- dane[!is.na(dane$steps),]    # nie bêdzie wierszy steps NA
avg_steps_per_int <- tapply(rob$steps, rob$interval, mean)

dim(avg_steps_per_int)    # 288
num_of_int <- dim(dane)[1]/(31+30)
num_of_int                # 288

plot(x=dane$interval[1:288], y=avg_steps_per_int, type = "l", xlab = "Interval")
max(avg_steps_per_int)        # 206.1698
which.max(avg_steps_per_int)  # 8 h, 35 m

#---
View(avg_steps_per_int)
#---

#-----
# Imputing missing values

# (Note that there are a number of days/intervals where there are missing 
#  values (coded as \color{red}{\verb|NA|}NA). The presence of missing days 
#  may introduce bias into some calculations or summaries of the data.)
# 
# 1. Calculate and report the total number of missing values in the dataset 
#    (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
# 2. Devise a strategy for filling in all of the missing values in the dataset. 
#    The strategy does not need to be sophisticated. For example, you could use 
#    the mean/median for that day, or the mean for that 5-minute interval, etc.
# 3. Create a new dataset that is equal to the original dataset but with the 
#    missing data filled in.
# 4. Make a histogram of the total number of steps taken each day and Calculate
#    and report the mean and median total number of steps taken per day. Do 
#    these values differ from the estimates from the first part of the 
#    assignment? What is the impact of imputing missing data on the estimates of
#    the total daily number of steps?

summary(dane)
head(!complete.cases(dane))        # wiersze z brakuj¹cymi wartoœciami bêd¹ TRUE
head(dane[!complete.cases(dane), ])

# Proxy missing values with median_steps_per_d/num_of_int
new_data <- dane
new_data[is.na(new_data$steps),"steps"] <- median_steps_per_d/num_of_int
# check:
any(is.na(new_data))
new_data[1:5,]

new_steps_per_d <- tapply(new_data$steps, new_data$date, sum)
hist(new_steps_per_d, col = "green")
mean_new_steps_per_d <- mean(new_steps_per_d)
median_new_steps_per_d <- median(new_steps_per_d)
print(mean_new_steps_per_d)
print(median_new_steps_per_d)

#-----
# Are there differences in activity patterns between weekdays and weekends?

# (For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of
#  some help here. Use the dataset with the filled-in missing values for this 
#  part.)
# 
# 1. Create a new factor variable in the dataset with two levels – “weekday” and
#    “weekend” indicating whether a given date is a weekday or weekend day.
# 2. Make a panel plot containing a time series plot 
#    (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval 
#    (x-axis) and the average number of steps taken, averaged across all weekday
#    days or weekend days (y-axis). See the README file in the GitHub repository
#    to see an example of what this plot should look like using simulated data.

#library(lubridate)

rob2 <- wday(as.Date(new_data$date)) # 1 = Sunday, 2 = Monday, 3 = Tuesday, etc.

for (i in 1:dim(new_data)[1]) {
    if(rob2[i] == 7 | rob2[i] == 1) {
        new_data$weekdays[i] <- "weekend"
    } else {
        new_data$weekdays[i] <- "weekday" 
    } 
}

head(new_data)
new_data$weekdays <- as.factor(new_data$weekdays)

table(new_data$weekdays)
table(new_data$weekdays)/288

rob3 <- new_data[new_data$weekdays=="weekend",]
avg_weekend <- tapply(rob3$steps, rob3$interval, mean)
rob4 <- new_data[new_data$weekdays=="weekday",]
avg_weekday <- tapply(rob4$steps, rob4$interval, mean)
x <- c(new_data$interval[1:288], new_data$interval[1:288])
y <- c(avg_weekend, avg_weekday)
f <- as.factor(c(rep("weekend", 288), rep("weekday", 288)))

library(lattice)

xyplot(y ~ x | f, layout=c(1,2), type = "l", lwd = 2, xlab = "Interval", 
       ylab = "Number of steps")

#---
max(avg_weekend)         # 157.7973
which.max(avg_weekend)   # 9 h 15 m
max(avg_weekday)         # 207.8727
which.max(avg_weekday)   # 8 h 35 m
#---