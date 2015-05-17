library(data.table)
library(lubridate)
library(dplyr)
library(lattice)

# Set working sub directory to keep files together
mainDir <- getwd()
subDir <- "RepData_PeerAssessment1"

if (file.exists(subDir)){
    setwd(file.path(mainDir, subDir))
} else {
    dir.create(file.path(mainDir, subDir))
    setwd(file.path(mainDir, subDir))    
}

# download file, unzip and load data
if (!file.exists("./activity.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                  method='curl', 
                  destfile="./activity.zip"
    )
    allData <-  unzip("./activity.zip",exdir = getwd())
    allData <- "./activity.csv"
} 

activityDT <- fread("./activity.csv", sep="auto", 
                    header="auto", na.strings=c("NA"), 
                    stringsAsFactors=FALSE, verbose=FALSE)
activityDT$date <- as.Date(ymd(activityDT$date))

# Calculate total number of steps taken per day
dailyTsteps <- tapply(activityDT$steps, activityDT$date, sum)

# Create Histogram of total number of steps taken per day
hist(dailyTsteps, main="Frequency of total daily steps", 
    xlab="Daily steps", ylab="Number of Days", col="blue")

# Calculate the mean of total steps taken per day
mean(dailyTsteps, na.rm=T)

# Calculate the median of total steps taken per day
median(dailyTsteps, na.rm=T)

# Plot the mean interval activity pattern for all dates
intervalMsteps <- tapply(activityDT$steps, activityDT$interval, mean, na.rm=T)

plot(intervalMsteps, type="l", main=("Daily mean of steps in each interval"), 
     ylab="Steps in Interval", xlab="Interval")

# Name the interval with the largest average number of steps and the average number of steps in that interval 
which.max(intervalMsteps)

# Count the total number of missing values
sum(is.na(activityDT$steps))

## Impute the missing values in the dataset using the mean interval value

    # Create a vector for the mean of each interval
    intervalMsteps_vector <- as.vector(intervalMsteps)
    
    # Loop for all 61 days
    intervalMsteps_vector <- rep(intervalMsteps_vector, 61)
    
    # Replace non-missing values in vector with 0
    intervalMsteps_vector[!is.na(activityDT$steps)] = 0
    
    # Create a vector for the steps of each interval
    allStepsVector <- as.vector(activityDT$steps)
    
    # Replace missing values in vector with 0
    allStepsVector[is.na(allStepsVector)] = 0
    
    # Add the two vectors together at the row level and place back into data table
    activityDT_imputed <- activityDT
    activityDT_imputed$steps <- intervalMsteps_vector + allStepsVector

# Calculate total number of steps taken per day on imputed data set
dailyTsteps_imputed <- tapply(activityDT_imputed$steps, activityDT_imputed$date, sum)

# Create Histogram of total number of steps taken per day on imputed data set
hist(dailyTsteps_imputed, main="Frequency of total daily steps", 
     xlab="Daily steps", ylab="Number of Days", col="blue")

# Calculate the mean of total steps taken per day
mean(dailyTsteps_imputed, na.rm=T)

# Calculate the median of total steps taken per day
median(dailyTsteps_imputed, na.rm=T)

#Are there differences in activity patterns between weekdays and weekends?
dayType <- factor(weekdays(activityDT_imputed$date) %in% c("Saturday","Sunday"), 
               labels=c("weekday","weekend"), ordered=FALSE)

activityDT_imputed[, c("dayType") := dayType]

# Plot the mean interval activity pattern for all dates
tapply(X = warpbreaks$breaks,  INDEX = list(warpbreaks$wool, warpbreaks$tension), mean)

#weekdayTypeIntervalMsteps <- tapply(X=activityDT_imputed$steps, 
 #                                   INDEX=list(activityDT_imputed$interval, activityDT_imputed$dayType), 
  #                                  mean, na.rm=T)

weekdayTypeIntervalMsteps <- aggregate(steps ~ interval + dayType, activityDT_imputed, mean)

xyplot(steps ~ interval | factor(dayType), data=weekdayTypeIntervalMsteps, aspect = 1/2, 
       type = "l")

#par(mfrow = c(2, 1))
#with(activityDT_imputed, {
#    par(mai = c(0, 1, 1, 0))
#    plot(weekdayTypeIntervalMsteps[, "weekend"], type = "l", xaxt = "n",
#         main = ("Weekend"))
#    title = ("Comparison of mean interval steps on weekends and weekdays")
#    par(mai = c(1, 1, 0, 0))
#    plot(weekdayTypeIntervalMsteps[, "weekday"], type = "l",
#         main=("Weekday"),
#         xlab="Interval", ylab = "Number of Steps")
#})
