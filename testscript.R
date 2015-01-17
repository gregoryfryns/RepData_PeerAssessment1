if (!file.exists("activity.csv")) {
    unzip("activity.zip")
}

data <- read.csv("activity.csv"
                 , header=TRUE
                 , sep=","
                 , na.strings="NA"
                 , colClasses=c("numeric","character","numeric"))
data$date <- as.Date(data$date)

stepsbyday <- aggregate(data$steps ~ data$date, data, FUN="sum", na.rm=TRUE)
colnames(stepsbyday) <- c("date", "steps")

plot(stepsbyday$date
     , stepsbyday$steps
     , type="h"
     , col="blue"
     , xlab="Date"
     , ylab="Number of steps")
mean(stepsbyday$steps)
median(stepsbyday$steps)

sum(is.na(data$steps))

