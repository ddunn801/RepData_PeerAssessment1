# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
```{r echo=TRUE}
setwd("C:\\Users\\ddunn\\Dropbox\\DD Cloud\\Courses\\Coursera - Reproducible Research\\Week 1\\RepData_PeerAssessment1")
d0 <- read.csv("activity.csv",header=TRUE,stringsAsFactors=FALSE)
d1 <- d0
d1$date <- as.Date(d1$date)
```


## What is mean total number of steps taken per day?
```{r echo=TRUE}
dailySteps <- aggregate(steps~date,d1,sum)
h1 <- hist(dailySteps$steps,xlab="Daily Steps",main="Histogram of Daily Steps",col="blue")
h1
mean(aggregate(steps~date,d1,sum)$steps)
median(aggregate(steps~date,d1,sum)$steps)
```

The mean total number of steps daily is `r mean(aggregate(steps~date,d1,sum)$steps)`.
The median total number of steps daily is `r median(aggregate(steps~date,d1,sum)$steps)`.


## What is the average daily activity pattern?
```{r echo=TRUE}
intervalSteps <- aggregate(steps~interval,d1,mean)
plot(x=intervalSteps$interval,y=intervalSteps$steps,type="l",xlab="Interval",ylab="Average Steps",main="Average # of Steps by Interval")
maxInterval <- intervalSteps$interval[which(intervalSteps$steps==max(intervalSteps$steps))]
paste(round(maxInterval/5)*5," to ",round(maxInterval/5)*5+5)
abline(v=maxInterval)
```

The interval containing the maximum number of average daily steps is `r paste(round(maxInterval/5)*5," to ",round(maxInterval/5)*5+5)`.


## Imputing missing values
```{r echo=TRUE}
sum(is.na(d1$steps))

d2 <- d1
for(i in d1$interval){
d2$steps[d2$interval==i&is.na(d2$steps)] <- mean(d2$steps[d2$interval==i],na.rm=T)
}

dailySteps2 <- aggregate(steps~date,d2,sum)
h2 <- hist(dailySteps2$steps,xlab="Daily Steps",main="Histogram of Daily Steps",col="blue")
h2
mean(aggregate(steps~date,d2,sum)$steps)
median(aggregate(steps~date,d2,sum)$steps)
hDiff <- data.frame(Breaks=h1$breaks[2:length(h1$breaks)],DensityDiff=h2$density - h1$density)
```
Missing values were imputed by taking the mean number of steps for the missing interval.
Imputing missing values in this manner didn ot materially change the histogram's density.


## Are there differences in activity patterns between weekdays and weekends?
```{r echo=TRUE}
d2$date <- as.POSIXlt(d2$date)
d2$DayType <- weekdays(d2$date)
d2$DayType[d2$DayType %in% c("Sunday","Saturday")] <- "weekend"
d2$DayType[d2$DayType %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")] <- "weekday"
d2$DayType <- ordered(d2$DayType,levels=c("weekend","weekday"))

library(lattice)
library(knitr)
intervalStepsDT <- aggregate(steps~interval+DayType,d2,mean)

xyplot(steps~interval|DayType,data=intervalStepsDT,type="l",main="Interval",xlab="Interval",ylab="Number of steps",layout=c(1,2))
```

