Course Project 1
======

1. Code for reading in the dataset and/or processing the data

```r
##setwd("./data")
activity<-read.csv("activity.csv")
totalsteps<-tapply(activity$steps,activity$date,sum,na.rm=TRUE)
```

2. Histogram of the total number of steps taken each day

```r
total_data<-data.frame(date=names(totalsteps),steps=totalsteps)
rownames(total_data)<-NULL
library(lattice)
```
3. Mean and median number of steps taken each day

```r
mean_eachday<-mean(total_data$steps)
median_eachday<-median(total_data$steps)
print(paste("Mean Per Day",as.character(round(mean_eachday)),sep=" "))
```

```
## [1] "Mean Per Day 9354"
```

```r
print(paste("Median Per Day",as.character(round(median_eachday)),sep=" "))
```

```
## [1] "Median Per Day 10395"
```

4.Time series plot of the average number of steps taken

```r
intersteps<-tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
plot(intersteps,type="l",xlab="5-minute interval",ylab="averaged across all days")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
5.The 5-minute interval that, on average, contains the maximum number of steps

```r
maxinter<-as.numeric(names(intersteps[intersteps==max(intersteps)]))
print(paste(as.character(maxinter),"to",as.character(maxinter+5), "is the 5-minute interval that, on average, contains the max number of steps",sep=" "))
```

```
## [1] "835 to 840 is the 5-minute interval that, on average, contains the max number of steps"
```

6. Code to describe and show a strategy for imputing missing data

```r
newactivity<-activity
numMiss=sum(is.na(activity$steps))
print(paste("Number of missing intervals is ",as.character(numMiss),sep=" "))
```

```
## [1] "Number of missing intervals is  2304"
```

```r
newactivity[is.na(activity$steps),1]<-as.numeric(intersteps[as.numeric(newactivity[is.na(newactivity$steps),3]+5)/5])

newtotalsteps<-tapply(newactivity$steps,newactivity$date,sum,na.rm=TRUE)
newtotal_data<-data.frame(date=names(newtotalsteps),steps=newtotalsteps)
rownames(newtotal_data)<-NULL
newmean_eachday<-mean(total_data$steps)
newmedian_eachday<-median(total_data$steps)

print(paste("Mean Per Day with no missing values",as.character(round(newmean_eachday)),sep=" "))
```

```
## [1] "Mean Per Day with no missing values 9354"
```

```r
print(paste("Median Per Day with no missing values",as.character(round(newmedian_eachday)),sep=" "))
```

```
## [1] "Median Per Day with no missing values 10395"
```

```r
histogram(newtotal_data$steps,main="new total number of steps without missing value taken each day",type="count")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
print("as we can see that the impact is very small!")
```

```
## [1] "as we can see that the impact is very small!"
```

7.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
temp<-as.character(activity$date)
temp2<-as.Date(temp)
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.3.1
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
activity2<-activity
activity2$day[wday(temp2,label=TRUE)=="Sat"|wday(temp2,label=TRUE)=="Sun"]<-"weekend"
activity2$day[wday(temp2,label=TRUE)!="Sat"&wday(temp2,label=TRUE)!="Sun"]<-"weekday"


dataweekday<-activity2[activity2$day=="weekday",]
dataweekend<-activity2[activity2$day=="weekend",]
par(mfrow=c(1,2))
intersteps1<-tapply(dataweekday$steps,dataweekday$interval,mean,na.rm=TRUE)
plot(intersteps1,type="l",xlab="5-minute interval",ylab="averaged across all weekdays")

intersteps2<-tapply(dataweekend$steps,dataweekend$interval,mean,na.rm=TRUE)
plot(intersteps2,type="l",xlab="5-minute interval",ylab="averaged across all weekend")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)




