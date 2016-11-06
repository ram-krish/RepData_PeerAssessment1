# PA1_template
RK  
November 6, 2016  
##Loading Data and Initial Processing  

```r
df<-read.csv("activity.csv",header=TRUE, stringsAsFactors =FALSE )
df$date<-as.Date(df$date)
```

##Steps Taken In a Day  

```r
aggrByDate<-aggregate(x=df$steps,by=list(df$date),FUN=sum,na.rm=TRUE)
names(aggrByDate)<-c("Date","Steps")
hist(aggrByDate$Steps,main="Histogram of Steps in a Day",xlab="Steps")
```

![](PA1_template_files/figure-html/TotalStepsInaDay.png)<!-- -->

```r
MeanStepsPerDay<-mean(aggrByDate$Steps,na.rm=TRUE)
MeanStepsPerDay
```

```
## [1] 9354.23
```

```r
MedianStepsPerDay<-median(aggrByDate$Steps,na.rm=TRUE)
MedianStepsPerDay
```

```
## [1] 10395
```
##Average Daily Activity Pattern  

```r
aggrByInterval<-aggregate(x=df$steps,by=list(df$interval),FUN=mean,na.rm=TRUE)
names(aggrByInterval)<-c("interval","steps")
plot(aggrByInterval$interval,aggrByInterval$steps,xlab="Interval",ylab="Steps",
     main="Interval Vs Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxStepsByInterval<-max(aggrByInterval$steps,na.rm=TRUE)
IntervalWithMaxSteps<-subset(aggrByInterval$interval,aggrByInterval$steps==maxStepsByInterval)
IntervalWithMaxSteps
```

```
## [1] 835
```
##Replacing NA with Mean   

```r
totalNA<-sum(is.na(df$steps))
totalNA
```

```
## [1] 2304
```

```r
newdf<-df
for(i in 1:nrow(newdf))
  {
  if(is.na(newdf[i,1]==TRUE))
    {
    temp<-subset(aggrByInterval$steps,aggrByInterval$interval==newdf[i,3])
    newdf[i,1]<-temp
  }
}
aggrByDateNA<-aggregate(x=newdf$steps,by=list(newdf$date),FUN=sum)
names(aggrByDateNA)<-c("Date","Steps")
hist(aggrByDateNA$Steps,main="Histogram of total steps per Day NA Imputed",xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
MeanStepsPerDayNA<-mean(aggrByDateNA$Steps)
MeanStepsPerDayNA
```

```
## [1] 10766.19
```

```r
MedianStepsPerDayNA<-median(aggrByDateNA$Steps)
MedianStepsPerDayNA
```

```
## [1] 10766.19
```
##Pattern in Weekdays  

```r
library(lattice)
dfWithWeekday<-df
dfWithWeekday$date<-as.Date(dfWithWeekday$date)
for(i in 1:nrow(dfWithWeekday))
{
  if(weekdays(dfWithWeekday[i,2],abbreviate = TRUE)=="Sat")
  {
    dfWithWeekday[i,4]<-"Weekend"
  }
  else if(weekdays(dfWithWeekday[i,2],abbreviate = TRUE)=="Sun")
  {
    dfWithWeekday[i,4]<-"Weekend"
  }
  else{
    dfWithWeekday[i,4]<-"Weekday"
  }
}
names(dfWithWeekday)<-c("Steps","Date","Interval","Day")
dfWithWeekday$Day<-as.factor(dfWithWeekday$Day)
aggrByIntervalDay<-aggregate(x=dfWithWeekday$Steps,
                             by=list(dfWithWeekday$Interval,dfWithWeekday$Day),
                             FUN=mean,na.rm=TRUE)
names(aggrByIntervalDay)<-c("Interval","Day","Steps")
xyplot(Steps~Interval|Day, data=aggrByIntervalDay,type="l",layout=c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
