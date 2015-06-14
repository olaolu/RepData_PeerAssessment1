# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data



```r
data=read.csv("activity.csv")
cdata=data[complete.cases(data),]
```

## What is mean total number of steps taken per day?


```r
dsum=tapply(cdata$steps, cdata$date, sum)
mean(dsum, na.rm=T)
```

```
## [1] 10766
```

```r
median(dsum, na.rm=T)
```

```
## [1] 10765
```

```r
hist(dsum,main="Histogram of Total steps in a day ", xlab=" number of steps" )
```

![plot of chunk unnamed-chunk-2](PA1_template_files/figure-html/unnamed-chunk-2.png) 
The mean is 10766  and the median  10765

## What is the average daily activity pattern?


```r
 tsum=tapply(cdata$steps, cdata$interval, sum)/288
l=ts(cdata$interval, start=0, end=287)
#dd=seq(from=5, to=5*288, by=5 )
 plot(l,(tsum), type="l", main="Time Series Plot of Total steps ", xlab="Time interval", ylab="Total steps")
```

![plot of chunk unnamed-chunk-3](PA1_template_files/figure-html/unnamed-chunk-3.png) 

```r
nn<-which.max(tsum)
l[nn]
```

```
## [1] 835
```
The interval with the highest number of average steps is 835

## Imputing missing values
The next step is to fill the missing data using the average for that a time interval across all days

```r
 sum(!complete.cases(data))
```

```
## [1] 2304
```

```r
mdata=data[!complete.cases(data),]
```

```r
newdata=data
require(sqldf)
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
```

```r
for(ii in 1:length(data$steps)){
  if (is.na(newdata$steps[ii])){
    gr=newdata$interval[ii]
    mn=paste ("select * from data where interval== ", as.character(gr)) 
    me=sqldf(mn)
    me=mean(me[,1], na.rm = T)
    newdata$steps[ii]=me
    }
  }
```

```
## Loading required package: tcltk
```

```r
 sum(!complete.cases(newdata))
```

```
## [1] 0
```

```r
dsum2=tapply(newdata$steps, newdata$date, sum)
mean(dsum2)
```

```
## [1] 10766
```

```r
median(dsum2)
```

```
## [1] 10766
```

```r
hist(dsum2, main="Histogram of Total steps in a day after inputing missing values ", xlab=" number of steps")
```

![plot of chunk unnamed-chunk-6](PA1_template_files/figure-html/unnamed-chunk-6.png) 
The mean and the median after filling the missing data are both 10766;
The number of missing data is 2304

## Are there differences in activity patterns between weekdays and weekends?

```r
require("chron")
```

```
## Loading required package: chron
```

```r
aday=dates(as.character(data$date), format=c(dates="y-m-d"))
aweek=weekdays(aday)
aweek=as.character(aweek)
 for(kk in 1:length(aweek)){
   if(aweek[kk]=="Sat"|| aweek[kk]=="Sun"){
     aweek[kk]="Weekend"
   } else { 
     aweek[kk]="Weekday"
     }
 }
newdata$date=aweek
```

The next step is to plot the average steps for weekends and weekdays per time interval

```r
require(sqldf)
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
```

```r
nwkday=sqldf("select * from newdata where date='Weekday' ")
```

```
## Loading required package: tcltk
```

```r
nwkend=sqldf("select * from newdata where date='Weekend'")

 twkday=tapply(nwkday$steps, nwkday$interval, sum)/288
 twkend=tapply(nwkend$steps, nwkend$interval, sum)/288
par(mfrow=c(2,1))

 plot(l, twkday, type="l", main="Time Series Plot of Average steps durring Weekdays  ", xlab="Time interval", ylab="Average steps for Weekdays ", ylim=c(0,50))

plot(l, twkend, type="l", main="Time Series Plot of Average steps durring weekends ", xlab="Time interval", ylab="Average steps for weekends",  ylim=c(0,50))
```

![plot of chunk unnamed-chunk-8](PA1_template_files/figure-html/unnamed-chunk-8.png) 

