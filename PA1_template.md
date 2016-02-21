# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

+ load the data

```r
library(ggplot2)
Data<-read.csv("activity.csv")
```

+ trasform the data

```r
Data.proc<- Data
Data.proc$date<- as.Date(as.character(Data.proc$date))
Data.proc<- Data.proc[!is.na(Data.proc$steps),]
```


## What is mean total number of steps taken per day?

+ make a histogram of the total number of steps taken each days

```r
sel<- unique(Data.proc$date)
Totalstep<-function(elemento){
  return(sum(Data.proc$steps[Data.proc$date==elemento]))
}
res1<-sapply(sel,Totalstep)
pp<- ggplot(data = data.frame(totalstep=res1),aes(x =  totalstep))+geom_histogram(colour=3)
print(pp)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

+ calculate and report the mean and median total number of steps taken per day

```r
res2<- data.frame(Mean=mean(res1),Median=median(res1))
res2
```

```
##       Mean Median
## 1 10766.19  10765
```



## What is the average daily activity pattern?

+ Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
sel2<- unique(Data.proc$interval)
Meanstep<-function(elemento){
  data<- Data.proc$steps[Data.proc$interval==elemento]
  result<- mean(data)
  return(result) 
}
res2<-sapply(sel2,Meanstep)
pp<- ggplot(data = data.frame(interval=sel2,meanstep=res2),aes(x=interval,y=meanstep))+geom_line()
print(pp)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

+ Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
sel2[which.max(res2)]
```

```
## [1] 835
```

## Imputing missing values


```r
Data.proc<- Data
Data.proc$date<- as.Date(as.character(Data.proc$date))
```


+ Calculate and report the total number of missing values in the dataset

```r
data.frame(steps=sum(is.na(Data.proc$steps)),date=sum(is.na(Data.proc$date)),interval=sum(is.na(Data.proc$interval)))
```

```
##   steps date interval
## 1  2304    0        0
```

+ Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.

```r
ii<- 1
NAcorr<- function(elem) {
  if (is.na(elem)) {
    res<-(mean(Data.proc$steps[Data.proc$interval==Data.proc$interval[ii]],na.rm = TRUE))
  }else{ res<-(elem)}
  ii<<-ii+1
  return(res)
}
Data.fill<- Data.proc
Data.fill$steps<- sapply(Data.fill$steps,NAcorr)
```

+ Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

```r
sel<- unique(Data.fill$date)
Totalstep2<-function(elemento){
  return(sum(Data.fill$steps[Data.fill$date==elemento]))
}
res1<-sapply(sel,Totalstep)
pp<- ggplot(data = data.frame(totalstep=res1),aes(x =  totalstep))+geom_histogram(colour=3)
print(pp)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
res2<- data.frame(Mean=mean(res1),Median=median(res1))
res2
```

```
##   Mean Median
## 1   NA     NA
```


## Are there differences in activity patterns between weekdays and weekends?

+ Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days 

```r
Data.fill$Day<-weekdays(Data.fill$date,abbreviate = TRUE)
sel3<- unique(Data.fill$Day)[c(7,1:6)]
Data.fill$week<-0
ii<- 1
Week<- function(elem) {
  if (sum(elem==sel3[1:5])) {
    res<-"weekday"
  }else{ res<-"weekend"}
  ii<<-ii+1
  return(res)
}
Data.fill$week<- sapply(Data.fill$Day,Week)


sel2<- unique(Data.fill$interval)
sel4<- unique(Data.fill$week)
Meanstep2<-function(elemento){
  data1<- Data.fill$steps[Data.proc$interval==elemento & Data.fill$week==sel4[1]]
  data2<- Data.fill$steps[Data.proc$interval==elemento & Data.fill$week==sel4[2]]
  result<- c(mean(data1),mean(data2))
  names(result)<- sel4
    return(result) 
}
res4<-t(sapply(sel2,Meanstep2))

temp<- rbind(
  data.frame(intervals=sel2,meanstep=res4[,1],week=sel4[1]),
  data.frame(intervals=sel2,meanstep=res4[,2],week=sel4[2])
)

pp<- ggplot(data = temp,aes(x=intervals,y=meanstep))+geom_line()+facet_grid(week~.)
print(pp)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
