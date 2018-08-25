---
This is Project 1-Reproducible Research
---



```{r activity}
library(plyr)
library(ggplot2)
activity <- read.csv("activity.csv")
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
clean <- activity[!is.na(activity$steps),]
sumTable <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(sumTable)<- c("Date", "Steps")
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
```


Mean total number of steps taken per day
```{r meansteps}
as.integer(mean(sumTable$Steps))
```
Median total number of steps taken per day

```{r mediansteps}
as.integer(median(sumTable$Steps))
```

Average daily activity pattern

``` {r dailyactivitypattern}
library(plyr)
library(ggplot2)
clean <- activity[!is.na(activity$steps),]
intervalTable <- ddply(clean, .(interval), summarize, Avg = mean(steps))
pattern <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
pattern + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` {r, interval}
maxSteps <- max(intervalTable$Avg)
intervalTable[intervalTable$Avg==maxSteps,1]
```
Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r, missing}
nrow(activity[is.na(activity$steps),])
```
Substitute  missing steps with the average 5-minute interval based on the day of the week.

``` {r, average}
avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))


missingdata<- activity[is.na(activity$steps),]
newdata<-merge(missingdata, avgTable, by=c("interval", "day"))

newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")
mergeData <- rbind(clean, newdata2)
sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(sumTable2)<- c("Date", "Steps")
as.integer(mean(sumTable2$Steps))
as.integer(median(sumTable2$Steps))
hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )
```

Are there differences in activity patterns between weekdays and weekends?

``` {r, actpatterns}
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
library(lattice) 
intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))
xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```
