actdata_original <- read.csv("activity.csv")


library(data.table)
act_DT <- data.table(actdata_original)

sumact_DT <- act_DT[, lapply(.SD, sum(na.rm=TRUE)), by = date, .SDcols = c('steps')]

hist(sumact_DT$steps,main="Self-activity monitoring data",xlab="Total number of steps per day",ylab="Number of days",breaks=10,col="darkred")

actdata <- actdata_original
actdata$daynumber <- as.numeric(actdata_original$date)
daylist <- unique(actdata$daynumber)
intlist <- unique(actdata$interval)

intmeans <- sapply(intlist,function(int) 
    mean(actdata$steps[actdata$interval==int],na.rm=TRUE))

plot(intlist,intmeans,type="l",xlab="Interval of day",ylab="Mean number of steps in interval",lwd=2,col="blue",main="Self-activity monitoring data")

maxinterval <- intlist[intmeans==max(intmeans)]
maxintervalposition <- match(max(intmeans),intmeans)

sum(is.na(actdata$steps))
NAsperday <- sapply(daylist,function(day) sum(is.na(actdata$steps[actdata$daynumber==day])))

actdata_filled <- actdata_original

for (thisday in daylist){
    if (NAsperday[thisday]!=0){
        actdata_filled$steps[actdata$daynumber==thisday] <- intmeans
    }
}

act_DT <- data.table(actdata_filled)
sumactfilled_DT <- act_DT[, lapply(.SD, sum(na.rm=TRUE)), by = date, 
                          .SDcols = c('steps')]

hist(sumactfilled_DT$steps,main="Self-activity monitoring data, NAs filled",
     xlab="Total number of steps per day",ylab="Number of days",breaks=10,
     col="darkred")


meanstepsperday_filled <- mean(sumactfilled_DT$steps,na.rm=TRUE)
medianstepsperday_filled <- median(sumactfilled_DT$steps,na.rm=TRUE)

percentmeandiff <- 100*abs(meanstepsperday - 
                               meanstepsperday_filled)/meanstepsperday
percentmediandiff <- 100*abs(medianstepsperday - 
                                 medianstepsperday_filled)/medianstepsperday

actdata_filled$date <- as.Date(actdata_filled$date)
actdata_filled$DayofWeek <- weekdays(actdata_filled$date)
actdata_filled$daynumber <- actdata$daynumber
dayclass <- vector(mode="character",length=length(actdata$daynumber))
dayclass <- rep.int("weekday",length(actdata$daynumber))
dayclass[actdata_filled$DayofWeek=="Saturday"] <- "weekend"
dayclass[actdata_filled$DayofWeek=="Sunday"] <- "weekend"
actdata_filled$dayclass <- as.factor(dayclass)

#subset the data frame

actdata_weekdays <- subset(actdata_filled,dayclass=="weekday")
actdata_weekends <- subset(actdata_filled,dayclass=="weekend")

intmeans_weekdays <- sapply(intlist,function(int) 
    mean(actdata_weekdays$steps[actdata$interval==int],na.rm=TRUE))
intmeans_weekends <- sapply(intlist,function(int) 
    mean(actdata_weekends$steps[actdata$interval==int],na.rm=TRUE))

par(mfrow=c(2,1))

plot(intlist,intmeans_weekends,type="l",col="red4",lwd=2,
     main="Weekend",ylab="No. of steps",
     xlab="Interval of day")
plot(intlist,intmeans_weekdays,type="l",col="blue4",lwd=2,
     main="Weekday",ylab="No. of steps",
     xlab="Interval of day")






