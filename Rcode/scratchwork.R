#Set WD to source file location
#R files are in a folder called Rcode, and the data is one level above that

#read data
rawdata=read.csv("../activity.csv")
class(rawdata[[1]])
class(rawdata[[2]])
class(rawdata[[3]])

summary(rawdata[[3]])



########Figuring out the interval labels
weird=rawdata[rawdata[[3]]==2355,]
head(weird)
dim(weird)
288*2
#interval is a start time, so 2355 is 11:55 PM

rawdata[[2]]=as.Date(rawdata[[2]])
dayOfWk=as.factor(weekdays(rawdata[[2]]))
data=data.frame(rawdata,dayOfWk)
weekdays=c("Monday","Tuesday","Wednesday","Thursday","Friday")
weekend=c("Saturday","Sunday")
k=1
x=c("All days were classified as weekday or weekend.")
check=x
for (i in 1:length(data[[1]])){
        if (data$dayOfWk[i] %in% weekdays){
                data[i,5]="weekday"
        } else if (data$dayOfWk[i] %in% weekend) {
                data[i,5]="weekend"
        } else {
                check=x
                x[k]=data[i,4]
                k=k+1
                if (check!=x){
                        check=c("There were days of the week that did 
                                not get identified as weekend or weekday. 
                                 Check the variable x.")
                }
        }
}
data[[5]]=as.factor(data[[5]])
names(data)[5]="dayType"
head(data)

############Look at the days with no NA's for mean, median, hist
steppingDays=data[!is.na(data$steps),]
head(steppingDays)
stepsByDay=aggregate(steppingDays$steps,by=list(Date=steppingDays$date),sum)
head(stepsByDay)
mean(stepsByDay[[2]])
median(stepsByDay[[2]])

#########Hist
par(mfrow=c(1, 1), mar=c(5, 4, 2, .8))
hist(stepsByDay[[2]],
     main="Total Steps in a Day",
     xlab="Steps per Day",
     ylab="Number of Days",
     )
legend("topright",
       legend=c("Mean","Median"),
       fill=c("red","blue"))
abline(
        v=c(mean(stepsByDay[[2]]),median(stepsByDay[[2]])),
        col=c("red","blue")
)

################Barplot
#check that the dates are chronological
!is.unsorted(stepsByDay[[1]])
barplot(stepsByDay[[2]],
        main="Total Steps in Each Day",
        xlab="Days Into Study (NA's skipped)",
        ylab="Number of Steps",
)
legend("topleft",
       horiz=TRUE, bty='n', cex=0.8,
       legend=c("Mean","Median"),
       fill=c("red","blue"))
abline(
        h=c(mean(stepsByDay[[2]]),median(stepsByDay[[2]])),
        col=c("red","blue")
)


#There are some days where no data was recorded
length(unique(stepsByDay[[1]]))
length(unique(steppingDays[[2]]))
length(unique(data[[2]]))

#Looking at daily patterns
dayPatterns=aggregate(steppingDays[1],by=list(interval=steppingDays$interval),mean)
head(dayPatterns)
class(dayPatterns[[1]])
class(dayPatterns[[2]])
plot(dayPatterns,type="l")


#get time with max steps
#install.packages("Hmisc")
library(Hmisc)
dayPatterns[dayPatterns$steps==max(dayPatterns$steps),]
plot(dayPatterns,
     type="l",
     xaxt="n",
     xlab="24 Hour Time",
     ylab="Average Number of Steps per 5 Minute Interval",
     main="Average Daily Activity Pattern"
     )
axis(side=1,
     at=seq(0,2300,by=600)
     )
minor.tick(nx=5, tick.ratio=.5)





###### na analysis
sum(is.na(data[[1]]))#gets number of True's
length(is.na(data[[1]]))

#check days of the week
naData=data[is.na(data[[1]]),]
summary(naData$dayOfWk)/(24*60/5)
summary(naData$dayType)/(24*60/5)


#two friday and mondays are missing entirely
#one thursday sunday and wednesday
steppingDays=data[!is.na(data),]
dataByDays=aggregate(steppingDays$steps,by=list(dayOfWk=steppingDays$dayOfWk,interval=steppingDays$interval),mean)
summary(dataByDays)

newData=data
head(newData)

for (i in 1:length(newData[[1]])){
        if (is.na(newData$steps[i])){
                newData$steps[i]=
                        dataByDays[dataByDays$dayOfWk==newData$dayOfWk[i] 
                                   & dataByDays$interval==newData$interval[i],3]
        }
}


head(newData)


stepsByDayNEW=aggregate(newData$steps,by=list(Date=newData$date),sum)
head(stepsByDayNEW)
dim(stepsByDayNEW)

##########HIStograms

par(mfrow=c(1,2),oma=c(0,0,2,0))
hist(stepsByDay[[2]],
     main="NA's Removed",
     xlab="Steps per Day",
     ylab="Number of Days",
)
hist(stepsByDayNEW[[2]],
     main="Interpolated NA Values",
     xlab="Steps per Day",
     ylab="Number of Days",
)
title(main="Total Steps in a Day", outer=T)



#########
head(newData)


head(newData)
summary(newData$V5)
summary(x)
newData[,5]=as.factor(newData[,5])
head(newData)
names(newData)[4:5]=c("dayOfWeek","dayType")

dayTypeAggr=aggregate(newData$steps,
                      by=list(dayType=newData$dayType,interval=newData$interval),
                      mean
)
head(dayTypeAggr)
par(mfrow=c(1,2))
plot(dayTypeAggr[dayTypeAggr$dayType=="weekday",2:3])
plot(dayTypeAggr[dayTypeAggr$dayType=="weekend",2:3])
