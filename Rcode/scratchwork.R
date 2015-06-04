#Set WD to source file location
#R files are in a folder called Rcode, and the data is one level above that

#read data
data=read.csv("../activity.csv")
class(data[[1]])
class(data[[2]])
class(data[[3]])

summary(data[[3]])

weird=data[data[[3]]==2355,]
head(weird)
dim(weird)
288*2
#interval is a time, so 2355 is 11:55 PM

hist(data[[3]])
head(data)
summary(data)
class(data[,2])

?as.date

summary(data$steps)
summary(data$steps>-1)

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

#######Barplot
barplot(stepsByDay[[2]])
abline(
        h=c(mean(stepsByDay[[2]]),median(stepsByDay[[2]])),
        col=c("red","blue")
)

#There are some days where no data was recorded
length(unique(stepsByDay[[1]]))
length(unique(steppingDays[[2]]))
length(unique(data[[2]]))

#Looking at daily patterns
dayPatterns=aggregate(steppingDays[1],by=list(int=steppingDays$interval),mean)
head(dayPatterns)
class(dayPatterns[[1]])
class(dayPatterns[[2]])
plot(dayPatterns,type="l")
summary(dayPatterns[[1]])
24*60/5
tail(dayPatterns)

#get time with max steps
dayPatterns[dayPatterns$steps==max(dayPatterns$steps),]


###### na analysis
sum(is.na(data[[1]]))
sum(c(T,T))
length(is.na(data[[1]]))

#check days of the week
?as.Date
dates=as.Date(data[[2]])
head(dates)
head(data[[2]])
class(dates)
dataD=data
data[[2]]=dates
dayWk=weekdays(dates)
head(dayWk)
dataD[[4]]=as.factor(dayWk)
naData=dataD[is.na(dataD[[1]]),]
summary(naData)

#two friday and mondays are missing entirely
#one thursday sunday and wednesday
dataDrmna=dataD[!is.na(dataD),]
dataByDays=aggregate(dataDrmna$steps,by=list(day=dataDrmna$V4,dataDrmna$interval),mean)
summary(dataByDays)
head(dataByDays)
class(dataD[[1]])
newData=dataD
class(newData[[1]])
head(newData)

head(newData$steps)
class(newData$steps)
class(newData$interval)

#newData$steps[1]=dataByDays[dataByDays$day=="Monday" & dataByDays$Group.2=="0",3]
head(newData)

for (i in 1:length(newData[[1]])){
        if (is.na(newData$steps[i])){
                newData$steps[i]=
                        dataByDays[dataByDays$day==newData$V4[i] 
                                   & dataByDays$Group.2==newData$interval[i],3]
        }
}

head(newData)
head(dataByDays)
dataByDays[1:10,]

stepsByDayNEW=aggregate(newData$steps,by=list(Date=newData$date),sum)
head(stepsByDayNEW)
hist(stepsByDayNEW[[2]])
mean(stepsByDay[[2]])
median(stepsByDay[[2]])
mean(stepsByDayNEW[[2]])
median(stepsByDayNEW[[2]])
abline(
        v=c(mean(stepsByDay[[2]]),median(stepsByDay[[2]])),
        col=c("red","blue")
)
abline(
        v=c(mean(stepsByDayNEW[[2]]),median(stepsByDayNEW[[2]])),
        col=c("red","blue")
)
#########
head(newData)
#Weekdays vs weekends
weekdays=c("Monday","Tuesday","Wednesday","Thursday","Friday")
weekend=c("Saturday","Sunday")
#dayType=factor(c("weekday","weekend"),c(0,1))
#class(newData[,4])
newData[,4]=as.character(newData[,4])
newData[,5]=character()
k=0
x=character()
for (i in 1:length(newData[[1]])){
        if (newData$V4[i] %in% weekdays){
                newData[i,5]="weekday"
        } else if (newData$V4[i] %in% weekend) {
                newData[i,5]="weekend"
        } else {
                x[k]=newData[i,4]
                k=k+1
        }
}
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
