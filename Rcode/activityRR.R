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

steppingDays=data[!is.na(data$steps>-1),]
head(steppingDays)
stepsByDay=aggregate(steppingDays$steps,by=list(Date=steppingDays$date),sum)
head(stepsByDay)
hist(stepsByDay[[2]])
mean(stepsByDay[[2]])
median(stepsByDay[[2]])
abline(
  v=c(mean(stepsByDay[[2]]),median(stepsByDay[[2]])),
  col=c("red","blue")
)
#Testing both lines are showing up
abline(h=c(10,15),col=c("red","blue"))

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
sum(is.na(data))
sum(c(T,T))

#check days of the week
data[[2]]=as.date(data[[2]])
