#Set WD to source file location
#R files are in a folder called Rcode, and the data is one level above that

#read data
data=read.csv("../activity.csv")

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


