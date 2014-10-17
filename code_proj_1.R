library(dplyr)

# Load data

#   Extract data from zip archive
unzip("activity.zip")

#   Read data file
activ <- read.csv("activity.csv")

#   Describe data

str(activ)

summary(active)

miss <- vector()
for (i in 1:ncol(activ)){
    miss <- rbind(miss,paste(colnames(activ)[i], ": ", sum(is.na(activ[,i])), 
                             " missing values",sep=""))
}
as.data.frame(miss)

hpc$Time <- strptime(paste(hpc$Date, hpc$Time), format="%d/%m/%Y %H:%M:%S")
hpc$Date <- as.Date(activ$date, format="%Y-%m-%d")
table(as.Date(activ$date, format="%Y-%m-%d"))

# Total daily steps 

nlevels(activ$date)
dsum <- group_by(activ,date) %>%
    summarise(total_steps = sum(steps))

hist(dsum$total_steps, breaks=10, xlab= "Daily Total Steps", main="Raw distribution of total steps across 61 days \n from 10-1-2012 - 12-2-2012")


mean(dsum$total_steps, na.rm=TRUE)
median(dsum$total_steps, na.rm=TRUE)

sprintf("%1.0f", mean(dsum$total_steps, na.rm=TRUE))


# mean steps by interval

table(activ$interval)
nlevels(as.factor(activ$interval))


dave <- group_by(activ,interval) %>%
    summarise(mean_steps = mean(steps, na.rm=TRUE))


plot(mean_steps ~ interval, data = dave, type="l", 
     ylab="Daily mean number of steps",
     xlab="Beginning time of 5-minute interval (minutes from midnight)",
     main="Daily mean number of steps by five minute data collection interval")

dave[dave$mean_steps == max(dave$mean_steps),]


#   Missing values

sum(complete.cases(activ))

activimp <- merge(activ,dave, by="interval")
activimp$steps <- ifelse(is.na(activimp$steps),activimp$mean_steps,activimp$steps)
activimp <- activimp[order(activimp$date,activimp$interval),]
activimp <- activimp[,c("steps","date","interval","mean_steps")]

activimp$day <- weekdays(as.Date(activimp$date))
activimp$day_type <- as.factor(ifelse(activimp$day %in% c("Saturday","Sunday"),"Weekend","Weekday"))

table(activimp$day_type,activimp$day)

# plot average steps by weekday versus weekend

daveimp <- group_by(activimp,interval, day_type) %>%
    summarise(mean_steps = mean(steps, na.rm=TRUE))


par(mfrow = c(2,1))

plot(mean_steps ~ interval, data = daveimp[daveimp$day_type == "Weekday",], type="l",
     ylim=c(0,225),
     ylab="Daily mean number of steps",
     xlab="Beginning time of 5-minute interval (minutes from midnight)",
     main="Weekday - Daily mean number of steps by 5-minute \n data collection interval")
plot(mean_steps ~ interval, data = daveimp[daveimp$day_type == "Weekend",], type="l", 
     ylim=c(0,225),
     ylab="Daily mean number of steps",
     xlab="Beginning time of 5-minute interval (minutes from midnight)",
     main="Weekend - Daily mean number of steps by 5-minute \n data collection interval")

library(ggplot2)

qplot(x=interval, y=mean_steps, data=daveimp, facets= .~day_type, ylim=c(0,225), 
      geom=c("line"),
      main="Weekend versus Weekday - \n Daily mean number of steps by 5-minute data collection interval",
      ylab="Daily mean number of steps",
      xlab="Beginning time of 5-minute interval (minutes from midnight)")



library(lattice)

dotplot(mean_steps ~ interval, groups - day_type, data=daveimp, type="o")


