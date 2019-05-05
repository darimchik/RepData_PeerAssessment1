---
title: "Reproducible research. Project1"
author: "Darima Butitova"
date: "5/2/2019"
output:
  html_document: default
  pdf_document: default
---

# Loading and preprocessing the data
Setting the working directory to access the data and loading activity.cvs. Looking at the structure of the dataset and summary of variables.


```{r, echo=TRUE}
data <- read.csv("activity.csv", header=TRUE)
summary(data)
str(data)
```

# What is mean total number of steps taken per day?

```{r, echo=TRUE}
totalsteps <- aggregate(data$steps, list(date=data$date), sum)
par(mfrow=c(1,1))
hist(totalsteps$x, breaks=20, main= "Mean total steps per day")
summary(totalsteps)
```

Mean of total number of steps per day is 10766, median is 10765.


# What is the average daily activity pattern?

```{r, echo=TRUE}
datanoNA <- na.omit(data)
meanstepsbyinterval <- aggregate(datanoNA$steps, list(interval=datanoNA$interval), mean)
par(mfrow=c(1,1))
plot(meanstepsbyinterval$interval, meanstepsbyinterval$x, type="l", main="Average daily activity pattern", xlab="interval", ylab="steps")

which.max(meanstepsbyinterval$x)
meanstepsbyinterval[104,]
```

Interval #104 which corresponds to 8.35am contains maximum number of steps - 206. Looks like the individual is commuting to work.

# Imputing missing values

```{r, echo=TRUE}
library(mice)
md.pattern(data)
mdata<-data
mdata <-cbind(mdata, meansteps=meanstepsbyinterval$x)
mdata$steps[is.na(mdata$steps)] <- mdata$meansteps[is.na(mdata$steps)]
mtotalsteps <- aggregate(mdata$steps, list(date=mdata$date), sum)
summary(mtotalsteps)
```

There are 2304 missing values, and they are all in "steps" variable. 
Missing values are replaced with the mean for the missed interval.


```{r}
par(mfrow=c(1,1))

hist(mtotalsteps$x, breaks=20, main="Average daily activity pattern with imputed NA's")
```


The histogram looks very similar to the one without NA's imputed. The mean is the same - 10766, the median is almost the same - 10766, which is very close to 10765. The min and max values of the distribution are the same. The 1st and 3rd quantiles are slightly shifted to the right, but since the measures of central tendency (mean and median) are the same, it's not a big issue. Thus, there is no impact of imputing missing values on the estimates.


# Are there differences in activity patterns between weekdays and weekends?

```{r}
library(dplyr)
dfactor <- as.factor(weekdays(as.Date(data$date)))
dd <- recode(dfactor, Saturday=1, Sunday=1, .default=0)
mdata <- cbind(mdata, dd)

mdataweekend <- mdata[dd==1,]
meanstepsbyintwend <- aggregate(mdataweekend$steps, list(interval=mdataweekend$interval), mean)

mdataweekday <- mdata[dd==0,]
meanstepsbyintwday <- aggregate(mdataweekday$steps, list(interval=mdataweekday$interval), mean)
```

```{r, fig.height=6, fig.width=6}
par(mfrow=c(2,1))

plot(meanstepsbyintwend$interval, meanstepsbyintwend$x, type="l", main="Average steps on weekends", xlab="interval", ylab="steps", ylim=c(0, 230))
plot(meanstepsbyintwday$interval, meanstepsbyintwday$x, type="l", main="Average steps on weekdays", xlab="interval", ylab="steps", ylim=c(0, 230))
```

The pattern of activity differs on weekdays and weekends. The individual is relatively active in the morning on weekdays, most likely commuting to work, and less active the rest of the day. On weekends the individual is quite active throughout the day.

