##############################################
#### Read dataset file and processing the data 
##############################################
file <- read.csv("activity.csv")
date <- file[, "date"]
datefac <- levels(file[, "date"])
steps <- file[, "steps"]
interval <- file[, "interval"]

##############################################################
#### Draw histogram of the total number of steps taken per day
##############################################################
stepsperdate <- tapply(steps, date, sum, na.rm = T, simplify = F) # Returns an array
stepsperday <- as.numeric(stepsperdate) # Convert the array into a numeric vector
stepsTable <- data.frame(datefac, stepsperday) 

png(file = "stepsPerDay.png", width = 480, height = 480) # Save as PNG file
with(stepsTable, hist(stepsperday, col = "red1", main = "Steps Taken Per Day", xlab = "Steps per day"))
dev.off()

writeLines('See "stepsPerDay.png" for the histogram of total number of steps taken per day.')

######################################################################
#### Calculate the mean and median total number of steps taken per day
######################################################################
# datainfo <- summary(stepsTable[, "stepsperday"])
# stepsperdaymean <- datainfo[4]
# stepsperdaymedian <- datainfo[3]
stepsperdaymean <- mean(stepsTable[, "stepsperday"]) # No need for na.rm = T as has been done in sum
writeLines(paste('Mean of the total number of steps taken per day:', stepsperdaymean, '.'))
stepsperdaymedian <- median(stepsTable[, "stepsperday"])
writeLines(paste('Median of the total number of steps taken per day:', stepsperdaymedian, '.'))

#############################################################################
#### Make time series plot of the 5-minute interval (x-axis) 
#### and the average number of steps taken, averaged across all days (y-axis)
#############################################################################


# Create table of steps taken for each day in columns
numofdays <- length(datefac)
daystepchunks <- (split(file[, "steps"], date)) # Split data according to dates and returns a list
intervaltable <- NULL
for (i in 1:numofdays) {
        intervaltable <- data.frame(c(intervaltable, daystepchunks[i]))
}

# Find the mean for each intervals (using values across the table for each row)
meanlist <- rowMeans(intervaltable, na.rm = T)

# Create table of daily intervals against mean values across all days
dayintervalchunks <- (split(file[, "interval"], date)) 
onedayinterval <- dayintervalchunks[[1]]
meantable <- data.frame(onedayinterval, meanlist)

# Plot of daily interval against average steps taken across all days
png(file = "averageSteps.png", width = 480, height = 480) # Save as PNG file
with(meantable, plot(onedayinterval, meanlist, main = "Average Number of Steps Taken for each 5-minute Interval", type = "l", xlab = "5-minute intervals (minutes)", ylab = "Average Number of Steps"))
dev.off()

writeLines('See "averageSteps.png" for the plot of the number of steps taken for each 5-minute interval averaged across all days.')

# Find the 5-minute interval with maximum number of steps
maxnumofsteps <- max(meanlist)
intervalofmax <- subset(meantable, meanlist == maxnumofsteps, select = onedayinterval)
writeLines(paste("Maximum daily average number of steps taken occurs between the 5-minute interval", intervalofmax[[1]], "and", intervalofmax[[1]] + 5, "mins."))

#############################
#### Imputting Missing Values
#############################

# Calculate and report total number of NAs in dataset
nasteps <- steps[is.na(steps)]
writeLines(paste("Total number of missing values (NAs):", length(nasteps), "."))

# Create a new dataset with NAs filled in with mean value for the corresponding 5-minute interval
numofintervals <- length(onedayinterval) # Number of 5-min intervals per day
newdaystepchunks <- daystepchunks # New list of daily steps values where NAs will be filled
filledsteps <- NULL # New vector of steps data
for (i in 1:numofdays) {
        for (j in 1:numofintervals) {
                newdaystepchunks[[i]][[j]][is.na(newdaystepchunks[[i]][[j]])] <- meanlist[j]
                filledsteps <- c(filledsteps, newdaystepchunks[[i]][[j]]) 
        }
}
newdataset <- data.frame(filledsteps, date, interval)
dput(newdataset, file = "activitymonitorNAsFilled.R")
writeLines('"activitymonitorNAsFilled.R" is the new dataset with NAs filled in using the average steps taken per 5-minute interval.')

# Draw histogram of the total number of steps taken per day (NAs filled with average)
newfile <- dget("activitymonitorNAsFilled.R")
newdate <- newfile[, "date"]
newsteps <- newfile[, "filledsteps"]
newstepsperdate <- tapply(newsteps, newdate, sum, na.rm = T, simplify = F) # Returns an array
newdatefac <- levels(newfile[, "date"])
newstepsperday <- as.numeric(newstepsperdate) # Convert the array into a numeric vector
newstepsTable <- data.frame(newdatefac, newstepsperday) 

png(file = "newStepsPerDay.png", width = 480, height = 480) # Save as PNG file
with(newstepsTable, hist(newstepsperday, col = "red1", main = "Steps Taken Per Day (NAs filled with average)", xlab = "Steps per day"))
dev.off()

writeLines('See "newStepsPerDay.png" for the histogram of total number of steps taken per day (NAs filled with average).')

# Calculate the mean and median total number of steps taken per day (NAs filled with average)
newstepsperdaymean <- mean(newstepsTable[, "newstepsperday"]) # No need for na.rm = T as has been done in sum
writeLines(paste('Mean of the total number of steps taken per day (NAs filled with average):', newstepsperdaymean, '.'))
newstepsperdaymedian <- median(newstepsTable[, "newstepsperday"])
writeLines(paste('Median of the total number of steps taken per day (NAs filled with average):', newstepsperdaymedian, '.'))
writeLines('New Mean and Median values differ from the previous estimates. Both the Mean and Median is now of higher values.')
writeLines('Also, the new Mean and Median values are now equal.')

####################################################
#### Activity patterns between weekdays and weekends
####################################################

#### Create new data set with weekday-type column
formatnewdate <- as.Date(newdate) # Format to Date class
dayofweek <- weekdays(formatnewdate)
WeekdayOrWeekend <- function(weekday) {
        if((weekday != "Saturday") && (weekday != "Sunday")) {"Weekday"} else {"Weekend"}
}
WeekdayType <- sapply(dayofweek, WeekdayOrWeekend)
fileWithWeekdayType <- data.frame(newfile, WeekdayType)

#### Create table of steps taken for each WEEKDAY in columns
file.weekday <- subset(fileWithWeekdayType, WeekdayType == "Weekday", select = c(date, filledsteps))
weekdaysdates <- levels(factor(file.weekday[, "date"])) # levels(file.weekday[, "date"]) will give the original dates before subsetting
numofdays.weekday <- length(weekdaysdates)
weekdaystepchunks <- (split(file.weekday[, "filledsteps"], weekdaysdates)) # Returns a list
intervaltable.weekday <- NULL
for (i in 1:numofdays.weekday) {
        intervaltable.weekday <- data.frame(c(intervaltable.weekday, weekdaystepchunks[i]))
}
# Find the mean for each intervals (using values across the table for each row) for WEEKDAYS
meanlist <- rowMeans(intervaltable.weekday, na.rm = T)
# Create table of daily intervals against mean values across all days, with "Weekday" factor column
WeekdayType <- rep("Weekday", length(onedayinterval))
meantable.weekday <- data.frame(onedayinterval, meanlist, WeekdayType)

#### Create table of steps taken for each WEEKEND in columns
file.weekend <- subset(fileWithWeekdayType, WeekdayType == "Weekend", select = c(date, filledsteps))
weekenddates <- levels(factor(file.weekend[, "date"])) # levels(file.weekend[, "date"]) will give the original dates before subsetting
numofdays.weekend <- length(weekenddates)
weekendstepchunks <- (split(file.weekend[, "filledsteps"], weekenddates)) # Returns a list
intervaltable.weekend <- NULL
for (i in 1:numofdays.weekend) {
        intervaltable.weekend <- data.frame(c(intervaltable.weekend, weekendstepchunks[i]))
}
# Find the mean for each intervals (using values across the table for each row) for WEEKENDS
meanlist <- rowMeans(intervaltable.weekend, na.rm = T)

# Create table of daily intervals against mean values across all days, with "Weekend" factor column
WeekdayType <- rep("Weekend", length(onedayinterval))
meantable.weekend <- data.frame(onedayinterval, meanlist, WeekdayType)

#### Combine the data frames for WEEKDAYS and WEEKENDS
meantable.weekdaytype <- rbind(meantable.weekday, meantable.weekend)

#### Plot the mean for WEEKDAYS & WEEKENDS (Lattice Panel Plot)
library(lattice)
xyplot(meanlist ~ onedayinterval | WeekdayType, data = meantable.weekdaytype, layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of Steps")

