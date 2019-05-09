#Machine Utilization
#From Kirill Eremenko's Advanced Analytics In R for Data Science, Section 3: Lists in R (https://sds-platform-private.s3-us-east-2.amazonaws.com/uploads/P3-Section3-The-Challenge.pdf)
#Goal: 
#You have been engaged as a Data Science consultant by a coal terminal. They would like you to investigate one of their heavy machines - RL1.
#You have been supplied one month worth of data for all of their machines. The dataset shows what percentage of capacity for each machine was idle (unused) in any given hour.
#You are required to deliver an R list with the following components:
#Character :   Machine name
#Vector :        (min, mean, max) utilisation for the month (excluding unknown hours)
#Logical:        Has utilisation ever fallen below 90%?   TRUE / FALSE
#Vector :        All hours where utilisation is unknown (NA’s)
#Dataframe:  For this machine
#Plot:            For all machines

#Import the data and explore it
setwd("/home/jess/Downloads")
data_util <- read.csv("P3-Machine-Utilization.csv")

str(data_util)
head(data_util,10)
summary(data_util)

#We are asked about utilization percentage, NOT idle percentage. So, let's add a column to our data frame
#to represent this...

data_util$percentUtil <- 1- data_util$Percent.Idle
head(data_util,10) #Check to make sure column is there and that values make sense...

#Deal with the timestamp: add another column with the timestamp in POSIXct 
#But first, confirm what format these dates are in

tail(data_util)
#Ok, so these dates are in DD/MM/YYYY format
data_util$POSIXct_time <- as.POSIXct(data_util$Timestamp, format = "%d/%m/%Y %H:%M")
head(data_util)

#Remove original timestamp, and rearrange so Posix time in front
data_util$Timestamp <- NULL
data_util <- data_util[,c(4,1,2,3)]
head(data_util)

#Let's subset our data to include only data for RL1 
RL1df <- data_util[data_util$Machine == "RL1",]
summary(RL1df)
RL1df$Machine <- factor(RL1df$Machine)
summary(RL1df)

#Create vector of stats for RL1 (min, mean, max utilization for the month, excluding NAs)
RL1_stats <- c(Min = min(RL1df$percentUtil, na.rm = TRUE), Mean = mean(RL1df$percentUtil, na.rm = TRUE), Max = max(RL1df$percentUtil, na.rm = TRUE))
RL1_stats

#Create a "flag" (T/F) for whether utilization has ever fallen below 90% (if you look at RL1_stats, it clearly has, but let's do it the right way)
log_flag <- length(which(RL1df$percentUtil  < .90)) > 0
log_flag

#Start creating the list deliverable
listRL1 <- list(Machine = "RL1", Stats = RL1_stats, LowThreshold = log_flag)
listRL1

#Just playing around with accessing things in lists....
listRL1[1]
typeof(listRL1[1])
listRL1[[1]]
typeof(listRL1[[1]])
listRL1$Machine
typeof(listRL1$Machine)

#Challenge: How do you access the maximum utlization using methods of accessing stuff in lists?
#First, access the Stats element of the list using $ and then go within that element (which is a vector) and pick the Max element using the name of that vector element "Max"
listRL1$Stats["Max"]

#Create a vector of all hours where utilisation is unknown (NA’s) and add that to our list deliverable
listRL1$UnknownHours <- RL1df[is.na(RL1df$percentUtil), "POSIXct_time"]
listRL1

#Add dataframe for RL1 to our list deliverable
listRL1$RL1dataframe <- RL1df
listRL1

#Challenge: How can you access the first element in "UnknownHours"?
listRL1$UnknownHours[1]
#OR
listRL1[[4]][1]

#Playing with subsetting lists...
listRL1[c("Machine", "LowThreshold")]
listRL1[1:3]
listRL1[2]

#Create timeseries plot
library(ggplot2)
p <- ggplot(data = data_util)
p1 <- p + geom_line(aes(x = POSIXct_time, y = percentUtil, color = Machine)) + facet_grid(Machine ~ .) + geom_hline(yintercept = .90, linetype = 3)
p1

#Add plot to list 
listRL1$MachineUtilPlot <- p1
str(listRL1)