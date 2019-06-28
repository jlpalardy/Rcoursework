#Weather Data Project (From Advanced R Programming, Kirill Eremenko)

#You are working on a project for a meteorology bureau. You have been supplied weather data 
#for 4 cities in the US: Chicago, NewYork, Houston and SanFrancisco.You are required to deliver 
#the following outputs:
#1. A table showing the annual averages of each observed metric for every city
#2. A table showing by how much temperature fluctuates each month from min to max (in %). 
#Take min temperature as the base
#3. A table showing the annual maximums of each observed metric for every city
#4. A table showing the annual minimums of each observed metric for every city
#5. A table showing in which months the annual maximums of each metric were observed 
#in every city (Advanced)

setwd("/home/jess/Rcourses/Rcourse2/Weather/P3-Weather-Data/Weather.Data")

#We have 2 files for each city. We are choosing the files in Celsius.
Chicago <- read.csv("Chicago-C.csv", row.names = 1)# The row.names = 1 keeps the row names from being col # 1
Houston <- read.csv("Houston-C.csv", row.names = 1)
NewYork <- read.csv("NewYork-C.csv", row.names = 1)
SanFrancisco <- read.csv("SanFrancisco-C.csv", row.names = 1)
#Take a look at the data
Chicago
Houston
NewYork
SanFrancisco

#We want these as matrices, because we have all the same data types (numeric)
Chicago <- as.matrix(Chicago)
Houston <- as.matrix(Houston)
NewYork <- as.matrix(NewYork)
SanFrancisco <- as.matrix(SanFrancisco)

#Put everything in a list:
Weather <- list(Chicago = Chicago, NewYork = NewYork, Houston = Houston, SanFrancisco = SanFrancisco)
Weather

#Practice with apply()... Not the most efficient way to get deliverable, just playing
ChicagoAverages <- apply(Chicago, 1, mean)
ChicagoAverages
HoustonAverages <- apply(Houston, 1, mean)
HoustonAverages
NewYorkAverages <- apply(NewYork, 1, mean)
NewYorkAverages
SanFranciscoAverages <- apply(SanFrancisco, 1, mean)
SanFranciscoAverages

#Annual maxes and mins of each metric for each city. Again, playing with apply, not efficient. 
ChicagoMax <- apply(Chicago, 1, max)
ChicagoMax
HoustonMax <- apply(Houston, 1, max)
HoustonMax
NewYorkMax <- apply(NewYork, 1, max)
NewYorkMax
SanFranciscoMax <- apply(SanFrancisco, 1, max)
SanFranciscoMax

ChicagoMin <- apply(Chicago, 1, min)
ChicagoMin
HoustonMin <- apply(Houston, 1, min)
HoustonMin
NewYorkMin <- apply(NewYork, 1, min)
NewYorkMin
SanFranciscoMin <- apply(SanFrancisco, 1, min)
SanFranciscoMin

#Practice with lapply()
lapply(Weather, rowMeans) #Takes the mean of each row of each matrix within Weather
#lapply with []
lapply(Weather, "[", 1, 1) #Selects the [1,1] element from each matrix within Weather, makes a list
lapply(Weather, "[", 1, ) #Selects the first row of each matrix from Weather (in this case, AvgHigh_C)

#Play with defining our own functions in lapply
tempFluctuations <- lapply(Weather, function(x) round((x[1,] - x[2,])/x[2,],2))
tempFluctuations

#Work on deliverables, USE SAPPLY TO MAKE IT PRESENTABLE 

#1. A table showing the annual averages of each observed metric for every city
annualAveragesDeliv <- round(sapply(Weather, rowMeans), 2)
annualAveragesDeliv

#2. A table showing by how much temperature fluctuates each month from min to max (in %). 
#Take min temperature as the base
tempFlucDeliv <- sapply(Weather, function(x) round((x[1,] - x[2,])/x[2,],2))
tempFlucDeliv

#3. A table showing the annual maximums of each observed metric for every city
#nest an apply function inside an sapply function using ability to define function used
#in apply family functions
annualMaxesDeliv <- sapply(Weather, apply, 1, max)
annualMaxesDeliv

#4. A table showing the annual minimums of each observed metric for every city
annualMinsDeliv <- sapply(Weather, apply, 1, min)
annualMinsDeliv

#5. A table showing in which months the annual maximums of each metric were observed 
#in every city (Advanced)
maxMonthDeliv <- sapply(Weather, function (y) apply(y, 1, function (x) names(which.max(x))))
maxMonthDeliv
#Note that this only provides the first month that the maxes occur! This won't indicate if the max
#is repeated, and NAs are discarded!
#Also- this is a 3-layered iteration! sapply goes through each matrix in the list. The function (y)
#apply goes through each row of the matrices in the list and applies function(x), which is a 
#nested function that provides the index of the max value in each row (which.max) and then returns
#the name of the index (names)


