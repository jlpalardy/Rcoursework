#Future 500 Images

#Import data, explore 
setwd("/home/jess/Downloads")
fin <- read.csv("P3-Future-500-The-Dataset.csv", na.strings = c(""))

str(fin)
head(fin)
tail(fin)
summary(fin)

#See rows with missing values
finNAs <- fin[!complete.cases(fin), ]
finNAs

#Convert ID and Inception columns to factors
fin$ID <- factor(fin$ID)
fin$Inception <- factor(fin$Inception)
#Double check
str(fin)

#Change formatting in Revenue, Expenses, and Growth columns so that we can convert to numerics
fin$Expenses <- gsub(" Dollars", "", fin$Expenses)
fin$Expenses <- gsub(",", "", fin$Expenses)
fin$Revenue <- gsub("\\$", "", fin$Revenue)
fin$Revenue <- gsub(",", "", fin$Revenue)
fin$Growth <- gsub("%", "", fin$Growth)

fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)