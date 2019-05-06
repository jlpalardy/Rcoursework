#Future 500 
#Data cleaning exercise

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

#Look for NAs in general
fin[!complete.cases(fin), ]
#Look for NAs in specific columns
fin[is.na(fin$ID), ] #0 with no ID 
fin[is.na(fin$Name), ] #0 with no name
fin[is.na(fin$Industry), ] #2 with no industry
fin[is.na(fin$Inception), ] #1 with no inception year
fin[is.na(fin$Employees), ] #2 with no employee # data
fin[is.na(fin$State), ] #4 with no state 
fin[is.na(fin$City), ] #0 with no city
fin[is.na(fin$Revenue), ] #2 with no revenue data
fin[is.na(fin$Expenses), ] #3 with no expenses data
fin[is.na(fin$Profit), ] #2 with no profit data
fin[is.na(fin$Growth), ] #1 with no growth data 

#make a backup
fin_backup <- fin

#Remove entries with no industry value- can't research it (these companies are imaginary)
#and the industry value will be used in later analysis- can't leave it blank.
fin <- fin[!is.na(fin$Industry), ]
#Double check:
fin
#Reset indices
rownames(fin) <- NULL
#Notice these indices don't match the ID values in the ID column!

#Add missing state data- no missing city data, and we know where NYC and San Francisco are.
fin[is.na(fin$State) & fin$City == "New York", "State"] <- "NY"
fin[is.na(fin$State) & fin$City == "San Francisco", "State"] <- "CA"
#Check:
fin[fin$ID == 11 | fin$ID == 84 | fin$ID == 267 | fin$ID == 379, ]

#There are two companies with missing employee numbers. Substitute in the mean employee numbers
#for companies in that industry. Remember to remove NAs, otherwise the median will be NA!

#For Greenfax...
fin[fin$Name == "Greenfax", "Employees"] <- median(fin[fin$Industry == "Retail","Employees"], na.rm = TRUE)
#Double check...
fin[fin$Name == "Greenfax", ]

#For Westminster...
fin[fin$Name == "Westminster", "Employees"] <- median(fin[fin$Industry == "Financial Services", "Employees"], na.rm = TRUE)
#Double check...
fin[fin$Name == "Westminster", ]


#Replace NAs in growth column with median in relevant industry
#This time, don't immediately replace with median, just so it's safer...
medianGrowthConstruction <- median(fin[fin$Industry == "Construction", "Growth"], na.rm = TRUE)
medianGrowthConstruction
#If you look at the column fin[fin$Industry == "Construction", "Growth"], this median (10) makes sense
#I'm using this method because there is only one company with an NA for growth. Otherwise, could generalize by
#using fin[is.na(fin$Growth) & fin$Industry == "Construction", "Growth"]
fin[fin$Name == "Rednimdox", "Growth"] <- medianGrowthConstruction
#check
fin[fin$Name == "Rednimdox", ]

#Check out what's left to do...
fin[!complete.cases(fin),]

#Rednimdox and Ganzgreen are both in the construction industry, and neither has revenue, expenses, or 
#profit info. 
medRevenueConstruction <- median(fin[fin$Industry == "Construction", "Revenue"], na.rm = TRUE)
fin[fin$Industry == "Construction" & is.na(fin$Revenue), "Revenue"] <- medRevenueConstruction
#Check...
fin[fin$Name == "Rednimdox" | fin$Name == "Ganzgreen", ]

fin[!complete.cases(fin),]
#I still have no expenses data for Rednimdox or Ganzgreen (again, both in construction)
#So, once again, find the median expenses in the construction industry and substitute that.
#Expenses for Ganzlax can be calculated and substituted in later, and profit for Rednimdox and 
#Ganzgreen can be calculated once I've substituted in a value for expenses.

medExpensesConstruction <- median(fin[fin$Industry == "Construction", "Expenses"], na.rm = TRUE)
#Once again, median looks reasonable. 
#Notice that we aren't substituting for the expenses of that IT company, Ganzlax. We can solve for that 
#exactly, because we have the revenue and profit. It's not an issue because Ganzlax is filtered out
#since it's not in construction. We could also filter that out even MORE if we added a filter 
# & is.na(fin$Profit)
fin[fin$Industry == "Construction" & is.na(fin$Expenses), "Expenses"] <- medExpensesConstruction
#Double check...
fin[fin$Name == "Rednimdox" | fin$Name == "Ganzgreen", ]

#Do another data backup
fin_backup2 <- fin

#It seems that Rednimbox and Ganzgreen have Revenue and Expenses, but no Profit value. We can solve for that. 
fin[is.na(fin$Profit), "Profit"] <- fin[is.na(fin$Profit), "Revenue"] - fin[is.na(fin$Profit), "Expenses"]
#Double check
fin[fin$Name == "Rednimdox" | fin$Name == "Ganzgreen", ]

#Now, from earlier I saw Ganzlax is missing an Expenses value, although it has Revenue and Profits. 
fin[is.na(fin$Expenses), "Expenses"] <- fin[is.na(fin$Expenses), "Revenue"] - fin[is.na(fin$Expenses), "Profit"]
#Double check
fin[fin$Name == "Ganzlax", ]

#The last record has a missing inception date... we'll leave that for now, since it won't affect analysis.

#Visualizations
library(ggplot2)
p <- ggplot(data = fin)
#Color blindness friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#A scatterplot by industry showing expenses, revenue, and profit
scatter_ <- p + geom_point(aes(x = Revenue, y = Expenses, color = Industry, size = Profit, alpha = .3)) + scale_color_manual(values=cbPalette) 
scatter_

#Industry trends in revenue and expenses
q <- ggplot(data = fin, aes(x = Revenue, y = Expenses, color = Industry)) + scale_color_manual(values=cbPalette)
scatter_trends <- q + geom_point() + geom_smooth(fill = NA, size = 1.2) 
scatter_trends

#Boxplot of growth by industry
v <- ggplot(data = fin, aes(x = Industry, y = Growth, color = Industry)) + scale_color_manual(values=cbPalette)
box_ <- v + geom_boxplot(alpha = .3) 
box_

#Boxplot of profit by industry
w <- ggplot(data = fin, aes(x = Industry, y = Profit, color = Industry)) + scale_color_manual(values=cbPalette)
box_profit <- w + geom_boxplot(alpha = .3)
box_profit
