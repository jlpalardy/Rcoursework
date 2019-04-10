#Data
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

#Except for profitMargin, all these variables are done without rounding so that
#we don't lose accuracy. Rounding will come when we print the results. 
profitPretax <- revenue - expenses 
profitPosttax <- .70 * profitPretax #Profit after 30% tax
profitMargin <- round((profitPosttax / revenue) * 100, 0) #Rounded profit margin, as a %
meanProfitPostTax <- mean(profitPosttax) #Average monthly profit (post taxes)
goodMonths <- profitPosttax > meanProfitfPostTax #months where profit (post taxes) higher than the mean
badMonths <- profitPosttax < meanProfitfPostTax #months where profit (post taxes) lower than the mean
bestMonth <- max(profitPosttax) 
worstMonth <- min(profitPosttax)

#Before we print our results, we will express our results in units of 
#thousands of dollars and round to zero decimal places.
"Profits before taxes:"
profitPretax <- round(profitPretax / 1000, 0)
profitPretax

"Profits after taxes:"
profitPosttax <- round(profitPosttax / 1000, 0)
profitPosttax

"Profit margin:"
profitMargin

"Mean profit after taxes:"
meanProfitfPostTax <- round(meanProfitfPostTax / 1000, 0)
meanProfitfPostTax

"Good months:"
goodMonths

"Bad months:"
badMonths

"Best month:"
bestMonth <- round(bestMonth / 1000, 0)
bestMonth

"Worst month:"
worstMonth <- round(worstMonth / 1000, 0)
worstMonth
