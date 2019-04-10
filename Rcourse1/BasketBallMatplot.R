
#Generic plotting function (makes sure we're plotting matrices with matplot, creates the same
#legend and uses the same colors, etc)
makePlot <- function (data, rows = Players[1:10], ylabel) {
  Data <- data[rows,, drop = F]
  matplot(t(Data), type = "b", pch = 1:9, col = 1:6, xlab = "Years since 2006", ylab = ylabel)
  legend("bottomleft",legend = rows, pch = 1:9, col = 1:6, horiz = F)}

#Field Goal Accuracy Plot
fieldGoalAccuracy <- round((FieldGoals / FieldGoalAttempts) * 100)
makePlot(fieldGoalAccuracy, ylab ="Field Goal Accuracy, %")

#Minutes Played per Game 
minutesPlayedPerGame <- round(MinutesPlayed/Games)
makePlot(minutesPlayedPerGame, ylab ="Minutes Played per Game")

#Points per Game
pointsPerGame <- round(Points/Games)
makePlot(pointsPerGame, Players[1:8], "Points per Game")
#Eliminated Derrick Rose and Dwayne Wade because their values at 2015 are ridiculous

#Pay per Game
payPerGame <- round((Salary/Games)/10000)
makePlot(payPerGame, Players[2:8], "Pay per Game in $10k")
#Eliminated Derrick Rose, Kobe Bryant, and Dwayne Wade because their values at 2015 are ridiculous

#Salary per Minutes Played in $1k
salaryperMinPlayed <- round((Salary/MinutesPlayed)/1000)
makePlot(salaryperMinPlayed, Players[2:8], "Pay per Minute Played in $1k")
#Eliminated Derrick Rose, Kobe Bryant, and Dwayne Wade because their values at 2015 are ridiculous

#Salary per Point Scored in $1k
salaryperPoint <- round((Salary/FieldGoals)/1000)
makePlot(salaryperPoint, Players[2:8],"Pay per Field Goal in $1k")
#Eliminated Derrick Rose, Kobe Bryant, and Dwayne Wade because their values at 2015 are ridiculous

#Points per Minute Played
pointsPerMinute <- round(Points/MinutesPlayed, 1)
makePlot(pointsPerMinute, ylab = "Points per Minute Played")

#Free Throw Attempts per Game
freeThrowAttemptsperGame <- round(FreeThrowAttempts/Games)
makePlot(freeThrowAttemptsperGame, ylab = "Free Throw Attempts per Game")

#Accuracy of Free Throws
freeThrowAccuracy <- round((FreeThrows/FreeThrowAttempts) * 100)
makePlot(freeThrowAccuracy, ylab = "% Accuracy, Free Throws")

#2 or 3 Point Field Goal Preference, taking into account free throws
pointsNoFreeThrows <- Points - FreeThrows
#Remember, free throws are 1 point each 
pointsPerFieldGoal <- round(pointsNoFreeThrows / FieldGoals, 1)
makePlot(pointsPerFieldGoal, ylab = "Points per Field Goal")
#Everyone has an an average point per field goal of less than 2.5 points (excluding free throws), 
#so it seems everyone prefers 2 point field goals to 3 point field goals. 2.5 would
#indicate no preference, anything 2.6 - 3.0 inclusive would be a 3 pointer preference.
