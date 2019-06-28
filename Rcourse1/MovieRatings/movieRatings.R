#Import movie data, set up packages
setwd("/home/jess/Rcourses/Rcourse1/MovieRatings")
df <- read.csv("P2-Movie-Ratings.csv")
library(ggplot2)
#Take a look at the data
str(df)
head(df)
tail(df)
summary(df)
#Rename columns so they play nicely with R
colnames(df) <- c("Film", "Genre", "CriticRating", "AudienceRating", "BudgetMillions", "Year")
#Convert Year into a factor
df$Year <- factor(df$Year)

#Various Plots
#Critic Rating vs Audience Rating
q <-ggplot(data = df)
q0 <- q + aes(x = CriticRating, y = AudienceRating, color = Genre, size = BudgetMillions) + geom_point(size = 3) + geom_smooth() + ggtitle("Critic Rating vs Audience Rating") + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.position = c(1,1), legend.justification = c(1,1))
q0
cor.test( ~ AudienceRating + CriticRating, data=df, method = "pearson", conf.level = 0.95)

#Critic Rating vs Audience Rating, Faceted by Genre and Year
q11 <- q0 + facet_grid(Genre ~ Year) + coord_cartesian(ylim = c(0,100)) + ggtitle("Critic Rating vs Audience Rating") + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.position = "none") + geom_smooth()
q11

#Budget vs Audience Rating
q1 <- q  + aes(x = BudgetMillions, y = AudienceRating, color = Genre) + geom_point() + facet_grid(Genre~.) + geom_smooth() + ggtitle("Budget vs Audience Rating") + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.position = "none")
q1
#Histogram of Film Budgets, colored by Genre
q2 <- q + aes(x = BudgetMillions) + geom_histogram(aes(fill = Genre), color = "Black", binwidth = 10) + ggtitle("Film Budget Distribution (By Genre)") + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.position = c(1,1), legend.justification = c(1,1), legend.text = element_text(size = 10))
q2
#Faceted Histogram of Same Thing
q9 <- q2 + facet_grid(Genre ~.) + theme(legend.position = "none")
q9
#Density chart representing same thing
q3 <- q + aes(x = BudgetMillions) + geom_density(aes(fill = Genre), position = "stack") + ggtitle("Film Budget Distribution (By Genre)") + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.position = c(1,1), legend.justification = c(1,1), legend.text = element_text(size = 10))  
q3
#Histogram of Audience Ratings
q4 <- q + aes(x = AudienceRating) + geom_histogram(binwidth = 5, fill = "LightBlue", color = "Black") + ggtitle("Audience Ratings") + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
q4
#Histogram of Audience Ratings, colored by Genre
q2 <- q + aes(x = AudienceRating) + geom_histogram(aes(fill = Genre), color = "Black", binwidth = 5) + ggtitle("Audience Ratings (By Genre)") + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.position = c(1,1), legend.justification = c(1,1))
q2
#Same, but by Genre, and Faceted
q10 <- q4 + geom_histogram(binwidth = 5, aes(fill = Genre), color = "Black") + facet_grid(Genre ~ .) + ggtitle("Audience Ratings (By Genre)") + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.position = "none")
q10
#Histogram of Critic Ratings
q5 <- q4 + aes(x = CriticRating) + ggtitle("Critic Rating Distribution") + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
q5
#Boxplots of Audience Ratings, by Genre (utilizing jitter)
q6 <- q + aes(x = Genre, y = AudienceRating, color = Genre) + geom_jitter() + geom_boxplot(size = 1.2, alpha = .5) + ggtitle("Audience Ratings (By Genre)") + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.position = "none")
q6
#Boxplots of Critic Ratings, by Genre (utilizing jitter)
q7 <- q6 + aes(x = Genre, y = CriticRating, color = Genre) + ggtitle("Critic Ratings (By Genre)") + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
q7
#Boxplot of Film Budgets, by Genre (utlizing jitter)
q8 <- q6 + aes(x = Genre, y = BudgetMillions, color = Genre) + ggtitle("Film Budgets (By Genre)") + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
q8
