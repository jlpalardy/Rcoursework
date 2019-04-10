#Domestic Gross % by Genre (Movie Data)

#Import data
setwd("/home/jess/")
df <- read.csv("movieGross.csv")
head(df)
tail(df)
str(df)
#Convert some of these factors into numerics
df$Adjusted.Gross...mill. <- as.numeric(as.character(df$Adjusted.Gross...mill.))
df$Gross...mill. <- as.numeric(as.character(df$Gross...mill.))
df$Overseas...mill. <- as.numeric(as.character(df$Overseas...mill.))
df$Profit...mill. <- as.numeric(as.character(df$Profit...mill.))
#Double check data types and values using str and head/tail

studioFilter <- (df$Studio == "Buena Vista Studios") | (df$Studio == "Fox") | (df$Studio == "Paramount Pictures") | (df$Studio == "Sony") | (df$Studio == "Universal") | (df$Studio == "WB")
genreFilter <- (df$Genre == "action") | (df$Genre == "adventure") | (df$Genre == "animation") | (df$Genre == "comedy") | (df$Genre == "drama")
filteredDf <- df[studioFilter & genreFilter, ]

#Explore data frames, verify that filteredDf is filtered properly
head(filteredDf)
tail(filteredDf)
str(filteredDf)

#Boxplot, Domestic Gross % by Genre
q <- ggplot(data = filteredDf)
filteredBoxplot <- q + aes(x = Genre, y = Gross...US, color = Studio) + geom_jitter(aes(color = Studio, size = Budget...mill.)) + geom_boxplot(size = .5, color = "black", alpha = .5) + theme(text = element_text(family = "Comic Sans MS"), axis.title.x = element_text(color = "Blue", size = 20), axis.title.y = element_text(color = "Blue", size = 20), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 25), legend.title = element_text(size = 12), legend.text = element_text(size = 10)) + ggtitle("Domestic Gross % by Genre")
filteredBoxplot$labels$size = "Budget $M"
filteredBoxplot$labels$y = "Domestic Gross %"
filteredBoxplot