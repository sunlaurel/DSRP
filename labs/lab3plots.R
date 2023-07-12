library(ggplot2)

list <- read.csv("data/movies_updated.csv")

## one variable
ggplot(data = list, aes(y = genre)) + 
  geom_histogram(color = "white", fill = "red", alpha = 0.5, stat = "count", 
                 binwidth = 0.3) +
  theme_bw()

## one numeric, one categorical
ggplot(data = list, aes(y = rating, x = gross, fill = rating)) + 
  geom_bar(alpha = 0.75, stat = "summary", fun = "mean") +
  theme_bw()

## two numerics
ggplot(data = list, aes(x = score, y = gross)) + geom_smooth(color = "black") +
  theme_bw()
