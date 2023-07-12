library(dplyr)
library(ggplot2)

## Are certain companies capable of creating the best movies?
my_data = read.csv("data/movies_updated.csv")

## summarize the actor/actress with most appearances and the popularity of their films
View(arrange(filter(summarize(my_data, appearances = n(), popularity = mean(score), .by = star), appearances >= 5), desc(popularity)))
## surprisingly, Kevin Spacey was featured in best scored movies, with an average of 7.7 while starring in 7 movies

## make sure that the budgets are above 0
movies <- filter(my_data, budget > 0)

## see if the budget affects the score that the movie receives
ggplot(data = movies, 
       aes(x = budget, y = score)) +
  geom_point(color = "purple", alpha = 0.4) +
  theme_bw()

## filter out movies that have less than 10,000 votes to ensure that there is enough votes for an accurate score
my_data <- filter(my_data, votes > 10000)

## mutated by adding how many movies that they released, and the average score from company
my_data <- mutate(my_data, average_score = mean(score), .by = company, na.rm = T)
my_data <- mutate(my_data, movies_released = n(), .by = company)

## filter it by getting companies that released more than one movie
my_data <- filter(my_data, movies_released > 20)

## arrange it by most popular rated companies
my_data <- arrange(my_data, average_score)

## select only the columns with name, rating, score, budget, gross, company, run time, average score, and movies_released
my_data <- select(my_data, name:rating, score, budget:movies_released)
View(my_data)

