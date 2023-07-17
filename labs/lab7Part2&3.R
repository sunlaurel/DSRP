## loading the libraries
library(tidyr)
library(dplyr)
library(janitor)

my_data = read.csv("data/movies_updated.csv")

## null hypothesis: there is no significance between the genre of a movie and its score
## The three most popular genres are comedy (1355), drama (807), and action (775)
top3Genres <- my_data |> 
  summarize(.by = genre, count = sum(!is.na(genre))) |> slice_max(count, n = 3)

moviesInTop3Genres <- my_data |> filter(genre %in% top3Genres$genre)

results <- aov(score ~ genre, moviesInTop3Genres)
summary(results)
TukeyHSD(results)

## There are significant differences between all the scores and the genres. 
## p score between comedy and action is 0.0002737
## p score between drama and action is 0
## p score between drama and comedy is 0

topCompanies <- my_data |> 
  summarize(.by = company, count = sum(!(is.na(company)))) |> 
  slice_max(order_by = count, n = 3)

new_data <- my_data |> filter(company %in% topCompanies$company, genre %in% top3Genres$genre)

t <- table(new_data$genre, new_data$company)

chisq_result <- chisq.test(t)
chisq_result$p.value
chisq_result$residuals
## the p-value is 0.087739
## Conclusion: there is no significant association between the company and the genre of the movie it releases
## based on the chi-squared test (p-value = 0.087739)
