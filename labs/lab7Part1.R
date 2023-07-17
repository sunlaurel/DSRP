## get my data
my_data = read.csv("data/movies_updated.csv")

## loading the packages
library(tidyr)
library(janitor)

'- Choose a categorical variable to group your data, and choose just 2 of those groups to compare
- Select a numeric variable to compare
- Write out the null hypothesis you are trying to test-- 
  Should this be one-tailed or two-tailed? Should this be paired or unpaired?
- Conduct a t-test to determine if there is a significant difference between the 2 groups. 
  Make sure to set the correct parameters based on your hypothesis
- Report the p-value and write a statement about your findings 
  (e.g. There is/isn’t a significant difference in the mean numeric-variable between groups
  categorical-group-1 and categorical-group-2)'

## group by stars, and compare Robert De Niro (27 appearances) and Tom Hanks (24 appearances)
my_data |> summarize(count = n(), .by = star) |> slice_max(count, n = 10)

## see if the actor is related to the score that a movie received
## null hypothesis: there is no correlation between the actor who starred in a movie and the its score
## this should be a two-tailed and unpaired test

robert_de_niro_movies <- my_data |> filter(star == "Robert De Niro")
tom_hanks_movies <- my_data |> filter(star == "Tom Hanks")

t.test(robert_de_niro_movies$score, tom_hanks_movies$score)

## There is no significant difference in the mean score between the movies that Tom Hanks starred in
## and the movies that Robert De Niro starred in. However, it might be because these two actors are
## equally popular among fans. The p-value was 0.9948


'Let us try with two different actors who may not be of the same caliber'
john_travolta_movies <- my_data |> filter(star == "John Travolta")

t.test(john_travolta_movies$score, tom_hanks_movies$score)

## This time, there is a clear significant difference between the mean scores of movies starring
## John Travolta and the movies starring Tom Hank. The p-value is a 0.0009014, which goes to show
## that having popular actors star in your movie does have its benefits





