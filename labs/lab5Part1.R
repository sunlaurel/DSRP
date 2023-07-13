## get my data
my_data = read.csv("data/movies_updated.csv")

## load the packages
library(tidyr)
library(janitor)

"""
- Choose a categorical variable to group your data, and choose just 2 of those groups to compare
- Select a numeric variable to compare
- Write out the null hypothesis you are trying to test-- 
  Should this be one-tailed or two-tailed? Should this be paired or unpaired?
- Conduct a t-test to determine if there is a significant difference between the 2 groups. 
  Make sure to set the correct parameters based on your hypothesis
- Report the p-value and write a statement about your findings 
  (e.g. There is/isn’t a significant difference in the mean numeric-variable between groups
  categorical-group-1 and categorical-group-2)
"""
## group by company, and compare Paramount Pictures and Universal Pictures
my_data |> summarize(count = n(), .by = company) |> slice_max(count, n = 2)

## see if the company is related to the score that a movie received
