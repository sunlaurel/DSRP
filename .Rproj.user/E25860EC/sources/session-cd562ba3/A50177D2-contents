## loading in the libraries
## install.packages('caret')

library(dplyr)
library(Metrics)
library(parsnip)
library(tidyr)
library(rsample)
library(reshape2)
library(janitor)
library(yardstick)
library(caret)

'
Create something that can predict the score a movie will get based on the genre, company, and actor
1.Collect Data
2.Clean & process data
3.Visualize data
4.Perform feature selection
5.Separate data into train/test sets
6.Choose suitable model (linear regression, logistic regression, or boosted decision tree, random forest)
7.Train model on training dataset
8.Evaluate performance on test dataset
             -For regression, report your error as RMSE and MAE
             -For classification, report the F1 score for accuracy

Now, go back to either step 4 or step 6 and pick different variables to use in your model and/or use a 
different model type. Re-train and re-calculate error or accuracy. Which model performed better?'

#### Regression model ####

##### step 1, collect data #####
movies <- read.csv("data/movies_updated.csv")

##### step 2, filter and process data #####
validMovies <- filter(movies, !is.na(rating), !is.na(genre), !is.na(gross), budget > 0, !is.na(gross))
movieAllNumeric <- select(validMovies, score, budget, gross, votes, year)

##### step 3, visualize data #####
movieCor <- movieAllNumeric |>
  cor() |>
  melt() |>
  as.data.frame()

ggplot(movieCor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "darkblue", mid = "white", midpoint = 0)

## high correlation
ggplot(movieAllNumeric, aes(x = votes, y = gross)) +
  geom_point(alpha = 0.2, color = "purple") +
  theme_minimal()

## low correlation
ggplot(movieAllNumeric, aes(x = score, y = gross)) +
  geom_point() +
  theme_minimal()

##### Step 4: Perform feature selection #####
## Predict the gross (regression) 

##### Step 5, separate data into testing and training sets #####

## Setting a seed
set.seed(71823)

## Creating a split
reg_split <- initial_split(movieAllNumeric, prop = 0.8)

## Use the split to form testing and training sets
reg_train <- training(reg_split)
reg_test <- testing(reg_split)

##### Step 6 & 7, training and testing the model using linear regression #####
lm_fit <- linear_reg() |> 
  set_engine("lm") |>
  set_mode("regression") |>
  fit(gross ~ budget + votes, data = reg_train)

summary(lm_fit$fit)
## p value is less than 2.2e-16

boosted_tree_fit <- boost_tree(trees = 100) |> 
  set_engine("xgboost") |>
  set_mode("regression") |>
  fit(gross ~ ., data = reg_train)

boosted_tree_fit$fit$evaluation_log

##### Step 8, evaluate model performance on test set #####
## Calculate error for regression
## lm_fit
reg_results <- reg_test

reg_results$lm_pred <- predict(lm_fit, reg_test)$.pred

reg_results$boosted_tree_pred <- predict(boosted_tree_fit, reg_test)$.pred

## mean absolute error
yardstick::mae(reg_results, score, lm_pred)
yardstick::mae(reg_results, score, boosted_tree_pred)

## root mean squared error
yardstick::rmse(reg_results, score, lm_pred)
yardstick::rmse(reg_results, score, boosted_tree_pred)

## Both models were absolutely terrible at predicting the gross of the movie,
## probably because there wasn't a strong correlation between any of the variables

#### Classification model ####

## filtering the data
newMovies <- movies
newMovies$score <- as.factor(trunc(as.numeric(movies$score)))
classMovies <- newMovies |> 
  filter(budget > 0,
         !is.na(gross),
         !is.na(rating),
         !rating == "X") |>
  select(-c(name, released, director, writer, country, runtime.., star, company))

class_split <- initial_split(classMovies, prop = 0.75)

## Train and test based on the data set
class_train <- training(class_split)
class_test <- testing(class_split)

## Trying to predict what the score of a movie will be, using boosted tree and random forest
boost_class_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(score ~ ., data = class_train)

boost_class_fit$fit$evaluation_log

forest_class_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(score ~ ., data = class_train)

forest_class_fit$fit

class_results <- class_test

class_results$boost_pred <- predict(boost_class_fit, class_test)$.pred_class
class_results$forest_pred <- predict(forest_class_fit, class_test)$.pred_class

View(select(class_results, boost_pred, forest_pred, score))

matrix <- confusionMatrix(class_results$boost_pred, class_results$score, mode = "everything")
byclass <- matrix[['byClass']]

matrix2 <- confusionMatrix(class_results$forest_pred, class_results$score, mode = "everything")
byclass <- matrix2[['byClass']]

#### PCA ####
pca <- prcomp(movieAllNumeric, scale. = T)
summary(pca)
pca$rotation ^ 2

pca_vals <- as.data.frame(pca$x)

ggplot(pca_vals, aes(PC1, PC2)) +
  geom_point() +
  theme_minimal()

