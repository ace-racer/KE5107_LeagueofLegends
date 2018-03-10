library(caret)
library(dplyr)

# load the iris dataset
setwd("/Users/davidleonardi/Projects/KE5107_LeagueofLegends")
matches <- read.csv("processed_matches.csv",na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

# set default seed value to 42 (Rattle default seed value)
seed_value = 42
set.seed(seed_value)

# convert target (y) variable type to factor
matches$bResult <- as.factor(make.names(matches$bResult))

# train the model 
# target = bResult
# input
# * rWinEfficiency
# * bWinEfficiency
# * isBluePreferredLineup
# * isRedPreferredLineup
# * isBluePreferredChamps
# * isRedPreferredChamps
# method = Decision Tree

input <- c("rWinEfficiency", "bWinEfficiency",
           "isBluePreferredLineup", "isRedPreferredLineup", 
           "isBluePreferredChamps", "isRedPreferredChamps")

target <- "bResult"

data <- matches[, c(input, target)]

# findLinearCombos(data)

# split data to 70% training, 15% validation, and 15% testing data
number_of_rows = nrow(data)
training_index <- sample(number_of_rows, 0.7*number_of_rows)

number_of_rows %>%
  seq_len() %>%
  setdiff(training_index) %>%
  sample(0.15*number_of_rows) ->
  validate_index

number_of_rows %>%
  seq_len() %>%
  setdiff(training_index) %>%
  setdiff(validate_index) ->
  testing_index

training_data = data[training_index,]
validate_data = data[validate_index,]
testing_data = data[testing_index,]

# get training header
str(training_data)

tunegrid <- expand.grid(.cp = c(0.01, 0.05))

# Support Vector Machine Model
model <- caret::train(bResult~., 
               data=training_data,
               method = "rpart",
               tuneGrid = tunegrid,
               metric = "Accuracy",
               control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 3))

# summarize results
print(model)

# run prediction on training data
prediction_train <- predict(model, training_data)

# confusion matrix based on training data
confusionMatrix(prediction_train, training_data$bResult)

# run prediction on testing data
prediction_test <- predict(model, testing_data)

# confusion matrix based on testing data
confusionMatrix(prediction_test, testing_data$bResult)

