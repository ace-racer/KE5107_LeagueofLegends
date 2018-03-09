library(caret)

# load the iris dataset
setwd("/Users/davidleonardi/Projects/KE5107_LeagueofLegends")
matches <- read.csv("processed_matches.csv",na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

# set default seed value to 42 (Rattle default seed value)
seed_value = 42
set.seed(seed_value)

# convert target (y) variable type to factor
matches$bResult <- as.factor(make.names(matches$bResult))

# define training control
# use 10-folds cross validation
train_control <- trainControl(method="cv", 
                              number=10)

# train the model 
# target = bResult
# input
# * rWinEfficiency
# * bWinEfficiency
# * isBluePreferredLineup
# * isRedPreferredLineup
# * isBluePreferredChamps
# * isRedPreferredChamps
# method = Binomial Logistic Regession

input <- c("rWinEfficiency", "bWinEfficiency",
           "isBluePreferredLineup", "isRedPreferredLineup", 
           "isBluePreferredChamps", "isRedPreferredChamps")

target <- "bResult"

data <- matches[, c(input, target)]

# findLinearCombos(data)

# split data to 85% training and 15% testing data
sample_size = floor(0.85 * nrow(data))
training_index = sample(seq_len(nrow(data)), size=sample_size)
training_data = data[training_index,]
testing_data = data[-training_index,]

# get training header
str(training_data)

tunegrid <- data.frame(.mtry=2)

metric <- "Accuracy"

# Support Vector Machine Model
model <- train(bResult~., 
               data=training_data, 
               tuneGrid=tunegrid,
               ntree=2000,
               trControl=train_control, 
               metric=metric,
               method = "rf")

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

