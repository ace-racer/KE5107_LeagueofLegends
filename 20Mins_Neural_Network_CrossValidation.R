library(caret)

# load the dataset
setwd('C:/Users/User/Documents/KE5107_LeagueofLegends')
matches <- read.csv("processed_matches.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

# set default seed value to 42 (Rattle default seed value)
seed_value = 42
set.seed(seed_value)

# convert target (y) variable type to factor
matches$bResult <- as.factor(make.names(matches$bResult))

# define training control
# use 10-folds cross validation repeated 3 times
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
# target = bResult
# input
# * rWinEfficiency
# * bWinEfficiency
# * isBluePreferredLineup
# * isRedPreferredLineup
# * isBluePreferredChamps
# * isRedPreferredChamps
# * bTowers10Mins
# * rTowers10Mins
# * bInhibs10Mins
# * rInhibs10Mins
# * bDragons10Mins
# * rDragons10Mins
# * bBarons10Mins
# * rBarons10Mins
# * bKills10Mins
# * rKills10Mins
# * goldblue10Mins
# * goldred10Mins
# * golddiff_10mins
# method = Binomial Logistic Regession

# input <- c("rWinEfficiency", "bWinEfficiency",
#           "bTowers10Mins", "rTowers10Mins", 
#           "bInhibs10Mins", "rInhibs10Mins", 
#           "bDragons10Mins", "rDragons10Mins", 
#           "bBarons10Mins", "rBarons10Mins", 
#           "bKills10Mins", "rKills10Mins",
#           "goldblue10Mins", "goldred10Mins",
#           "golddiff_10mins", "isBluePreferredLineup",
#           "isRedPreferredLineup", "isBluePreferredChamps",
#           "isRedPreferredChamps")

input <- c("rWinEfficiency", "bWinEfficiency", 
           "bTowers20Mins", "rTowers20Mins",
           "bInhibs20Mins", "rInhibs20Mins",
           "bDragons20Mins", "rDragons20Mins",
           "bBarons20Mins", "rBarons20Mins",
           "bKills20Mins", "rKills20Mins",
           "goldblue20Mins", "goldred20Mins",
           "isBluePreferredLineup", 
           "isRedPreferredLineup",  "isBluePreferredChamps", 
           "isRedPreferredChamps")

target <- "bResult"

data <- na.exclude(matches[, c(input, target)])

# findLinearCombos(data)

# split data to 85% training and 15% testing data
sample_size = floor(0.85 * nrow(data))
training_index = sample(seq_len(nrow(data)), size=sample_size)
training_data = data[training_index,]
testing_data = data[-training_index,]


model <- train(bResult~., data=training_data, trControl=train_control, 
                      method = "nnet", tuneGrid=expand.grid(size=c(30), decay=c(0.1)), linout = FALSE)   

#  a sigmoidal activation function is used and all of the predictions will be constrained to be on [0, 1].



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


