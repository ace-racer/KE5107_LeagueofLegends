library(caret)
# load the iris dataset
setwd("/Users/davidleonardi/Projects/KE5107_LeagueofLegends")
matches <- read.csv("processed_matches.csv",na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

# convert target (y) variable type to factor
matches$bResult <- as.factor(matches$bResult)

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
           "bTowers10Mins", "rTowers10Mins", 
           "rInhibs10Mins", 
           "bDragons10Mins", "rDragons10Mins", 
           "bKills10Mins", "rKills10Mins",
           "goldblue10Mins", "goldred10Mins",
           "isBluePreferredLineup",
           "isRedPreferredLineup", "isBluePreferredChamps",
           "isRedPreferredChamps")

target <- "bResult"

data <- matches[, c(input, target)]

findLinearCombos(data)

# set default seed value to 42 (Rattle default seed value)
seed_value = 42
set.seed(seed_value)

# split data to 85% training and 15% testing data
sample_size = floor(0.85 * nrow(data))
training_index = sample(seq_len(nrow(data)), size=sample_size)
training_data = data[training_index,]
testing_data = data[-training_index,]

model <- train(bResult~., data=training_data, trControl=train_control, method="glm", family="binomial")

# summarize results
print(model)

# run prediction
prediction <- predict(model)

# confusion matrix based on training data
confusionMatrix(prediction, training_data$bResult)
