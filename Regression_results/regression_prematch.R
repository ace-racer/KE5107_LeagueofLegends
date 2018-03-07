
# Get the processed dataset
crs$dataset <- read.csv("/Users/pierlim/R_Projects/League_Of_Legends/LOL/processed/matches_processed.csv",
                        na.strings=c(".", "NA", "", "?"),
                        strip.white=TRUE, encoding="UTF-8")

# Build the train/validate/test datasets.
set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("rWinEfficiency", "bWinEfficiency",
                   "isBluePreferredLineup", "isRedPreferredLineup",
                   "isBluePreferredChamps", "isRedPreferredChamps")

crs$numeric   <- c("rWinEfficiency", "bWinEfficiency",
                   "isBluePreferredLineup", "isRedPreferredLineup",
                   "isBluePreferredChamps", "isRedPreferredChamps")

crs$categoric <- NULL

crs$target    <- "bResult"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("redTeamTag", "blueTeamTag", "League", "Year", "Season", "Type", "gamelength", "blueTop", "blueTopChamp", "blueJungle", "blueJungleChamp", "blueMiddle", "blueMiddleChamp", "blueADC", "blueADCChamp", "blueSupport", "blueSupportChamp", "blueBans", "redTop", "redTopChamp", "redJungle", "redJungleChamp", "redMiddle", "redMiddleChamp", "redADC", "redADCChamp", "redSupport", "redSupportChamp", "redBans", "Address", "bTowersNum", "bInhibsNum", "bDragonsNum", "bBaronsNum", "rTowersNum", "rInhibsNum", "rDragonsNum", "rBaronsNum", "bTowersFirstDestroyedTime", "rTowersFirstDestroyedTime", "TowersFirstKillBy", "bTowers5Mins", "rTowers5Mins", "bTowers10Mins", "rTowers10Mins", "bTowers20Mins", "rTowers20Mins", "bTowersQuarterGame", "rTowersQuarterGame", "bTowersHalfGame", "rTowersHalfGame", "bInhibsFirstDestroyedTime", "rInhibsFirstDestroyedTime", "InhibsFirstKillBy", "bInhibs5Mins", "rInhibs5Mins", "bInhibs10Mins", "rInhibs10Mins", "bInhibs20Mins", "rInhibs20Mins", "bInhibsQuarterGame", "rInhibsQuarterGame", "bInhibsHalfGame", "rInhibsHalfGame", "bDragonsFirstDestroyedTime", "rDragonsFirstDestroyedTime", "DragonsFirstKillBy", "bDragons5Mins", "rDragons5Mins", "bDragons10Mins", "rDragons10Mins", "bDragons20Mins", "rDragons20Mins", "bDragonsQuarterGame", "rDragonsQuarterGame", "bDragonsHalfGame", "rDragonsHalfGame", "bBaronsFirstDestroyedTime", "rBaronsFirstDestroyedTime", "BaronsFirstKillBy", "bBarons5Mins", "rBarons5Mins", "bBarons10Mins", "rBarons10Mins", "bBarons20Mins", "rBarons20Mins", "bBaronsQuarterGame", "rBaronsQuarterGame", "bBaronsHalfGame", "rBaronsHalfGame", "bKillsFirstDestroyedTime", "rKillsFirstDestroyedTime", "KillsFirstKillBy", "bKills5Mins", "rKills5Mins", "bKills10Mins", "rKills10Mins", "bKills20Mins", "rKills20Mins", "bKillsQuarterGame", "rKillsQuarterGame", "bKillsHalfGame", "rKillsHalfGame", "goldblue5Mins", "goldred5Mins", "goldblue10Mins", "goldred10Mins", "goldblue20Mins", "goldred20Mins", "goldblueQuarterGame", "goldredQuarterGame", "goldblueHalfGame", "goldredHalfGame", "FirstBlood", "golddiff_5mins", "golddiff_10mins", "golddiff_20mins", "YearSeason", "winner")
crs$weights   <- NULL

# Support vector machine. 
library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.
set.seed(crv$seed)
crs$ksvm <- ksvm(C=0.5, as.factor(bResult) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="rbfdot", kpar=list(sigma = 0.01),
                 prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm

# Do a grid search on the SVM model to get the best hyperparameters
library(mlr)
ksvm_task = makeClassifTask(data = crs$dataset[crs$train,c(crs$input, crs$target)], target = "bResult")

discrete_ps = makeParamSet(
  makeDiscreteParam("C", values = c(0.01, 0.05, 0.1,0.5)),
  makeDiscreteParam("sigma", values = c(0.005, 0.01, 0.05, 0.1,0.5))
)
print(discrete_ps)

ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 3L)

res = tuneParams("classif.ksvm", ksvm_task , rdesc, par.set = discrete_ps, control = ctrl)
print(res)

############################# Evaluation ###############################
# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$bResult, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$bResult, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))
