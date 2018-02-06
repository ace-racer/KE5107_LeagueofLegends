library(ggplot2)

matches_data = read.csv("data/matches.csv")


# Get the gold difference as a list of numbers - process the values by removing the [, ] brackets
getGoldDiffAsListOfNumbers <- function(matchNumber){
  # get the characters from the first row of the matches data and the golddiff column
  golddiffStr <- as.character(matches_data[matchNumber, c("golddiff")])
  
  # split the value by the comma
  golddiffList <- strsplit(golddiffStr,", ")
  
  # get the length of the number of entries in the gold difference list
  playLength <- length(golddiffList[[1]])
  
  # get the numbers equivalent to the elements
  golddiffListValues <- golddiffList[[1]]
  
  # replace [ with empty
  golddiffListValues[1] <- gsub("\\[", "", golddiffListValues[1])
  
  # replace ] with empty
  golddiffListValues[playLength] <- gsub("]", "", golddiffListValues[playLength])
  golddiffListValues[playLength] 
  
  # Convert values to numeric
  golddiffNumericList <- as.numeric(as.character(golddiffListValues))
  
  # return the list obtained
  golddiffNumericList
}

for (matchNumber in 1:nrow(matches_data)) {
  golddiffNumericList <- getGoldDiffAsListOfNumbers(matchNumber)
  timesBlueWithMoreGoldThanRed <- length(which(golddiffNumericList > 0))
  matchLength <- length(golddiffNumericList)
  
  # fraction of the game where blue team was leading in gold
  matches_data$ratioBlueMoreGoldThanRed[matchNumber] <- timesBlueWithMoreGoldThanRed/matchLength
}

for (i in 1:10){
  ratioOfMatchBlueLedInGold <- 0.1 * i
  timesBlueTeamWonForRatio[i] <- length(which(matches_data$bResult[matches_data$ratioBlueMoreGoldThanRed > (ratioOfMatchBlueLedInGold - 0.1) & matches_data$ratioBlueMoreGoldThanRed <= ratioOfMatchBlueLedInGold] == 1))
  timesBlueTeamLoseForRatio[i] <- length(which(matches_data$bResult[matches_data$ratioBlueMoreGoldThanRed > (ratioOfMatchBlueLedInGold - 0.1) & matches_data$ratioBlueMoreGoldThanRed <= ratioOfMatchBlueLedInGold] == 0))
}
