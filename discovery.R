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

timesBlueTeamWonForRatio = list()
timesBlueTeamLoseForRatio = list()

for (i in 1:10){
  ratioOfMatchBlueLedInGold <- 0.1 * i
  timesBlueTeamWonForRatio[i] <- length(which(matches_data$bResult[matches_data$ratioBlueMoreGoldThanRed > (ratioOfMatchBlueLedInGold - 0.1) & matches_data$ratioBlueMoreGoldThanRed <= ratioOfMatchBlueLedInGold] == 1))
  timesBlueTeamLoseForRatio[i] <- length(which(matches_data$bResult[matches_data$ratioBlueMoreGoldThanRed > (ratioOfMatchBlueLedInGold - 0.1) & matches_data$ratioBlueMoreGoldThanRed <= ratioOfMatchBlueLedInGold] == 0))
}

# scatter plot of the 2 different values
xValues <- seq(10, 100, by = 10)
plot(xValues, timesBlueTeamWonForRatio, type="p", main="Advantage in gold possession with winning", xlab="Percent of total match Blue team had more gold than Red team", ylab="Number of matches won", col="blue")
par(new = TRUE)
plot(xValues, timesBlueTeamLoseForRatio, type="p", col="red", axes = FALSE, xlab="", ylab="")
axis(1, at = xValues)
legend("top", legend = c("Matches won by Blue team", "Matches won by Red team"), col = c("blue", "red"), cex = 0.8, pch=1)
