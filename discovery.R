matches_data = read.csv("data/matches.csv")
setDefaults("source",echo=FALSE)

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

# Plot the gold difference as time progresses in the match
plotGoldDiffWithTimeInMatch <- function(matchNumber){
  golddiffNumericList <- getGoldDiffAsListOfNumbers(matchNumber)
  
  playLength <- length(golddiffNumericList)
  
  # get the maximum amount of gold diff in the match
  max(golddiffNumericList)
  time <- seq(1, playLength)
  
  # scatter plot of the gold difference with time in the match for first match
  plot(time, golddiffNumericList, xlab="Time in the game", ylab="Gold difference")
}


for (matchNumber in 1:nrow(matches_data)) {
  golddiffNumericList <- getGoldDiffAsListOfNumbers(matchNumber)
  timesBlueWithMoreGoldThanRed <- length(which(golddiffNumericList > 0))
  matchLength <- length(golddiffNumericList)
  
  # fraction of the game where blue team was leading in gold
  matches_data$ratioBlueMoreGoldThanRed[matchNumber] <- timesBlueWithMoreGoldThanRed/matchLength
}


# The number of times the blue team won when the fraction of the game the blue team led in gold > 0.5
timesBlueTeamWonAndLedInGold <- length(which(matches_data$bResult[matches_data$ratioBlueMoreGoldThanRed > 0.5] == 1))

# total games blue team won
timesBlueTeamWon <- length(matches_data$bResult[matches_data$bResult == 1])

probabilityBlueTeamWonWhenLeadingGold <- timesBlueTeamWonAndLedInGold/timesBlueTeamWon

print(paste("P(Blue team win and leads gold possession for majority of game)", probabilityBlueTeamWonWhenLeadingGold))

# Outcome: 80% chance that blue team will win when they lead in possession of gold for majority of the match

# The number of times the red team won when the fraction of the game the red team led in gold > 0.5
timesRedTeamWonAndLedInGold <- length(which(matches_data$bResult[matches_data$ratioBlueMoreGoldThanRed < 0.5] == 0))

# total games red team won
timesRedTeamWon <- length(matches_data$bResult[matches_data$rResult == 1])

probabilityRedTeamWonWhenLeadingGold <- timesRedTeamWonAndLedInGold/timesRedTeamWon


print(paste("P(Red team win and leads gold possession for majority of game)", probabilityRedTeamWonWhenLeadingGold))
# Outcome: 82% chance that red team will win when they lead in possession of gold for majority of the match

#plotGoldDiffWithTimeInMatch(1)
#plotGoldDiffWithTimeInMatch(234)
#plotGoldDiffWithTimeInMatch(3)
