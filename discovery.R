


matches_data = read.csv("data/matches.csv")

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

plotGoldDiffWithTimeInMatch <- function(matchNumber){
  golddiffNumericList <- getGoldDiffAsListOfNumbers(matchNumber)
  
  playLength <- length(golddiffNumericList)
  
  # get the maximum amount of gold diff in the match
  max(golddiffNumericList)
  time <- seq(1, playLength)
  
  # scatter plot of the gold difference with time in the match for first match
  plot(time, golddiffNumericList, xlab="Time in the game", ylab="Gold difference")
}



#plotGoldDiffWithTimeInMatch(1)
plotGoldDiffWithTimeInMatch(234)
#plotGoldDiffWithTimeInMatch(3)
