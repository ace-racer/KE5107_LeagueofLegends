matches_data = read.csv("data/matches.csv")

# get the characters from the first row of the matches data and the golddiff column
golddiffStr <- as.character(matches_data[1, c("golddiff")])

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

# get the maximum amount of gold diff in the match
max(golddiffNumericList)

#golddiffVal <- as.numeric(golddiffStr)

#golddiffVal
#max(as.numeric(golddiffStr))


#for(i in 1:50){
  #golddiffStr <- as.character(matches_data[i, c("golddiff")])
  #golddiffStr
  # golddiffs <- as.numeric(unlist(strsplit(golddiffStr[i], ',')))
  #golddiffs <- unlist(strsplit(golddiffStr, ','))
  #print(golddiffs)
  # matches_data$maxGoldDiff[i] = max(golddiffs)
#}

# matches_data$maxGoldDiff[1:50]
