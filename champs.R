matches_data = read.csv("data/matches.csv")

appendChamp <- function(p){
  paste(p, "Champ", sep="")
}

prependRed <- function(p){
  paste("red", p, sep="")
}

prependBlue <- function(p){
  paste("blue", p, sep="")
}

getTopChampsForPosition <- function(champNames, n, columnName){
  print(paste("Location: ", columnName))
  topChamps <- sort(table(champNames), decreasing = TRUE)
  topChampsAtPositions <- head(topChamps, n)
  print(topChampsAtPositions)
}

mapPositions <- c("Top", "Jungle", "Middle", "ADC", "Support")
mapPositions <- lapply(mapPositions, appendChamp)


blueChampPositions <- lapply(mapPositions, prependBlue)
redChampPositions <- lapply(mapPositions, prependRed)


allChampPositionColumnNames <- c(blueChampPositions, redChampPositions)

allChampsAtPositions <- matches_data[, unlist(allChampPositionColumnNames)]

for(i in 1:length(allChampPositionColumnNames))
{
  cname <- unlist(allChampPositionColumnNames[i])
  topChampsPosition <- getTopChampsForPosition(allChampsAtPositions[,cname], 5, cname)
}
