
rm(list=ls())
matches = read.csv('C:\\Users\\pierl\\Dropbox\\Machine_Learning\\KE5107\\processed\\matches.csv')
matches$YearSeason <- ifelse(matches$Season == "Spring", paste("01/01/", as.character(matches$Year), sep=""), paste("01/06/", as.character(matches$Year), sep=""))

library(data.table)
library(ggplot2)
# Total matches played by each team (consider them as blue or red)
blueTeams = data.frame(table(matches$blueTeamTag))
redTeams = data.frame(table(matches$redTeamTag))
names(blueTeams)[names(blueTeams) == 'Var1'] <- 'Team'
names(redTeams)[names(redTeams) == 'Var1'] <- 'Team'
combinedPlayed <- merge(blueTeams, redTeams, by="Team", all=TRUE)
setnames(combinedPlayed, "Freq.y", "rMatchesPlayed")
setnames(combinedPlayed, "Freq.x", "bMatchesPlayed")
combinedPlayed$totalPlayed <- combinedPlayed$bMatchesPlayed + combinedPlayed$rMatchesPlayed

# Get each team wins, whether they won as blue or red
bluewin <- subset(matches, matches$bResult==1)
redwin <- subset(matches, matches$rResult==1)
blueTeamWin = data.frame(table(bluewin$blueTeamTag))
redTeamWin = data.frame(table(redwin$redTeamTag))
names(blueTeamWin)[names(blueTeamWin) == 'Var1'] <- 'Team'
names(redTeamWin)[names(redTeamWin) == 'Var1'] <- 'Team'
combinedWins <- merge(blueTeamWin, redTeamWin, by="Team", all=TRUE)
setnames(combinedWins, "Freq.y", "rMatchesWon")
setnames(combinedWins, "Freq.x", "bMatchesWon")
combinedWins$totalWon <- combinedWins$bMatchesWon + combinedWins$rMatchesWon

# Merge and calc the win efficiency of each team
combinedTeams <- merge(combinedWins, combinedPlayed, by="Team")
combinedTeams$winEfficiency = combinedTeams$totalWon / combinedTeams$totalPlayed
combinedTeams[is.na(combinedTeams)] <- 0 

# Plotting top 20 just for visualization
top20efficient <- (combinedTeams[with(combinedTeams, order(-combinedTeams$winEfficiency)), ])[1:20, ]
ggplot(top20efficient, aes(x=Team, y=winEfficiency)) + geom_bar(stat="identity") +ggtitle("Top 20 Teams Most Efficient At Winning") + theme(plot.title = element_text(hjust = 0.5))

movetolast <- function(data, move) {
  data[c(setdiff(names(data), move), move)]
}

combinedTeams_blue = combinedTeams[,-c(2:7)] # remove unecessary columns
setnames(combinedTeams_blue, "winEfficiency", "bWinEfficiency")
setnames(combinedTeams_blue, "Team", "blueTeamTag")
matches <- merge(combinedTeams_blue, matches, by="blueTeamTag")

combinedTeams_red = combinedTeams[,-c(2:7)] # remove unecessary columns
setnames(combinedTeams_red, "winEfficiency", "rWinEfficiency")
setnames(combinedTeams_red, "Team", "redTeamTag")
matches <- merge(combinedTeams_red, matches, by="redTeamTag")

matches <- (matches[with(matches, order(matches$League)), ])
head(matches)
movetolast(matches, c("bWinEfficiency", "rWinEfficiency"))
#write.csv(matches, file = "matches_processed.csv")

library(plyr)

# add new columns to matches
matches$isBluePreferredLineup <- 0
matches$isRedPreferredLineup <- 0

for (row  in 1:nrow(combinedPlayed)) {
  print(combinedPlayed[row,])
  Team <- combinedPlayed[row, 1]
  if (is.na(Team)) next 
  
  # Get all instances of when this team wins (blue/red)
  blueWins <- subset(matches, (matches$blueTeamTag==Team & matches$bResult==1))
  redWins <- subset(matches, (matches$redTeamTag==Team & matches$rResult==1))
  
  # In all the blueWins for this team, get the most popular player for each pos
  btop <- names (sort(table(blueWins$blueTop), decreasing=TRUE)[1] )
  badc <- names (sort(table(blueWins$blueADC), decreasing=TRUE)[1] )
  bjungle <- names (sort(table(blueWins$blueJungle), decreasing=TRUE)[1])
  bsupport <- names (sort(table(blueWins$blueSupport), decreasing=TRUE)[1])
  bmiddle <- names (sort(table(blueWins$blueMiddle), decreasing=TRUE)[1])
  
  # In all the redWins for this team, get the most popular player for each pos
  rtop <- names (sort(table(redWins$redTop), decreasing=TRUE)[1] )
  radc <- names (sort(table(redWins$redADC), decreasing=TRUE)[1] )
  rjungle <- names (sort(table(redWins$redJungle), decreasing=TRUE)[1])
  rsupport <- names (sort(table(redWins$redSupport), decreasing=TRUE)[1])
  rmiddle <- names (sort(table(redWins$redMiddle), decreasing=TRUE)[1])
  
  # Mark all those matches that fit this particular combi for this team as having the preferred line-up
  matches$isBluePreferredLineup <- ifelse((matches$blueTop==btop & matches$blueADC==badc & matches$blueJungle==bjungle & matches$blueSupport==bsupport & matches$blueMiddle==bmiddle), 1, matches$isBluePreferredLineup)
  matches$isRedPreferredLineup <- ifelse((matches$redTop==rtop & matches$redADC==radc & matches$redJungle==rjungle & matches$redSupport==rsupport & matches$redMiddle==rmiddle), 1, matches$isRedPreferredLineup)
}
