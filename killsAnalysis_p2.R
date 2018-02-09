library(corrplot)

setwd("H:\\KE 5107 Projects\\code\\KE5107_LeagueofLegends")
matches_data = read.csv("processed/matches.csv")
game_elements <- c("Towers", "Inhibs", "Dragons", "Barons", "Heralds") 

# Get the column name for the red team
getRedColumnName <- function(p){
  paste("r", p, "Num", sep="")
}

# Get the column name for the blue team
getBlueColumnName <- function(p){
  paste("b", p, "Num", sep="")
}

# Get the number of Kills columns for blue and red teams
blue_num_kills_columns <- lapply(game_elements, getBlueColumnName)
red_num_kills_columns <- lapply(game_elements, getRedColumnName)

# Combine the Kills columns
all_num_kills_columns <- c(blue_num_kills_columns, red_num_kills_columns)

all_num_kills_columns_with_result <- c(all_num_kills_columns, "bResult")


# Get correlation between the number of kills and the result of the match
killscor <- cor(matches_data[,unlist(all_num_kills_columns_with_result)])
corrplot(killscor, method = "circle")

first_kill_columns_with_result <- c("TowersFirstKillBy", "InhibsFirstKillBy", "DragonsFirstKillBy", "bResult")

# Get correlation between the first kill and the result of the match
first_kills_correlation <- cor(matches_data[,unlist(first_kill_columns_with_result)])
corrplot(first_kills_correlation, method = "circle")

