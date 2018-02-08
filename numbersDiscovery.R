library(ggplot2)
library(stringr)

setwd("H:\\KE 5107 Projects\\code\\KE5107_LeagueofLegends")
matches_data = read.csv("data/matches.csv")
game_elements <- c("Towers", "Inhibs", "Dragons", "Barons", "Heralds") 

# Prepends r to the item
prependRed <- function(p){
  paste("r", p, sep="")
}

# Prepends b to the item
prependBlue <- function(p){
  paste("b", p, sep="")
}

# Get the Kills columns for blue and red teams
blue_kills_columns <- lapply(game_elements, prependBlue)
red_kills_columns <- lapply(game_elements, prependRed)

# Combine the Kills columns
all_kills_columns <- c(blue_kills_columns, red_kills_columns)

for(i in 1:length(all_kills_columns))
{
  column_name <- unlist(all_kills_columns[i])
  output_column_name <- paste(column_name, "Num", sep="")
  
  # Each item is enclosed in its own square brackets. Note that there is an extra [ to contain all the elements which needs to be removed
  matches_data[,output_column_name] <- str_count(matches_data[,column_name], "\\[")-1
}

#head(matches_data)
write.csv(matches_data, "processed/matches.csv", row.names = FALSE)