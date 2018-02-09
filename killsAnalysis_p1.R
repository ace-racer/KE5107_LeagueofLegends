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

# Get the first number from a series of numbers
getFirstNumber <- function(value){
  if(value != "[]"){
    splits <- strsplit(value, ",")
    value <- sapply(splits, `[`, 1)
    
    number <- sub("\\[\\[", "", value)
    return(number)
  }
  
  # A high value if the first number is not present
  return(10000)
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

first_kill_elements <- c("Towers", "Inhibs", "Dragons") 
for(i in 1: length(first_kill_elements))
{
  column_name <- unlist(first_kill_elements[i])
  
  red_column_name <- paste("r", column_name,sep="")
  blue_column_name <- paste("b", column_name,sep="")
  
  print(red_column_name)
  print(blue_column_name)
  
  red_output_column_name <- paste("r", column_name, "FirstDestroyedTime",sep="")
  blue_output_column_name <- paste("b", column_name, "FirstDestroyedTime",sep="")
  
  matches_data[,blue_output_column_name] <- unlist(lapply(as.character(matches_data[,blue_column_name]), getFirstNumber))
  matches_data[,red_output_column_name] <- unlist(lapply(as.character(matches_data[,red_column_name]), getFirstNumber))
  
  # convert the first kill times to numeric for comparison
  matches_data[,blue_output_column_name] <- as.numeric(matches_data[,blue_output_column_name])
  matches_data[,red_output_column_name] <- as.numeric(matches_data[,red_output_column_name])
  
  first_kill_by_column_name <- paste(column_name, "FirstKillBy",sep="")
  
  matches_data[,first_kill_by_column_name] <- matches_data[,blue_output_column_name] < matches_data[,red_output_column_name]
}


#head(matches_data)
write.csv(matches_data, "processed/matches.csv", row.names = FALSE)
