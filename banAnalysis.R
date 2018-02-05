matches_data = read.csv("data/matches.csv")


typeof(matches_data$blueBans)
matches_data$allBans <- paste(matches_data$blueBans, matches_data$redBans)
#head(matches_data$allBans)

# append all the bans
matches_data$allBans <- sub ("] \\[", ", ", matches_data$allBans)


champs <- list()
for (row_num in 1:2){
  items <- strsplit(matches_data[row_num, c("allBans")], ", ")
  #print(typeof(items))
  # print(items)
  print(items[1])
  print(items[2])
}