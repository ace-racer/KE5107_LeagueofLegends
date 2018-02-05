matches_data = read.csv("data/matches.csv")

# columnnames <- colnames(matches_data)
champs_with_counts <- table(matches_data$blueTopChamp)
champs_with_counts_df <- as.data.frame(champs_with_counts)


blue_champs <- champs_with_counts_df[order(champs_with_counts_df$Freq), ]
tail(blue_champs)


red_champs_with_counts <- table(matches_data$redTopChamp)
red_champs_with_counts_df <- as.data.frame(red_champs_with_counts)


red_champs <- red_champs_with_counts_df[order(red_champs_with_counts_df$Freq), ]
tail(red_champs)