setwd("H:\\KE 5107 Projects\\association_analysis")

library(arules)

# Get the bans from the transactions
bans = read.transactions("output_bans_original.csv", format = "single", sep = ",", cols = c("Id", "Banned hero"))
itemFrequencyPlot(bans, support = 0.1, cex.names = 0.8)

# Get the sets 
fsets = eclat(bans, parameter = list(support = 0.1), control = list(verbose=FALSE))
summary(fsets)

# The single items which have support >= 0.1
singleItems = fsets[size(items(fsets)) == 1]
inspect(singleItems)

# Get the multi items which have support >= 0.1
multiItems = fsets[size(items(fsets)) > 1]
inspect(multiItems)

# Get the rules with support > 0.05
grules = apriori(bans, parameter = list(support = 0.05, confidence = 0.5))

inspect(sort(grules, by = "lift"))

# Get the quality of the rules
quality(grules)