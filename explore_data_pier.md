---
title: "explore_data_pier"
author: "pier"
date: "10/02/2018"
output:
  html_document:
    keep_md: true
---





```r
rm(list=ls()) # cleanup
matches <- read.csv("/Users/pierlim/R_Projects/League_Of_Legends/matches.csv")
bluewin <- subset(matches, matches$bResult==1)
redwin <- subset(matches, matches$rResult==1)
```

**See blue top winning teams ** 

![](explore_data_pier_files/figure-html/top_winning_blue_teams-1.png)<!-- -->

See top winning red teams.

![](explore_data_pier_files/figure-html/top_winning_red_teams-1.png)<!-- -->

Okay, Blue Team SKT clearly has the most wins, regardless of whether they are blue or red. 
Let's take out all the games where SKT won, and analyze this particular team to see what is their magic. 

** Analyzing the Top Team ** 


```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](explore_data_pier_files/figure-html/analyze_top_team-1.png)<!-- -->

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](explore_data_pier_files/figure-html/analyze_top_team-2.png)<!-- -->

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](explore_data_pier_files/figure-html/analyze_top_team-3.png)<!-- -->

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](explore_data_pier_files/figure-html/analyze_top_team-4.png)<!-- -->

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](explore_data_pier_files/figure-html/analyze_top_team-5.png)<!-- -->

It's very interesting - it's not so much the champions, but the players themselves play the same position almost all of the time! They are highly specialized.

For SKT team, the most frequent player at each position is:

MaRin/Duke at Top position
Bang at ADC position (Bang is the only one who sometimes plays at Jungle position)
Bengi/Blank/Peanut at Jungle position
Wolf at Support position
Faker at Middle position

We can go one step further to see which are the most frequent combos based on this data, if required. 



```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](explore_data_pier_files/figure-html/analyze_top_champs-1.png)<!-- -->

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](explore_data_pier_files/figure-html/analyze_top_champs-2.png)<!-- -->

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](explore_data_pier_files/figure-html/analyze_top_champs-3.png)<!-- -->

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](explore_data_pier_files/figure-html/analyze_top_champs-4.png)<!-- -->

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](explore_data_pier_files/figure-html/analyze_top_champs-5.png)<!-- -->

For Champions, there definitely are favourite champions for each position, but as you can see it is more varied. 


