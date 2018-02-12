
<h1>League of Legends data analysis</h1>


```R
lolraw=read.csv("LeagueofLegends.csv")
bluewins <-length(lolraw$bResult[lolraw$bResult=='1'])
redwins <-length(lolraw$bResult[lolraw$bResult=='0'])


```

We will plot Winning percentages for Blue and Red Teams


```R
# Simple Pie Chart
cols=c("green","red")
slices <- c(bluewins,redwins)
lbls <- c("BlueWins","RedWins")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices, labels = lbls, main="Winning Distribution",col=cols)
```


![png](output_3_0.png)



```R
blueteams <-table(lolraw$blueTeamTag[lolraw$bResult=='1'] )
bluewinframe <-data.frame(rbind(blueteams))
bluewinframe
```


<table>
<thead><tr><th></th><th scope=col>V1</th><th scope=col>X7h</th><th scope=col>A</th><th scope=col>AE</th><th scope=col>AFs</th><th scope=col>ahq</th><th scope=col>AHQ</th><th scope=col>ALL</th><th scope=col>ANC</th><th scope=col>ANX</th><th scope=col>...</th><th scope=col>VP</th><th scope=col>VS</th><th scope=col>WE</th><th scope=col>WFX</th><th scope=col>WS</th><th scope=col>X5</th><th scope=col>XG</th><th scope=col>yoeFW</th><th scope=col>ZONE</th><th scope=col>ZTG</th></tr></thead>
<tbody>
	<tr><th scope=row>blueteams</th><td>19 </td><td>18 </td><td>6  </td><td>15 </td><td>62 </td><td>75 </td><td>16 </td><td>3  </td><td>10 </td><td>15 </td><td>...</td><td>13 </td><td>16 </td><td>16 </td><td>5  </td><td>5  </td><td>1  </td><td>9  </td><td>6  </td><td>2  </td><td>12 </td></tr>
</tbody>
</table>




```R
redteams <-table(lolraw$redTeamTag[lolraw$rResult=='1'] )
redwinframe <-data.frame(rbind(redteams))
redwinframe
```


<table>
<thead><tr><th></th><th scope=col>V1</th><th scope=col>X7h</th><th scope=col>A</th><th scope=col>AE</th><th scope=col>AFs</th><th scope=col>ahq</th><th scope=col>AHQ</th><th scope=col>ALL</th><th scope=col>ANC</th><th scope=col>ANX</th><th scope=col>...</th><th scope=col>VP</th><th scope=col>VS</th><th scope=col>WE</th><th scope=col>WFX</th><th scope=col>WS</th><th scope=col>X5</th><th scope=col>XG</th><th scope=col>yoeFW</th><th scope=col>ZONE</th><th scope=col>ZTG</th></tr></thead>
<tbody>
	<tr><th scope=row>redteams</th><td>19 </td><td>15 </td><td>6  </td><td>12 </td><td>43 </td><td>67 </td><td>12 </td><td>0  </td><td>7  </td><td>8  </td><td>...</td><td>12 </td><td>19 </td><td>8  </td><td>2  </td><td>10 </td><td>1  </td><td>9  </td><td>6  </td><td>0  </td><td>6  </td></tr>
</tbody>
</table>




```R
library(dplyr)

# add rownames as a column in each data.frame and bind rows
bind_rows(bluewinframe %>% add_rownames(), 
          redwinframe %>% add_rownames()) %>% 
    # evaluate following calls for each value in the rowname column
    group_by(rowname) %>% 
    # add all non-grouping variables
    summarise_all(sum)
```

    Warning message:
    "Deprecated, use tibble::rownames_to_column() instead."Warning message:
    "Deprecated, use tibble::rownames_to_column() instead."


<table>
<thead><tr><th scope=col>rowname</th><th scope=col>V1</th><th scope=col>X7h</th><th scope=col>A</th><th scope=col>AE</th><th scope=col>AFs</th><th scope=col>ahq</th><th scope=col>AHQ</th><th scope=col>ALL</th><th scope=col>ANC</th><th scope=col>...</th><th scope=col>VP</th><th scope=col>VS</th><th scope=col>WE</th><th scope=col>WFX</th><th scope=col>WS</th><th scope=col>X5</th><th scope=col>XG</th><th scope=col>yoeFW</th><th scope=col>ZONE</th><th scope=col>ZTG</th></tr></thead>
<tbody>
	<tr><td>blueteams</td><td>19       </td><td>18       </td><td>6        </td><td>15       </td><td>62       </td><td>75       </td><td>16       </td><td>3        </td><td>10       </td><td>...      </td><td>13       </td><td>16       </td><td>16       </td><td>5        </td><td> 5       </td><td>1        </td><td>9        </td><td>6        </td><td>2        </td><td>12       </td></tr>
	<tr><td>redteams </td><td>19       </td><td>15       </td><td>6        </td><td>12       </td><td>43       </td><td>67       </td><td>12       </td><td>0        </td><td> 7       </td><td>...      </td><td>12       </td><td>19       </td><td> 8       </td><td>2        </td><td>10       </td><td>1        </td><td>9        </td><td>6        </td><td>0        </td><td> 6       </td></tr>
</tbody>
</table>




```R
alist <- list()
uniqueteams <-list(union(unique(lolraw$blueTeamTag),unique(lolraw$redTeamTag)))

    for (team in uniqueteams){
        if (((lolraw$blueTeamTag =team) & (lolraw$bResult=1)) | ((lolraw$redTeamTag =team) & (lolraw$rResult=1) ) )
               alist(team)
    }


```


    Error in `$<-.data.frame`(`*tmp*`, "blueTeamTag", value = c("TSM", "CST", : replacement has 186 rows, data has 7058
    Traceback:
    

    1. `$<-`(`*tmp*`, "blueTeamTag", value = c("TSM", "CST", "WFX", 
     . "TIP", "CLG", "DIG", "TL", "C9", "T8", "GV", "TDK", "NME", "REN", 
     . "FOX", "IMT", "NRG", "NV", "APX", "P1", "FLY", "EL", "GIA", "CW", 
     . "SK", "GMB", "MYM", "ROC", "FNC", "UOL", "H2K", "OG", "SPY", 
     . "VIT", "G2", "S04", "MSF", "NIP", "MM", "CJE", "JAG", "SKT", 
     . "GET", "NJE", "IM", "SSG", "kt", "ANC", "SBENU", "Longzhu", "SSB", 
     . "KOO", "LZ", "NJF", "CJ", "AFs", "EMF", "ROX", "KDM", "ESC", 
     . "MVP", "bbq", "EEW", "ahq", "Crew", "TPA", "LOG", "DLY", "MSE", 
     . "NGU", "DoR", "HKES", "yoeFW", "", "LGS", "as", "HKE", "FW", 
     . "Mac", "AS", "M17", "CGE", "XG", "JT", "TM", "FB", "WS", "RG", 
     . "HKA", "AHQ", "PNG", "ITZ", "CNB", "OPK", "VFK", "KBM", "RED", 
     . "g3x", "BGG", "KST", "REM", "TSW", "PRG", "ONE", "BJK", "TT", 
     . "A", "ZONE", "HWA", "NR1", "DP", "BPI", "OHM", "CREW", "SUP", 
     . "AUR", "CLK", "CRW", "GS", "GAL", "P3P", "INF", "DW", "LGC", 
     . "SIN", "TRI", "CHF", "HLN", "AV", "AE", "X5", "RGC", "ISG", "HAF", 
     . "FG", "RBT", "LK", "KLG", "B2K", "PDS", "IFG", "JTH", "LYN", 
     . "D9", "ZTG", "GG", "SCW", "7h", "USG", "DFM", "RPG", "CRJ", "BE", 
     . "SZ", "RJ", "BC", "HR", "DF", "VEG", "JST", "VS", "EMP", "ANX", 
     . "M19", "VP", "JSA", "EDG", "SSW", "SHR", "LMQ", "NWS", "OMG", 
     . "ALL", "IG", "BKT", "KT", "LGD", "RNG", "GAM", "WE", "QG", "H2k", 
     . "INTZ", "JTM", "GlA", "MSK", "ASC"))

    2. `$<-.data.frame`(`*tmp*`, "blueTeamTag", value = c("TSM", "CST", 
     . "WFX", "TIP", "CLG", "DIG", "TL", "C9", "T8", "GV", "TDK", "NME", 
     . "REN", "FOX", "IMT", "NRG", "NV", "APX", "P1", "FLY", "EL", "GIA", 
     . "CW", "SK", "GMB", "MYM", "ROC", "FNC", "UOL", "H2K", "OG", "SPY", 
     . "VIT", "G2", "S04", "MSF", "NIP", "MM", "CJE", "JAG", "SKT", 
     . "GET", "NJE", "IM", "SSG", "kt", "ANC", "SBENU", "Longzhu", "SSB", 
     . "KOO", "LZ", "NJF", "CJ", "AFs", "EMF", "ROX", "KDM", "ESC", 
     . "MVP", "bbq", "EEW", "ahq", "Crew", "TPA", "LOG", "DLY", "MSE", 
     . "NGU", "DoR", "HKES", "yoeFW", "", "LGS", "as", "HKE", "FW", 
     . "Mac", "AS", "M17", "CGE", "XG", "JT", "TM", "FB", "WS", "RG", 
     . "HKA", "AHQ", "PNG", "ITZ", "CNB", "OPK", "VFK", "KBM", "RED", 
     . "g3x", "BGG", "KST", "REM", "TSW", "PRG", "ONE", "BJK", "TT", 
     . "A", "ZONE", "HWA", "NR1", "DP", "BPI", "OHM", "CREW", "SUP", 
     . "AUR", "CLK", "CRW", "GS", "GAL", "P3P", "INF", "DW", "LGC", 
     . "SIN", "TRI", "CHF", "HLN", "AV", "AE", "X5", "RGC", "ISG", "HAF", 
     . "FG", "RBT", "LK", "KLG", "B2K", "PDS", "IFG", "JTH", "LYN", 
     . "D9", "ZTG", "GG", "SCW", "7h", "USG", "DFM", "RPG", "CRJ", "BE", 
     . "SZ", "RJ", "BC", "HR", "DF", "VEG", "JST", "VS", "EMP", "ANX", 
     . "M19", "VP", "JSA", "EDG", "SSW", "SHR", "LMQ", "NWS", "OMG", 
     . "ALL", "IG", "BKT", "KT", "LGD", "RNG", "GAM", "WE", "QG", "H2k", 
     . "INTZ", "JTM", "GlA", "MSK", "ASC"))

    3. stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
     .     "replacement has %d rows, data has %d"), N, nrows), domain = NA)


We will create column named as "winners" and all the winners


```R
#add ratio of number of wins /total matches
lolraw$bResult <- as.character(lolraw$bResult)
lolraw$blueTeamTag <- as.character(lolraw$blueTeamTag)
lolraw$redTeamTag <- as.character(lolraw$redTeamTag)

f <-ifelse(lolraw$bResult=='1',lolraw$blueTeamTag,lolraw$redTeamTag)
lolraw$winners <-f
#write.csv(lolraw,file="win.csv")

winfreq <-table(lolraw$winners)
winfreq_sorted = sort(winfreq, decreasing=TRUE)[1:10] # sorts and gets top 10
winfreq_sorted


# if (lolraw$bResult=='1'){
#     lolraw$winner <-lolraw$blueTeamTag
# }else{
#     lolraw$winner <-lolraw$redTeamTag
# }

#write.csv(lolraw,file="test.csv")

```


    
    SKT TSM  kt SSG  C9  FW CLG FNC H2K ahq 
    283 206 186 177 175 163 161 160 151 142 

