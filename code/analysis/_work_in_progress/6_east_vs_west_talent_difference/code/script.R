## clear environment
rm(list = ls())


## load libraries
library(nbastatR)
library(ggplot2)
library(gridExtra)
library(psych)
library(plyr)
library(dplyr)
library(BBmisc)
library(reshape2)
library(tidyr)  # tidyr is successor to reshape2
library(data.table)  # for setDT()
library(knitr)
library(kableExtra)


## load data
rs_games <- get_game_logs(seasons=1980:2017, result_types='team', season_types='Regular Season')


## get team ids
team_ids <- unique(rs_games$idTeam)
x <- get_teams_seasons_info(team_ids=team_ids)
head(x)

## 
agg <- 
  rs_games %>% 
  rename(
    season = slugSeason,
    team = slugTeam,
    outcome = outcomeGame
  ) %>%
  group_by(season, team) %>%
  summarize(
    w_vs_e = sum(Out)
  )
table(rs_games$outcomeGame)










# Library
library(fmsb)

# Create data: note in High school for several students
set.seed(99)
data=as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data)=c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data)=paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(20,5) , rep(0,5) , data)
data

#==================
# Plot 1: Default radar chart proposed by the library:
radarchart(data)
?radarchart

#==================
# Plot 2: Same plot with custom features
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
?radarchart
radarchart( data , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)
legend(x=0.9, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)



#=================
# Plot3: If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( data[-c(1,2),]  , axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


