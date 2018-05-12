#### load regular season games
games <- read.csv("../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)
games$outcome <- ifelse(games$won, 'won', 'lost')
games$site <- ifelse(games$site=='H', 'home', 'away')



#### load libraries
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



#### 
mean(games$FGP2x)
mean(games$FGP3x, na.rm=TRUE)
table(is.na(games$FGP3x))
x <- subset(games, is.na(FGP3x))
x$FGM == x$FGM2x


0.48 * 2/3
