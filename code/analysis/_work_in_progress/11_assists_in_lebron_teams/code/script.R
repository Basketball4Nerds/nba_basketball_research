## clear environment
remove(list = ls())


## loading libraries
library(ggplot2)
library(gridExtra)
library(psych)
library(dplyr)
library(reshape2)
library(data.table)
library(BBmisc)
library(knitr)
library(nbastatR)
#detach('package:plyr', unload=TRUE)


## load data
games <- read.csv("../../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)

## light preprocessing
games$outcome <- ifelse(games$won, 'won', 'lost')
games$site <- ifelse(games$site=='H', 'home', 'away')
games$team_type <- ifelse(games$line < 0, 'favorite',
                          ifelse(games$line > 0, 'underdog', NA))
games$ast_per_FGM <- games$ast / games$FGM
games$astA_per_FGMA <- games$astA / games$FGMA


## remove faulty rows
table(games$ast_per_FGM > 1 | games$astA_per_FGMA > 1)
games <- subset(games, games$ast_per_FGM <= 1 & games$astA_per_FGMA <= 1)

## subset regular season games
rs_games <- subset(games, playoffs==0)
ps_games <- subset(games, playoffs==1)



####

## get lebron team/season games
lebron_df <- subset(rs_games, 
                      (season %in% 2003:2009 & team=='Cavaliers') |
                      (season %in% 2010:2013 & team=='Heat') |
                      (season %in% 2014:2016 & team=='Cavaliers'))

## get lebron team's aggregate performance by season
lebron_team_agg <- lebron_df %>%
  group_by(season) %>%
  summarize(
    team = team[1],
    team_group = 'LeBron Team',
    avg_ast_per_game = mean(ast)
  )


## plot lebron team's assist-per-game performance by season
ggplot(lebron_team_agg) + 
  geom_point(aes(x=season, y=avg_ast_per_game, color=team)) + 
  geom_line(aes(x=season, y=avg_ast_per_game), linetype='dotted') + 
  scale_y_continuous(limits=c(0, 30)) + 
  ylab('assists') + 
  ggtitle("LeBron Team Performance by Season: Average Assists Per Game")


## get overall assist-per-game performance aggregation
overall_agg <- games %>% 
  group_by(team, season) %>%
  summarize(
    avg_ast_per_game = mean(ast),
    avg_ast_per_FGM = sum(ast) / sum(FGM)
  ) 


## get teams with the highest assist performance
top_team_agg <- overall_agg %>%
  group_by(season) %>%
  summarize(
    team = team[which.max(avg_ast_per_game)],
    team_group = 'Top-Performing Team',
    avg_ast_per_game = max(avg_ast_per_game)
  ) 

## get teams with the lowest assist performance
bottom_team_agg <- overall_agg %>%
  group_by(season) %>%
  summarize(
    team = team[which.min(avg_ast_per_game)],
    team_group = 'Bottom-Performing Team',
    avg_ast_per_game = min(avg_ast_per_game)
  ) 


## plot top teams' performance by season
ggplot(top_team_agg) + 
  geom_point(aes(x=season, y=avg_ast_per_game, color=team)) + 
  geom_line(aes(x=season, y=avg_ast_per_game), linetype='dotted') +
  scale_y_continuous(limits=c(0, 40)) + 
  ylab('assists') 


## plot bottom teams' performance by season
ggplot(bottom_team_agg) + 
  geom_point(aes(x=season, y=avg_ast_per_game)) + 
  geom_line(aes(x=season, y=avg_ast_per_game), linetype='dotted') +
  scale_y_continuous(limits=c(0, 40)) + 
  ylab('assists')


## put things in perspective: combined dfs
combined_agg <- rbind.data.frame(lebron_team_agg, 
                                     subset(top_team_agg, season >= 2003),
                                     subset(bottom_team_agg, season >= 2003))

## reordering label levels
combined_agg$team_group <- factor(combined_agg$team_group, 
                                      levels=c('Top-Performing Team',
                                               'LeBron Team',
                                               'Bottom-Performing Team'))

## plot top-performing vs. bottom-performing vs. lebron team
ggplot(combined_agg) + 
  geom_point(aes(x=season, y=avg_ast_per_game, color=team_group)) +
  geom_line(aes(x=season, y=avg_ast_per_game, group=team_group)) + 
  ylab('assists per game') + 
  scale_y_continuous(limits=c(0, 35)) + 
  guides(color=guide_legend(title="team group")) + 
  ggtitle('Average Assists Per Game by Season')
  


##### using assists-per-FGM metric instead of assists

## lebron team's performance
lebron_team_agg2 <- lebron_df %>%
  group_by(season) %>%
  summarize(
    team = team[1],
    team_group = 'LeBron Team',
    avg_ast_per_FGM = sum(ast) / sum(FGM)
  )

## top-performing team
top_team_agg2 <- overall_agg %>%
  group_by(season) %>%
  summarize(
    team = team[which.max(avg_ast_per_FGM)],
    team_group = 'Top-Performing Team',
    avg_ast_per_FGM = max(avg_ast_per_FGM)
  ) 

## bottom-performing team
bottom_team_agg2 <- overall_agg %>%
  group_by(season) %>%
  summarize(
    team = team[which.min(avg_ast_per_FGM)],
    team_group = 'Bottom-Performing Team',
    avg_ast_per_FGM = min(avg_ast_per_FGM)
  ) 

## put things in perspective: combined dfs
combined_agg2 <- rbind.data.frame(lebron_team_agg2, 
                                 subset(top_team_agg2, season >= 2003),
                                 subset(bottom_team_agg2, season >= 2003))

## reordering label levels
combined_agg2$team_group <- factor(combined_agg2$team_group, 
                                  levels=c('Top-Performing Team',
                                           'LeBron Team',
                                           'Bottom-Performing Team'))

## plot top-performing vs. bottom-performing vs. lebron team
ggplot(combined_agg2) + 
  geom_point(aes(x=season, y=avg_ast_per_FGM, color=team_group)) +
  geom_line(aes(x=season, y=avg_ast_per_FGM, group=team_group)) + 
  ylab('assist per FGM') + 
  scale_y_continuous(limits=c(0, 1)) + 
  guides(color=guide_legend(title="team group")) + 
  ggtitle('Average Assist Per Field Goal Made by Season')



####

## get champions' games (https://www.ticketcity.com/nba/nba-finals-schedule/nba-finals-champions.html)
champs_df <- subset(games,
                      (season %in% 1995 & team=='Bulls') |
                      (season %in% 1996 & team=='Bulls') |
                      (season %in% 1997 & team=='Bulls') |
                      (season %in% 1998 & team=='Spurs') |
                      (season %in% 1999 & team=='Lakers') |
                      (season %in% 2000 & team=='Lakers') |
                      (season %in% 2001 & team=='Lakers') |
                      (season %in% 2002 & team=='Spurs') |
                      (season %in% 2003 & team=='Pistons') |
                      (season %in% 2004 & team=='Spurs') |
                      (season %in% 2005 & team=='Heat') |
                      (season %in% 2006 & team=='Spurs') |
                      (season %in% 2007 & team=='Celtics') |
                      (season %in% 2008 & team=='Lakers') |
                      (season %in% 2009 & team=='Lakers') |
                      (season %in% 2010 & team=='Mavericks') |
                      (season %in% 2011 & team=='Heat') |
                      (season %in% 2011 & team=='Heat') |
                      (season %in% 2013 & team=='Spurs') |
                      (season %in% 2014 & team=='Warriors') |
                      (season %in% 2015 & team=='Cavaliers') | 
                      (season %in% 2016 & team=='Warriors'))


lebron_df <- subset(games, 
                    (season %in% 2003:2009 & team=='Cavaliers') |
                      (season %in% 2010:2013 & team=='Heat') |
                      (season %in% 2014:2016 & team=='Cavaliers'))



tapply(games$ast_per_FGM, list(games$outcome, games$site), mean)
ggplot(games) +
  geom_density(aes(x=ast_per_FGM, color=outcome, fill=outcome), alpha=0.2)


x <- games[1:5, ]
plot(hclust(dist(x)))
