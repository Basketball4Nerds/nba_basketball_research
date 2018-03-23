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


## load data
rs_games <- get_game_logs(seasons=2004:2017,  # actually 2003 - 2016 data
                          result_type='team', 
                          season_types='Regular Season')
ps_games <- get_game_logs(seasons=2004:2017,  # actually 2013 - 2016 data
                          result_type='team', 
                          season_types='Playoffs')
games <- rbind.data.frame(rs_games, ps_games)


## light preprocessing
games$hasVideo <- games$urlTeamSeasonLogo <- NULL
names(games) <- gsub('^yearSeason$', 'season', names(games))
names(games) <- gsub('^typeSeason$', 'season_type', names(games))
names(games) <- gsub('^dateGame$', 'date', names(games))
names(games) <- gsub('^idGame$', 'game_id', names(games))
names(games) <- gsub('^numberGameTeamSeason$', 'n', names(games))
names(games) <- gsub('^nameTeam$', 'team', names(games))
names(games) <- gsub('^locationGame$', 'site', names(games))
names(games) <- gsub('^countDaysRestTeam$', 'rst', names(games))
names(games) <- gsub('^outcomeGame$', 'game', names(games))
names(games) <- gsub('Team', '', names(games))
names(games) <- gsub('^fgm$', 'FGM', names(games))
names(games) <- gsub('^fga$', 'FGA', names(games))
names(games) <- gsub('^pctFG$', 'FGP', names(games))
names(games) <- gsub('^fg2m$', 'FGM2x', names(games))
names(games) <- gsub('^fg2a$', 'FGA2x', names(games))
names(games) <- gsub('^pctFG2$', 'FGP2x', names(games))
names(games) <- gsub('^fg3m$', 'FGM3x', names(games))
names(games) <- gsub('^fg3a$', 'FGA3x', names(games))
names(games) <- gsub('^pctFG3$', 'FGP3x', names(games))
names(games) <- gsub('^pctFT$', 'FTP', names(games))
names(games) <- gsub('^ftm$', 'FTM', names(games))
names(games) <- gsub('^fta$', 'FTA', names(games))
names(games) <- gsub('^oreb$', 'oRb', names(games))
names(games) <- gsub('^dreb$', 'dRb', names(games))
names(games) <- gsub('^treb$', 'rb', names(games))
names(games) <- gsub('^pts$', 'p', names(games))

games$ast_per_FGM <- games$ast / games$FGM
games$season <- games$season - 1



## get lebron team/season games
lebron_df <- subset(games, 
                      season_type=='Regular Season' & 
                      (season %in% 2003:2009 & team=='Cleveland Cavaliers') |
                      (season %in% 2010:2013 & team=='Miami Heat') |
                      (season %in% 2014:2016 & team=='Cleveland Cavaliers'))


## get lebron team's aggregate performance by season
lebron_team_agg <- lebron_df %>%
  group_by(season, slugSeason) %>%
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
  ggtitle("LeBron's Team Regular Season Average Assists Per Game By Season")


## get overall assist-per-game performance aggregation
overall_agg <- games %>% 
  group_by(team, season, slugSeason) %>%
  summarize(
    avg_ast_per_game = mean(ast),
    avg_ast_per_FGM = sum(ast) / sum(FGM)
  ) 


## get teams with the highest assist performance
top_team_agg <- overall_agg %>%
  group_by(season, slugSeason) %>%
  summarize(
    team = team[which.max(avg_ast_per_game)],
    team_group = 'Top-Performing Team',
    avg_ast_per_game = max(avg_ast_per_game)
  ) 

## get teams with the lowest assist performance
bottom_team_agg <- overall_agg %>%
  group_by(season, slugSeason) %>%
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
  geom_line(aes(x=season, y=avg_ast_per_game, group=team_group), linetype='dotted') + 
  ylab('assists per game') + 
  scale_y_continuous(limits=c(0, 35)) + 
  guides(color=guide_legend(title="team group")) + 
  ggtitle('Average Assists Per Game by Season')



##### using assists-per-FGM metric instead of assists

## lebron team's performance
lebron_team_agg2 <- lebron_df %>%
  group_by(season, slugSeason) %>%
  summarize(
    team = team[1],
    team_group = 'LeBron Team',
    avg_ast_per_FGM = sum(ast) / sum(FGM)
  )

## top-performing team
top_team_agg2 <- overall_agg %>%
  group_by(season, slugSeason) %>%
  summarize(
    team = team[which.max(avg_ast_per_FGM)],
    team_group = 'Top-Performing Team',
    avg_ast_per_FGM = max(avg_ast_per_FGM)
  ) 

## bottom-performing team
bottom_team_agg2 <- overall_agg %>%
  group_by(season, slugSeason) %>%
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
  geom_line(aes(x=season, y=avg_ast_per_FGM, group=team_group), linetype='dotted') + 
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

