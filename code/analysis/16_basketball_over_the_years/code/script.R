## clear environment
rm(list=ls())


## load libraries
library(knitr)
library(dplyr)
library(zoo)
library(BBmisc)
library(Metrics)
library(ggplot2)
library(gridExtra)
library(ggthemes) 
library(nbastatR)
source('../../../helper/general/fill_in_opp_cols.R')


## load data
rs_games <- get_game_logs(seasons=1980:2017, result_types='team', season_types='Regular Season')


## add team possession column
# https://www.burntorangenation.com/2011/10/19/2464697/advanced-basketball-statistics-understanding-possession-estimation
# https://www.nbastuffer.com/analytics101/possession/
rs_games$posTeam <- rs_games$fgaTeam + 0.44 * rs_games$ftaTeam - rs_games$orebTeam + rs_games$tovTeam


## aggregate data
agg <- rs_games %>%
  #filter(minutesTeam==240) %>%  # exclude overtime games data
  group_by(slugSeason) %>%  # group by season
  summarize(
    
    avg_pts = mean(ptsTeam, na.rm=TRUE),
    avg_ast = mean(astTeam, na.rm=TRUE),
    avg_stl = mean(stlTeam, na.rm=TRUE),
    avg_blk = mean(blkTeam, na.rm=TRUE),
    avg_pf = mean(pfTeam, na.rm=TRUE),
    avg_tov = mean(tovTeam, na.rm=TRUE),
    
    ## field goal and free throw percentages
    avg_FGP = mean(pctFGTeam * 100, na.rm=TRUE),
    avg_FGP2x = mean(pctFG2Team * 100, na.rm=TRUE),
    avg_FGP3x = mean(pctFG3Team * 100, na.rm=TRUE),
    avg_FTP = mean(pctFTTeam * 100, na.rm=TRUE),
    
    ## share of 2-point and 3-point shot attempts
    avg_shrFGA2x = mean(fg2aTeam / fgaTeam * 100, na.rm=TRUE),
    avg_shrFGA3x = mean(fg3aTeam / fgaTeam * 100, na.rm=TRUE),

    ## estimated possessions
    avg_pos = mean(posTeam, na.rm=TRUE),
    avg_ppp = avg_pts / avg_pos
    
  ) %>%
  rename(
    season = slugSeason
  ) %>%
  data.frame()



#### plots: points, assists, blocks, steals, rebounds, turnovers

## avg team points per game over the years
a <- ggplot(agg) +
  geom_point(aes(x=season, y=avg_pts)) + 
  geom_line(aes(x=season, y=avg_pts, group=1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) + 
  ggtitle("Avg Regular-Quarter Team Points / Game") + 
  ylab('points / game') 


## avg team assists per game over the years
b <- ggplot(agg) +
  geom_point(aes(x=season, y=avg_ast)) + 
  geom_line(aes(x=season, y=avg_ast, group=1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) + 
  ggtitle("Avg Team Assists / Game") + 
  ylab('assists / game') 


## avg team steals per game over the years
c <- ggplot(agg) +
  geom_point(aes(x=season, y=avg_stl)) + 
  geom_line(aes(x=season, y=avg_stl, group=1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) + 
  ggtitle("Avg Team Steals / Game") + 
  ylab('steals / game') 


## avg team blocks per game over the years
d <- ggplot(agg) +
  geom_point(aes(x=season, y=avg_blk)) + 
  geom_line(aes(x=season, y=avg_blk, group=1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) + 
  ggtitle("Avg Team Blocks / Game") + 
  ylab('blocks / game') 


## avg team personal fouls per game over the years
e <- ggplot(agg) +
  geom_point(aes(x=season, y=avg_pf)) + 
  geom_line(aes(x=season, y=avg_pf, group=1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) + 
  ggtitle("Avg Team Personal Fouls / Game") + 
  ylab('fouls / game') 


## avg team turnovers per game over the years
f <- ggplot(agg) +
  geom_point(aes(x=season, y=avg_tov)) + 
  geom_line(aes(x=season, y=avg_tov, group=1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) + 
  ggtitle("Avg Team Turnovers / Game") + 
  ylab('turnovers / game') 

## avg team possessions per game over the years
g <- ggplot(agg) +
  geom_point(aes(x=season, y=avg_pos)) + 
  geom_line(aes(x=season, y=avg_pos, group=1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) + 
  ggtitle("Avg Team Possessions / Game") + 
  ylab('possessions / game') 

## avg team points per possessions over the years
l <- ggplot(agg) +
  geom_point(aes(x=season, y=avg_ppp)) + 
  geom_line(aes(x=season, y=avg_ppp, group=1)) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) +
  ggtitle("Avg Team Points / Possessions") + 
  ylab('points / possession') 






#### plots: field goal and free throw percentages

## avg FGP over the years
h <- ggplot(agg) +
  geom_point(aes(x=season, y=avg_FGP)) + 
  geom_line(aes(x=season, y=avg_FGP, group=1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) + 
  ggtitle("Avg Team Field Goal Percentage") + 
  ylab('field goal percentage') 


## avg 2-pointer FGP over the years
i <- ggplot(agg) +
  geom_point(aes(x=season, y=avg_FGP2x)) + 
  geom_line(aes(x=season, y=avg_FGP2x, group=1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) + 
  ggtitle("Avg Team 2-Pointer Field Goal Percentage") + 
  ylab('2-pointer FGP') 


## avg 3-pointer FGP over the years
j <- ggplot(agg) +
  geom_point(aes(x=season, y=avg_FGP3x)) + 
  geom_line(aes(x=season, y=avg_FGP3x, group=1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) + 
  ggtitle("Avg Team 3-Pointer Field Goal Percentage") + 
  ylab('3-pointer FGP') 


## avg FTP over the years
k <- ggplot(agg) +
  geom_point(aes(x=season, y=avg_FTP)) + 
  geom_line(aes(x=season, y=avg_FTP, group=1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) + 
  ggtitle("Avg Team Free Throw Percentage") + 
  ylab('free throw percentage') 




####

## all together
grid.arrange(a, b, c, d, e, f, g, l)
grid.arrange(a, g, h, l)
grid.arrange(h, i, j, k)


## correlation matrix

cor_mtx <- round(cor(rs_games[ , c('ptsTeam', 'posTeam', 'pctFGTeam')], use='pairwise.complete.obs'), 3)
cor_mtx[upper.tri(cor_mtx)] <- ""
cor_mtx <- as.data.frame(cor_mtx)
kable(cor_mtx, 'html')

cor_mtx2 <- round(cor(agg[ , c('avg_pts', 'avg_pos', 'avg_FGP')], use='pairwise.complete.obs'), 3)
cor_mtx2[upper.tri(cor_mtx2)] <- ""
cor_mtx2 <- as.data.frame(cor_mtx2)
kable(cor_mtx2, 'html')





## share of 2-point and 3-point shot attempts
ggplot(agg) +
  geom_point(aes(x=season, y=avg_shrFGA2x, color='2-pointer share')) + 
  geom_line(aes(x=season, y=avg_shrFGA2x, group=1, color='2-pointer share')) + 
  geom_point(aes(x=season, y=avg_shrFGA3x, color='3-pointer share')) + 
  geom_line(aes(x=season, y=avg_shrFGA3x, group=1, color='3-pointer share')) +   
  scale_colour_manual(name="Line Color",
                      values=c('2-pointer share'="red", 
                               '3-pointer share'="blue")) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)]) + 
  ggtitle("Avg Team Share of 2-Point and 3-Point Shot Attempts") + 
  ylab('share (%)') 


