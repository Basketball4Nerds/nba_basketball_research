## on average, how many players play per game?
t <- rs_player_gamelogs_2017 %>%
  group_by(slugTeam, dateGame) %>%
  summarize(n_players = n())
range(t$n_players)
table(t$n_players)
hist(t$n_players)



#### take 1

## find top scorers
top_scorers_df <- rs_player_gamelogs_2017 %>% 
  group_by(slugTeam, namePlayer) %>%
  summarize(mean_pts = mean(pts)) %>%
  arrange(-mean_pts)
top_scorers_df


  
#### take 2

## from each team, find the top player that contribute the most in scoring
top_contributors_df <- 
  
  ## add column for team points by joining
  left_join(
    rs_player_gamelogs_2017,
    rs_team_gamelogs_2017[ , c('idGame', 'slugTeam', 'slugOpponent', 'ptsTeam')],
    by = c('idGame', 'slugTeam', 'slugOpponent')
  ) %>% 
  
  ## calculate player's share of team points scored
  mutate(
    pctPts = round(pts / ptsTeam, 3)
  ) %>%
  
  ## calculate player's average share of team points scored
  group_by(slugTeam, namePlayer) %>%
  summarize(
    mean_pctPts = mean(pctPts),
    median_pctPts = median(pctPts)
  ) %>% 
  
  ## select top contributors from each team
  group_by(slugTeam) %>%
  top_n(n=1, wt=mean_pctPts) %>%

  ## order by share percentage  
  arrange(-mean_pctPts) %>%
  as.data.frame()


## view
head(top_contributors_df)
tail(top_contributors_df)

## distribution of top contributors' share percentages
quantile(top_contributors_df$mean_pctPts)




#### take 3 

## teams where scoring contributions are most skewed and least skewed
## teams with the biggest and smallest average gap in contribution share between #1 and #2 contributors
contribution_skew_df <- 
  
  ## add column for team points by joining
  left_join(
    rs_player_gamelogs_2017,
    rs_team_gamelogs_2017[ , c('idGame', 'slugTeam', 'slugOpponent', 'ptsTeam')],
    by = c('idGame', 'slugTeam', 'slugOpponent')
  ) %>% 
  
  ## calculate player's share of team points scored
  mutate(
    pctPts = round(pts / ptsTeam, 3)
  ) %>%

  ## for each game, select top 5 contributors' performances
  group_by(slugTeam, dateGame) %>%
  top_n(n=5, wt=pctPts) %>%
  summarize(
    skew = skew(pctPts),
    var = var(pctPts),
    top2_share_diff = rev(sort(pctPts))[1] - rev(sort(pctPts))[2]
  ) %>%
  
  ## for each team
  group_by(slugTeam) %>%
  
  ## find average skew and average gap (between #1 and #2 contributors)
  summarize(
    mean_skew = mean(skew),
    median_skew = median(skew),
    mean_top2_share_diff = mean(top2_share_diff),
    median_top2_share_diff = median(top2_share_diff)
  ) 

head(contribution_skew_df)
View(contribution_skew_df)

head(team_agg)
x <- merge(team_agg, contribution_skew_df, by='slugTeam')
cor(x$pts_sd, x$median_skew)
cor(x$pts_sd, x$median_top2_share_diff)


ggplot(x, aes(x=pts_sd, y=median_skew)) + 
  geom_point() + 
  geom_smooth(method='lm')
