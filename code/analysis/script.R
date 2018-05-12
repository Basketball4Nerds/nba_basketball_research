## add team possession column
# https://www.burntorangenation.com/2011/10/19/2464697/advanced-basketball-statistics-understanding-possession-estimation
# https://www.nbastuffer.com/analytics101/possession/
rs_games$posTeam <- rs_games$fgaTeam + 0.44 * rs_games$ftaTeam - rs_games$orebTeam + rs_games$tovTeam


teams <- get_teams_details(all_teams=TRUE)
get_nba_teams_ids(teams = c("Brooklyn Nets", "Denver Nuggets"))

x <- get_teams_year_by_year_stats()
tail(as.data.frame(x))
table(x$isNBAChampion)
table(x$ptsRank, x$isConferenceChampion)
names(x)

y <- get_teams_seasons_info(team_ids=1610612743)
head(y)
names(y)



z <- get_teams_seasons_shots(team_ids=1610612743)
head(as.data.frame(z))

get_teams_tables_data
get_teams_year_by_year_stats


t <- get_bref_players_seasons()
names(t)
head(t)

f <- get_game_logs(seasons=2015, result_types='player')
names(f)

agg <- rs_games %>%
  rename(season = slugSeason) %>%
  filter(minutesTeam==240) %>%  # exclude overtime games data
  group_by(season, cnf, o_cnf) %>%  # group by season
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
  data.frame()
