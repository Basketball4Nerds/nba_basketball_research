-- Create the player_gamelogs table
drop table if exists player_gamelogs;
create table player_gamelogs(season_id TEXT, player_id INT, game_id INT, game_date TEXT, matchup TEXT, wl TEXT,
       min INT, fgm INT, fga INT, fg_pct DOUBLE PRECISION, fg3m INT, fg3a INT, fg3_pct DOUBLE PRECISION, ftm INT,
       fta INT ,ft_pct DOUBLE PRECISION, oreb INT, dreb INT, reb INT, ast INT, stl INT, blk INT, tov INT, pf INT,
       pts INT, plus_minus INT, vid_available INT, home TEXT, away TEXT);

-- run in psql command line
\copy player_gamelogs from '/Users/Dan/Desktop/nba4nerds/db_flat/player_gamelogs.csv'  DELIMITER ',' CSV HEADER;


-----------------------------------------------------------------------------------------------------------------
-- Create player_header table
-----------------------------------------------------------------------------------------------------------------
drop table if exists player_header;
create table player_header(player_id INT, first_name TEXT, last_name TEXT, display_name TEXT, display_last_first TEXT, display_fi_last TEXT,
birthday TEXT, school TEXT, country TEXT, last_affiliation TEXT, height TEXT, weight TEXT, season_exp TEXT, jersey TEXT, position TEXT, roster_status TEXT,
team_id INT, team_name TEXT, team_abbr TEXT, team_code TEXT, team_city TEXT, player_code TEXT, from_year INT, to_year INT, dleague_flag TEXT,
games_played_flag TEXT, draft_year TEXT, draft_round TEXT, draft_number TEXT);

\copy player_header from '/Users/Dan/Desktop/nba4nerds/db_flat/players_header.csv' DELIMITER ',' CSV HEADER;

-----------------------------------------------------------------------------------------------------------------
-- Create team_game_logs
-----------------------------------------------------------------------------------------------------------------
drop table if exists team_gamelogs;
create table team_gamelogs(team_id INT, game_id INT, game_date TEXT, matchup TEXT, wl TEXT, win INT, loss INT, w_pct DOUBLE PRECISION,
       min INT, fgm INT, fga INT, fg_pct DOUBLE PRECISION, fg3m INT, fg3a INT, fg3_pct DOUBLE PRECISION, ftm INT, fta INT,
       ft_pct DOUBLE PRECISION, oreb INT, dreb INT, reb INT, ast INT, stl INT, blk INT, tov INT, pf INT,
       pts INT)

\copy team_gamelogs from '/Users/Dan/Desktop/nba4nerds/db_flat/team_gamelogs.csv' DELIMITER ',' CSV HEADER;

-----------------------------------------------------------------------------------------------------------------
-- Create team_boxscores- needs repair
-----------------------------------------------------------------------------------------------------------------
drop table if exists team_boxscores;
create table team_boxscores(game_id INT, team_id INT, team_name TEXT, team_abbr TEXT, team_city TEXT, min TEXT, fgm INT,
	fga INT, fg_pct DOUBLE PRECISION, fg3m INT, fg3a INT, fg3_pct DOUBLE PRECISION, ftm INT, fta INT, ft_pct DOUBLE PRECISION,
	oreb INT, dreb INT, reb INT, ast INT, stl INT, blk INT, tov INT, pf INT, pts INT, plus_minus INT);

\copy team_boxscores from '/Users/Dan/Desktop/nba4nerds/db_flat/team_game_logs.csv' DELIMITER ',' CSV HEADER;

-----------------------------------------------------------------------------------------------------------------
-- Create team_season
-----------------------------------------------------------------------------------------------------------------
drop table if exists team_seasons;
create table team_seasons(team_id INT, team_city TEXT, team_name TEXT, season TEXT, gp INT, win INT, loss INT,
       win_pct DOUBLE PRECISION, conf_rank int, div_rank INT, po_wins INT, po_losses INT,
       conf_count DOUBLE PRECISION, div_count INT, finals_appearance TEXT, fgm DOUBLE PRECISION, fga DOUBLE PRECISION,
       fg_pct DOUBLE PRECISION, fg3m DOUBLE PRECISION, fg3a DOUBLE PRECISION, fg3_pct DOUBLE PRECISION, 
       ftm DOUBLE PRECISION, fta DOUBLE PRECISION, ft_pct DOUBLE PRECISION, oreb DOUBLE PRECISION,
       dreb DOUBLE PRECISION, reb DOUBLE PRECISION, ast  DOUBLE PRECISION, pf DOUBLE PRECISION, stl DOUBLE PRECISION, 
       tov DOUBLE PRECISION, blk DOUBLE PRECISION, pts DOUBLE PRECISION, pts_rank INT);

\copy team_seasons from '/Users/Dan/Desktop/nba4nerds/db_flat/team_seasons.csv' DELIMITER ',' CSV HEADER;




















