/* create db if it doesn't exist */
CREATE DATABASE IF NOT EXISTS nba_basketball_db;


/* select db to use */
USE nba_basketball_db;


/* drop tables if they exist */
DROP TABLE IF EXISTS `games`;
DROP TABLE IF EXISTS `spreads`;
DROP TABLE IF EXISTS `totals`;
DROP TABLE IF EXISTS `moneylines`;


/* create games db */
CREATE TABLE games (
id INT NOT NULL AUTO_INCREMENT,
gid VARCHAR(20) NOT NULL,
season YEAR NOT NULL,  
date DATE NOT NULL,
site VARCHAR(5) NOT NULL,
playoffs BOOLEAN NOT NULL,

team VARCHAR(20) NOT NULL,
pts TINYINT UNSIGNED NOT NULL,
wins TINYINT UNSIGNED NOT NULL,
losses TINYINT UNSIGNED NOT NULL,
rest TINYINT UNSIGNED,
steals TINYINT UNSIGNED,
assists TINYINT UNSIGNED,
blocks TINYINT UNSIGNED,  
defensive_rebounds TINYINT UNSIGNED,
offensive_rebounds TINYINT UNSIGNED,
rebounds TINYINT UNSIGNED,
turnovers TINYINT UNSIGNED,
fouls TINYINT UNSIGNED,
field_goals_attempted TINYINT UNSIGNED,
field_goals_made TINYINT UNSIGNED,
three_pointers_attempted TINYINT UNSIGNED,
three_pointers_made TINYINT UNSIGNED,
free_throws_attempted TINYINT UNSIGNED,
free_throws_made TINYINT UNSIGNED,
pts_in_paint TINYINT UNSIGNED,
fast_break_pts TINYINT UNSIGNED,
quarter_scores VARCHAR(40),
biggest_lead TINYINT UNSIGNED,

o_team VARCHAR(20) NOT NULL,
o_pts TINYINT UNSIGNED NOT NULL,
o_wins TINYINT UNSIGNED NOT NULL,
o_losses TINYINT UNSIGNED NOT NULL,
o_rest TINYINT UNSIGNED,
o_steals TINYINT UNSIGNED,
o_assists TINYINT UNSIGNED,
o_blocks TINYINT UNSIGNED,
o_defensive_rebounds TINYINT UNSIGNED,
o_offensive_rebounds TINYINT UNSIGNED,
o_rebounds TINYINT UNSIGNED,
o_turnovers TINYINT UNSIGNED,
o_fouls TINYINT UNSIGNED,
o_field_goals_attempted TINYINT UNSIGNED,
o_field_goals_made TINYINT UNSIGNED,
o_three_pointers_attempted TINYINT UNSIGNED,
o_three_pointers_made TINYINT UNSIGNED,
o_free_throws_attempted TINYINT UNSIGNED,
o_free_throws_made TINYINT UNSIGNED,
o_pts_in_paint TINYINT UNSIGNED,
o_fast_break_pts TINYINT UNSIGNED,
o_quarter_scores VARCHAR(40),
o_biggest_lead TINYINT UNSIGNED,

matchup_wins TINYINT UNSIGNED,
matchup_losses TINYINT UNSIGNED,
lead_changes TINYINT UNSIGNED,
margin_after_first TINYINT,
margin_at_half TINYINT,
margin_after_third TINYINT,

total DECIMAL(5, 2),
line DECIMAL(5, 2),
PRIMARY KEY (id)
);


/* create moneylines db */
CREATE TABLE moneylines (
id INT NOT NULL AUTO_INCREMENT,
gid VARCHAR(20) NOT NULL,
season YEAR NOT NULL,
date DATE NOT NULL,
team VARCHAR(20) NOT NULL,
o_team VARCHAR(20) NOT NULL,
pinnacle_ml DECIMAL(4, 1),
betonline_ml DECIMAL(4, 1),
interlops_ml DECIMAL(4, 1), 
bookmaker_ml DECIMAL(4, 1),
5dimes_ml DECIMAL(4, 1),  
betdsi_ml DECIMAL(4, 1),  
heritage_ml DECIMAL(4, 1),  
bovada_ml DECIMAL(4, 1),  
youwager_ml DECIMAL(4, 1),  
justbet_ml DECIMAL(4, 1), 
PRIMARY KEY (id)
);


/* create totals db */
CREATE TABLE totals (
id INT NOT NULL AUTO_INCREMENT,
gid VARCHAR(20) NOT NULL,
season YEAR NOT NULL,
date DATE NOT NULL,
team VARCHAR(20) NOT NULL,
o_team VARCHAR(20) NOT NULL,
pinnacle_total DECIMAL(4, 1),
pinnacle_payout NUMERIC,
betonline_total DECIMAL(4, 1),
betonline_payout NUMERIC,
interlops_total DECIMAL(4, 1),
interlops_payout NUMERIC,
bookmaker_total DECIMAL(4, 1),
bookmaker_payout NUMERIC,
5dimes_total DECIMAL(4, 1),
5dimes_payout NUMERIC,
betdsi_total DECIMAL(4, 1),
betdsi_payout NUMERIC,
heritage_total DECIMAL(4, 1),
heritage_payout NUMERIC,
bovada_total DECIMAL(4, 1),  
bovada_payout NUMERIC,
youwager_total DECIMAL(4, 1), 
youwager_payout NUMERIC,
justbet_total DECIMAL(4, 1), 
justbet_payout NUMERIC,
PRIMARY KEY (id)
);


/* create spreads db */
CREATE TABLE spreads (
id INT NOT NULL AUTO_INCREMENT,
gid VARCHAR(20) NOT NULL,
season YEAR NOT NULL,
date DATE NOT NULL,
team VARCHAR(20) NOT NULL,
o_team VARCHAR(20) NOT NULL,
pinnacle_line DECIMAL(4, 1),
pinnacle_payout NUMERIC,
betonline_line DECIMAL(4, 1),
betonline_payout NUMERIC,
interlops_line DECIMAL(4, 1),
interlops_payout NUMERIC,
bookmaker_line DECIMAL(4, 1),
bookmaker_payout NUMERIC,
5dimes_line DECIMAL(4, 1),
5dimes_payout NUMERIC,
betdsi_line DECIMAL(4, 1),
betdsi_payout NUMERIC,
heritage_line DECIMAL(4, 1),
heritage_payout NUMERIC,
bovada_line DECIMAL(4, 1),  
bovada_payout NUMERIC,
youwager_line DECIMAL(4, 1), 
youwager_payout NUMERIC,
justbet_line DECIMAL(4, 1), 
justbet_payout NUMERIC,
PRIMARY KEY (id)
);

