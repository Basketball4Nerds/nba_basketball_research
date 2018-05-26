## correct and standardize team abbreviations 
df_names <- c('moneylines', 'spreads', 'totals')

for (df_name in df_names) {
  
  df <- get(df_name) %>%
    mutate(
      team_abbr = case_when(
        team_abbr=='PHO' ~ 'PHX',  # change PHO to PHX
        team_abbr=='NOP' & season < '2013-14' ~ 'NOH',  # prior to 2013-14 season, Pelicans were called Hornets
        team_abbr=='BKN' & '1977-78' <= season & season < '2012-13' ~ 'NJN',  # prior to 2012-13 season, Brooklyn Nets were called New Jersey Nets
        TRUE ~ team_abbr
      ),
      
      opponent_abbr = case_when(
        opponent_abbr=='PHO' ~ 'PHX',  # change PHO to PHX
        opponent_abbr=='NOP' & season < '2013-14' ~ 'NOH',  # prior to 2013-14 season, Pelicans were called Hornets
        opponent_abbr=='BKN' & '1977-78' <= season & season < '2012-13' ~ 'NJN',  # prior to 2012-13 season, Brooklyn Nets were called New Jersey Nets
        TRUE ~ opponent_abbr      
      )
    )  
  assign(x=df_name, value=df)
}


PHO => PHX
UTH => UTA
GOS => GSW
NOK => NOH
SAN => SAS
CHH => Charlotte Hornets
KCK => Kansas City Kings
PHL => PHI

SDC (San Diego Clippers)
SEA (Seattle Supersonics)
VAN (Vancouver Grizzlies)