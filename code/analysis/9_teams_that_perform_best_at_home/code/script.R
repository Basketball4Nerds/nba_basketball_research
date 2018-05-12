#### load regular season games
games <- read.csv("../../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)
games$outcome <- ifelse(games$won, 'won', 'lost')
games$site <- ifelse(games$site=='H', 'home', 'away')
games$team_type <- ifelse(games$line < 0, 'favorite', 
                          ifelse(games$line > 0, 'underdog', NA))


#### loading libraries
library(ggplot2)
library(gridExtra)
library(psych)
library(dplyr)
library(reshape2)
library(data.table)
library(BBmisc)
library(knitr)



## subsetting 2015 regular season games data
games2015 <- subset(games, season==2015)


## finding win
agg <- games2015 %>% 
  group_by(team, site) %>%
  summarize(w=sum(won), l=sum(!won)) %>%
  mutate(
    n = w + l,
    record = paste0(w, '-', l),
    wpc = round(w / n, 3)
  )
agg <- as.data.frame(agg)
agg <- dcast(setDT(agg), team ~ site, value.var=c('record', 'wpc'))
agg <- agg[ , c('team', 'record_home', 'record_away', 'wpc_home', 'wpc_away')]
agg$wpc_diff <- agg$wpc_home - agg$wpc_away
agg <- sortByCol(agg, col='wpc_diff', asc=FALSE)
agg <- 
  agg %>% 
  mutate(
  wpc_home = as.numeric(paste0(wpc_home * 100)),
  wpc_away = as.numeric(paste0(wpc_away * 100)),
  wpc_diff = as.numeric(paste0(wpc_diff * 100))
)

## create separate df with % for html table
agg_pc <- 
  agg %>%
  mutate(
    wpc_home = paste0(wpc_home, '%'),
    wpc_away = paste0(wpc_away, '%'),
    wpc_diff = paste0(wpc_diff, '%')
  )
kable(agg_pc, 'html')


## plotting
ggplot(agg) + 
  geom_bar(aes(x=reorder(team, -wpc_diff), y=wpc_diff), stat='identity') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  xlab('team') + 
  ylab("win percentage difference") + 
  ggtitle("2015-2016 Regular Season: Home vs. Away Win Percentage Difference")

