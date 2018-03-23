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



#### subset 2016 season
games2016 <- subset(games, season==2016)



#### scored point mean and sd
agg <- games2016 %>% 
  group_by(team) %>%
  summarize(rqP_mean = round(mean(rqP), 2),
            rqP_sd = round(sd(rqP), 2))
agg <- as.data.frame(agg)
agg <- sortByCol(agg, col='rqP_sd')
agg
kable(agg, 'html')
head(agg)
tail(agg)



####
knicks2016 <- subset(games2016, team=='Knicks')
cavaliers2016 <- subset(games2016, team=='Cavaliers')

ggplot(knicks2016, aes(x=date, y=rqP)) + 
  geom_point(aes(color=site)) + 
  geom_line(group=1) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_y_continuous(limits=c(70, 145)) + 
  ylab('regular-quarter points') + 
  ggtitle("New York Knicks 2016 Regular Season: Regular-Quarter Points")

ggplot(cavaliers2016, aes(x=date, y=p)) + 
  geom_point(aes(color=site)) + 
  geom_line(group=1) + 
  ylab('points') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(limits=c(70, 145)) + 
  ylab('regular-quarter points') + 
  ggtitle("Cleveland Cavaliers 2016 Regular Season: Regular-Quarter Points")



#### points variation by site
agg <- games2016 %>% 
  group_by(team, site) %>%
  summarize(rqP_mean = round(mean(rqP), 2),
            rqP_sd = round(sd(rqP), 2))
agg <- dcast(setDT(agg), team ~ site, value.var=c('rqP_mean', 'rqP_sd'))
agg <- agg[ , c('team', 'rqP_mean_home', 'rqP_mean_away', 'rqP_sd_home', 'rqP_sd_away')]
agg <- 
  agg %>%
  mutate(
    rqP_mean_diff = rqP_mean_home - rqP_mean_away,
    rqP_sd_diff = rqP_sd_home - rqP_sd_away
  )
head(agg)
View(sortByCol(agg, col='rqP_mean_diff'))




ggplot(celtics2016, aes(x=date, y=rqP)) + 
  geom_point(aes(color=outcome)) + 
  geom_line(group=1) + 
  ylab('points') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(limits=c(70, 145)) + 
  facet_grid(site ~ .)

ggplot(cavaliers2016, aes(x=date, y=rqP)) + 
  geom_point(aes(color=outcome)) + 
  geom_line(group=1) + 
  ylab('points') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(limits=c(70, 145)) + 
  facet_grid(site ~ .)


agg <- x %>% 
  group_by(team, site) %>%
  summarize(p_mean = round(mean(p), 2),
            p_sd = round(sd(p), 2))
agg <- as.data.frame(agg)
agg

x <- dcast(setDT(y), team ~ site, value.var=c("p_mean", "p_sd"))





## find teams that do great at home but poorly at away
## find teams 


x <- mean(celtics2016$p[celtics2016$site=='home'])
y <- sd(celtics2016$p[celtics2016$site=='home'])

a <- mean(celtics2016$p[celtics2016$site=='away'])
b <- sd(celtics2016$p[celtics2016$site=='away'])