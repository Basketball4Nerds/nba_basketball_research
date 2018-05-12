cavs <- subset(games, season==1995 & team=='Cavaliers')
bulls <- subset(games, season==1995 & team=='Bulls')

cavs <- 
  cavs %>%
  group_by(season, team, site) %>%
  arrange(date) %>%
  mutate(
    w_site = cumsum(won),
    l_site = cumsum(!won),
    w_site = c(0, w_site)[-length(w_site)],
    l_site = c(0, l_site)[-length(l_site)],
    n_site = w_site + l_site,
    wpc_site = w_site / n_site
  ) 

bulls <- 
  bulls %>%
  group_by(season, team, site) %>%
  arrange(date) %>%
  mutate(
    w_site = cumsum(won),
    l_site = cumsum(!won),
    w_site = c(0, w_site)[-length(w_site)],
    l_site = c(0, l_site)[-length(l_site)],
    n_site = w_site + l_site,
    wpc_site = w_site / n_site
  ) 


x <- subset(games, season==1995 & team=='Cavaliers' & o_team=='Bulls')
x <- as.data.frame(x)
x <- x[ , c(base_cols, 'wpc_site')]
x


y <- subset(games, season==1995 & team=='Bulls' & o_team=='Cavaliers')
y <- as.data.frame(y)
y <- y[ , c(base_cols, 'wpc_site')]
y


tail(bulls)
z <- subset(games, season==1995 & team=='Bulls' & o_team=='Cavaliers')
z <-as.data.frame(z)