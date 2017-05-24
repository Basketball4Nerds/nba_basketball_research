## subset dataset
focDateData <- reactive({
  if (is.null(input$date)) return()
  subset(gmsRawDf, date==input$date)
})


## teams that are playing at a given date
teams <- reactive({
  if (is.null(focDateData())) return()
  unique(as.character(focDateData()$team))
})


## opponent team of a selected team on a given date
oTeam <- reactive({
  if (is.null(input$team)) return()
  if (is.null(focDateData())) return()
  df <- focDateData()
  print(head(df))
  as.character(df$o_team[df$team==input$team])
})


## projected team points (not rounded)
ptsProjRaw <- reactive({
  if (is.null(input$date)) return()
  if (is.null(input$team)) return()
  if (is.null(oTeam())) return()
  pts.avg <- getMvAvg(gmsRawDf, var='pts', tm=input$team, focDate=input$date, n=24)
  o_o_pts_alwd.avg <- getMvAvg(gmsRawDf, var='o_pts_alwd', tm=oTeam(), focDate=input$date, n=24)
  pts.proj <- (pts.avg + o_o_pts_alwd.avg) / 2
  pts.proj
})


## projected opponent team points (not rounded)
oPtsAlwdProjRaw <- reactive({
  if (is.null(input$date)) return()
  if (is.null(input$team)) return()
  if (is.null(oTeam())) return()
  o_pts_alwd.avg <- getMvAvg(gmsRawDf, var='o_pts_alwd', tm=input$team, focDate=input$date, n=24)
  o_pts.avg <- getMvAvg(gmsRawDf, var='pts', tm=oTeam(), focDate=input$date, n=24)
  o_pts_alwd.proj <- (o_pts_alwd.avg + o_pts.avg) / 2
  o_pts_alwd.proj
})


## projected team points and projected opponent team points (not rounded)
ptsProjs <- reactive({
  a <- ptsProjRaw()
  b <- oPtsAlwdProjRaw()
  
  if (is.null(a)) return()
  if (is.null(b)) return()
  if (is.nan(a) | is.nan(b)) return()

  if (round(a)==round(b)) {
    if (a > b) {
      a <- ceiling(a)
      b <- floor(b)
    } else if (a < b) {
      a <- floor(a)
      b <- ceiling(b)
    } 
  } 
  a <- round(a)
  b <- round(b)
  c(a, b)
})


## projected point spread
ptsSprdProj <- reactive({
  if (is.null(ptsProjs())) return()
  ptsProjs()[1] - ptsProjs()[2]
})


## team's wins-losses
WL <- reactive({
  if (is.null(input$date)) return()
  if (is.null(input$team)) return()
  df <- subset(gmsRawDf, date==input$date & team==input$team)
  wins <- df$wins
  losses <- df$losses
  paste0(wins, ' - ', losses)
})


## opponent's wins-losses
oWL <- reactive({
  if (is.null(input$date)) return()
  if (is.null(oTeam())) return()
  df <- subset(gmsRawDf, date==input$date & team==oTeam())
  wins <- df$wins
  losses <- df$losses
  paste0(wins, ' - ', losses)
})


# ## Pinnacle betting line
# bettingLine <- reactive({
#   if (is.null(input$date)) return()
#   if (is.null(input$team)) return()
#   odds$pts.margin.bet[odds$date==input$date & odds$team==input$team]
# })
# 
# 
# ## Pinnacle betting line for opponent team
# oBettingLine <- reactive({
#   if (is.null(input$date)) return()
#   if (is.null(oTeam())) return()
#   odds$pts.margin.bet[odds$date==input$date & odds$team==oTeam()]
# })
# 
# 
# ## Pinnacle betting odds ratio
# payoutOdd <- reactive({
#   if (is.null(input$date)) return()
#   if (is.null(input$team)) return()
#   odds$payout.odd[odds$date==input$date & odds$team==input$team]
# })
# 
# 
# ## Pinnacle betting odds ratio for opponent team
# oPayoutOdd <- reactive({
#   if (is.null(input$date)) return()
#   if (is.null(oTeam())) return()
#   odds$payout.odd[odds$date==input$date & odds$team==oTeam()]
# })



## historical accuracy percentage (general)


## historical accuracy percentage (team-specific)

