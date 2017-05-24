## team select control
output$teamCtrl <- renderUI({
  if (is.null(teams())) return()
  selectInput('team', label='Team', choices=teams())  
})


## team match-up text
output$teamMatchUpTxt <- renderText({
  if (is.null(input$team)) return()
  if (is.null(oTeam())) return()
  matchUpTxt <- paste0(input$team, ' vs. ', oTeam())
  matchUpTxt
})


## projected team points
output$ptsProjTxt <- renderText({
  if (is.null(input$team)) return()
  ptsProj <- ptsProjs()[1]
  paste0(input$team, ' Projected Points: ', ptsProj)
})


## projected opponent team point
output$oPtsAlwdProjTxt <- renderText({
  if (is.null(oTeam())) return()
  oPtsAlwdProj <- ptsProjs()[2]
  paste0(oTeam(), ' Projected Points: ', oPtsAlwdProj)
})
  
  
## projected point spread
output$ptsSprdProjTxt <- renderText({
  if (is.null(ptsSprdProj())) return()
  paste0('Projected Points Spread: ', ptsSprdProj())
})


## team's wins/losses
output$winLossTxt <- renderText({
  if (is.null(input$team)) return()
  if (is.null(WL())) return()
  paste0(input$team, ' W-L: ', WL())
})

## opponent's wins/losses
output$oWinLossTxt <- renderText({
  if (is.null(oTeam())) return()
  if (is.null(oWL())) return()
  paste0(oTeam(),  ' W-L: ', oWL())
})


# ## team's payout odd
# output$payoutOddTxt <- renderText({
#   if (is.null(payoutOdd())) return()
#   if (is.null(input$team)) return()
#   paste0(input$team, ' Payout Ratio: ', payoutOdd())
# })
# 
# 
# ## opponent team's payout odd
# output$oPayoutOddTxt <- renderText({
#   if (is.null(oPayoutOdd())) return()
#   if (is.null(oTeam())) return()
#   paste0(oTeam(), ' Payout Ratio: ', payoutOdd())
# })
# 
# 
# ## team's betting line
# output$bettingLineTxt <- renderText({
#   if (is.null(bettingLine())) return()
#   if (is.null(input$team)) return()
#   line <- bettingLine()
#   if (line > 0)
#     line <- paste0('+', line)
#   paste0(input$team, ' Betting Line: ', line)
# })
# 
# 
# ## opponent team's betting line
# output$oBettingLineTxt <- renderText({
#   if (is.null(oBettingLine())) return()
#   if (is.null(oTeam())) return()
#   line <- oBettingLine()
#   if (line > 0)
#     line <- paste0('+', line)
#   paste0(oTeam(), ' Betting Line: ', line)
# })