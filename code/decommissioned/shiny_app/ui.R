shinyUI(
  fluidPage(
    titlePanel("NBA Team Points Prediction App"),
  
    sidebarLayout(
      sidebarPanel(
        uiOutput('dateCtrl'),
        uiOutput('teamCtrl')
      ),
      
      mainPanel(
        h3(uiOutput('teamMatchUpTxt')),
        br(),
        
        fluidRow(
          column(4, 
                 uiOutput('ptsProjTxt'),
                 #uiOutput('payoutOddTxt'),
                 #uiOutput('bettingLineTxt'),
                 uiOutput('winLossTxt')
          ), 
          column(4, 
                 uiOutput('oPtsAlwdProjTxt'),
                 #uiOutput('oPayoutOddTxt'),
                 #uiOutput('oBettingLineTxt')
                 uiOutput('oWinLossTxt')
          ),
          column(4, 
                 uiOutput('ptsSprdProjTxt')
          )
        )
      )
    )
  )
)
