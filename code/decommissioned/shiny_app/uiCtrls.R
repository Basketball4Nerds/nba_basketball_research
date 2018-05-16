
## date select control
output$dateCtrl <- renderUI({
  dateInput('date',
            label = 'Date',
            value = Sys.Date()
            #value = '2015-11-13'
  )
})



