#Code to create OneClickAway Shiny platform
#In this script include all the server side functions: plots, reactive objects, etc.

###############################################.

## Define a server for the Shiny app
function(input, output, session) {
  
  ###############################################.
  ## Landing page ----
  ###############################################.
  # Creating events that take you to different tabs
  # activated when pressing buttons from the landing page
  
  observeEvent(input$jump_to_int_acc, {
    updateTabsetPanel(session, "intabset", selected = "acc_int")
  })
  
  output$gmplot <-renderPlot({
    
   p1 <- ggplot(lim_internet, aes(x= Reason, y= Total, fill= Total)) +geom_bar(stat= "identity")
   print(p1)
    
  })
    
} #server closing bracket

#########################  END ----
