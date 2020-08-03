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
  
  
  
  
  
  df <- callModule(linkedScatter, "scatters", reactive(mpg),
                   left = reactive(c("cty", "hwy")),
                   right = reactive(c("drv", "hwy"))
  )
  
  output$summary <- renderText({
    sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
  })
} #server closing bracket

#########################  END ----
