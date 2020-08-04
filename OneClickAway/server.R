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

    # Create a "data_source" reactive variable
    data_intacc <- reactive({
      # Return the appropriate data source depending on
      # the chosen radio button
      if (input$ind_access == "reason_access" & input$dissag_access == "age_access") {
        data <- lim_internet %>% select("Reason", "9–11",   "12–14",  "15–17")
      } else if (input$ind_access == "reason_access" & input$dissag_access == "gender_access") {
        data <- lim_internet %>% select(Reason, Male, Female)
      } else if (input$ind_access == "reason_access" & input$dissag_access == "reason_access") {
        data <- lim_internet %>% select(Reason, Total)
      }
      else if (input$ind_access == "places_access" & input$dissag_access == "age_access") {
        data <- place_access %>% select("Place", "9–11",   "12–14",  "15–17")
      } else if (input$ind_access == "places_access" & input$dissag_access == "gender_access") {
        data <- place_access %>% select(Place, Male, Female)
      } else if (input$ind_access == "places_access" & input$dissag_access == "reason_access") {
        data <- place_access %>% select(Place, Total)
      }
      else if (input$ind_access == "freq_access") {
        data <- device_freq}
      
      return(data)
    })
    
  output$gmplot <-renderPlot({
    
   p1 <- ggplot(data, aes(x= Reason, y= Total, fill= Total)) +geom_bar(stat= "identity")
   print(p1)
    
  })
    
} #server closing bracket

#########################  END ----
