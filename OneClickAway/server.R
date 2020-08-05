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
      data <- lim_internet %>% select("Reason", "9–11", "12–14", "15–17") %>%
      gather("age_group", "age_value", "9–11", "12–14", "15–17")
      
    } else if (input$ind_access == "reason_access" & input$dissag_access == "gender_access") {
      data <- lim_internet %>% select("Reason", Male, Female) %>%
        gather ("gender", "gender_value", "Male", "Female")  
        
    } else if (input$ind_access == "reason_access" & input$dissag_access == "reason_access") {
      data <- lim_internet %>% select("Reason", "Total")
    }
    else if (input$ind_access == "places_access" & input$dissag_access == "age_access") {
      data <- place_access %>% select("Place", "9–11",   "12–14",  "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
    } else if (input$ind_access == "places_access" & input$dissag_access == "gender_access") {
      data <- place_access %>% select(Place, Male, Female)  %>%
        gather ("gender", "gender_value", "Male", "Female")  
    } else if (input$ind_access == "places_access" & input$dissag_access == "reason_access") {
      data <- place_access %>% select(Place, Total)
    }
    else if (input$ind_access == "freq_access") {
      data <- device_freq %>%
        gather("Frequency", "Value", "Never","Hardly ever","At least every month","At least every week","Daily or almost daily",
               "Several times each day","Almost all the time","Don’t know","Refusal")}
    return(data)
  })
  

  output$gmplot <-renderPlot({
    if (input$ind_access == "reason_access" & input$dissag_access == "age_access")
      {
      p <- ggplot(data = data_intacc()) +
      geom_col(aes(x=age_value, y=Reason, fill=age_group), position = "stack")
      print(p)
      }         
     else if (input$ind_access == "reason_access" & input$dissag_access == "gender_access") {
      p <- ggplot(data = data_intacc()) +
      geom_col(aes(x=gender_value, y=Reason, fill=gender), position = "stack")
      print(p)
     }
     else if (input$ind_access == "reason_access" & input$dissag_access == "reason_access") {
       p <- ggplot(data = data_intacc()) +
         geom_col(aes(x=Total, y=Reason, fill = Total), position = "stack")
       print(p)
                 }   
      else if (input$ind_access == "places_access" & input$dissag_access == "age_access") {
        p <- ggplot(data = data_intacc()) +
          geom_col(aes(x=age_value, y=Place, fill=age_group), position = "stack")
        print(p)
                 }
      else if (input$ind_access == "places_access" & input$dissag_access == "gender_access") {
         p <- ggplot(data = data_intacc()) +
         geom_col(aes(x=gender_value, y=Place, fill=gender), position = "stack")
         print(p)
                 }
       else if (input$ind_access == "places_access" & input$dissag_access == "reason_access") {
         p <- ggplot(data = data_intacc()) +
         geom_col(aes(x=Total, y=Place, fill = Total), position = "stack")
         print(p)
                }   
                else if (input$ind_access == "freq_access") {   
                  p <- ggplot(data = data_intacc()) +
                    geom_col(aes(x=Value, y=Device, fill=Frequency), position = "stack")
                  print(p)
                }
                   }) 

  } #server closing bracket

#########################  END ----