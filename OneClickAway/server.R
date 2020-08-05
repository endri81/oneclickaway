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
  observeEvent(input$jump_to_digital, {
    updateTabsetPanel(session, "intabset", selected = "digital")
  })
  
  # Create a "data_source" reactive variable
  data_intacc <- reactive({
    # Return the appropriate data source depending on
    # the chosen radio button
    if (input$ind_access == "Total" & input$dissag_access == "age_access") {
      data <- lim_internet %>% select("Reason", "9–11", "12–14", "15–17") %>%
      gather("age_group", "age_value", "9–11", "12–14", "15–17")
      
    } else if (input$ind_access == "reason_access" & input$dissag_access == "gender_access") {
      data <- lim_internet %>% select("Reason", Male, Female) %>%
        gather ("gender", "gender_value", "Male", "Female")  
        
    } else if (input$ind_access == "reason_access" & input$dissag_access == "total_access") {
      data <- lim_internet %>% select("Reason", "Total")
    }
    else if (input$ind_access == "places_access" & input$dissag_access == "age_access") {
      data <- place_access %>% select("Place", "9–11",   "12–14",  "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
    } else if (input$ind_access == "places_access" & input$dissag_access == "gender_access") {
      data <- place_access %>% select(Place, Male, Female)  %>%
        gather ("gender", "gender_value", "Male", "Female")  
    } else if (input$ind_access == "places_access" & input$dissag_access == "total_access") {
      data <- place_access %>% select(Place, Total)
    }
    else if (input$ind_access == "freq_access") {
      data <- device_freq %>%
        gather("Frequency", "Value", "Never","Hardly ever","At least every month","At least every week","Daily or almost daily",
               "Several times each day","Almost all the time","Don’t know","Refusal")}
    return(data)
  })
  

  output$accplot <-renderPlot({
    if (input$ind_access == "reason_access" & input$dissag_access == "age_access")
      {
      p <- ggplot(data = data_intacc()) +
      geom_col(aes(x=age_value, y=Reason, fill=age_group), position = "stack")+
      theme(legend.position = "none")    + 
      labs(title = "Reasons for limited access to Internet by child’s age (%)", 
                                                                 x = NULL, y = NULL)  +
        scale_fill_manual(values = pal_simd_bar)
      
      print(p)
      }         
     else if (input$ind_access == "reason_access" & input$dissag_access == "gender_access") {
      p <- ggplot(data = data_intacc()) +
      geom_col(aes(x=gender_value, y=Reason, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Reasons for limited access to Internet by child’s gender(%)", 
             x = NULL, y = NULL)  +
        scale_fill_manual(values = pal_simd_bar)
      print(p)
     }
     else if (input$ind_access == "reason_access" & input$dissag_access == "total_access") {
       p <- ggplot(data = data_intacc()) +
         geom_col(aes(x=Total, y=Reason, fill = Total), position = "stack")+
         theme(legend.position = "none")    + 
         labs(title = "Reasons for limited access to Internet(%)", 
              x = NULL, y = NULL)  +
         scale_color_manual(values = pal_simd_bar)
       print(p)
                 }   
      else if (input$ind_access == "places_access" & input$dissag_access == "age_access") {
        p <- ggplot(data = data_intacc()) +
          geom_col(aes(x=age_value, y=Place, fill=age_group), position = "stack")+
    theme(legend.position = "none")    + 
    labs(title = "Places of Internet use, by child’s age (%)", 
         x = NULL, y = NULL)   +
          scale_fill_manual(values = pal_simd_bar)
        print(p)
                 }
      else if (input$ind_access == "places_access" & input$dissag_access == "gender_access") {
         p <- ggplot(data = data_intacc()) +
         geom_col(aes(x=gender_value, y=Place, fill=gender), position = "stack")+
           theme(legend.position = "none")    + 
           labs(title = "Places of Internet use, by child’s gender (%)", 
                x = NULL, y = NULL)  +
           scale_fill_manual(values = pal_simd_bar)
         print(p)
                 }
       else if (input$ind_access == "places_access" & input$dissag_access == "total_access") {
         p <- ggplot(data = data_intacc()) +
         geom_col(aes(x=Total, y=Place, fill = Total), position = "stack")+
           theme(legend.position = "none")    + 
           labs(title = "Places of Internet use (%)", 
                x = NULL, y = NULL)  +
           scale_color_manual(values = pal_simd_bar)
         print(p)
                }   
                else if (input$ind_access == "freq_access") {   
                  p <- ggplot(data = data_intacc()) +
                    geom_col(aes(x=Value, y=Device, fill=Frequency), position = "stack")+
                    labs(title = "How often different devices are used to access the Internet (%)", 
                         x = NULL, y = NULL)  +
                    scale_fill_manual(values = pal_simd_bar)
                  print(p)
                }
                   }) 

  } #server closing bracket

#########################  END ----