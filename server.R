#Code to create OneClickAway Shiny platform
#In this script include all the server side functions: plots, reactive objects, etc.

###############################################.
shinyOptions(shiny.useragg = TRUE)
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
  observeEvent(input$jump_to_risk, {
    updateTabsetPanel(session, "intabset", selected = "risk")
  })
  observeEvent(input$jump_to_parent, {
    updateTabsetPanel(session, "intabset", selected = "parent")
  })
  observeEvent(input$jump_to_kfind, {
    updateTabsetPanel(session, "intabset", selected = "kfind")
  })
  observeEvent(input$jump_to_method, {
    updateTabsetPanel(session, "intabset", selected = "method")
  })
  observeEvent(input$jump_to_data, {
    updateTabsetPanel(session, "intabset", selected = "data")
  })
  observeEvent(input$jump_to_biblio, {
    updateTabsetPanel(session, "intabset", selected = "biblio")
  })
  observeEvent(input$jump_to_about, {
    updateTabsetPanel(session, "intabset", selected = "about")
  })
  observeEvent(input$jump_to_parentchildren, {
    updateTabsetPanel(session, "intabset", selected = "parentchildren")
  })
 ## IntroJS allow switching between tabs----
  observeEvent(input$btn_landing, {
   introjs(session,
           events = list(onbeforechange = readCallback("switchTabs")))
 })
  ############################################### 
  ## Internet Access  ----  
  ###############################################.
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
  

  output$accplot <-renderPlot(height = 400, width = 650,{
    if (input$ind_access == "reason_access" & input$dissag_access == "age_access")
      {
      p <- ggplot(data = data_intacc()) +
      geom_col(aes(x=age_value, y=Reason, fill=age_group), position = "stack")+
      theme(legend.position = "none")    + 
      labs(title = "Reasons for limited access to Internet by child’s age (%)", 
                                                                 x = NULL, y = NULL)  + 
        scale_fill_brewer(type = "qual", palette = "PRGn")
      
      print(p)
      }         
     else if (input$ind_access == "reason_access" & input$dissag_access == "gender_access") {
      p <- ggplot(data = data_intacc()) +
      geom_col(aes(x=gender_value, y=Reason, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Reasons for limited access to Internet by child’s gender(%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
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
          scale_fill_brewer(type = "qual", palette = "PRGn")
        print(p)
                 }
      else if (input$ind_access == "places_access" & input$dissag_access == "gender_access") {
         p <- ggplot(data = data_intacc()) +
         geom_col(aes(x=gender_value, y=Place, fill=gender), position = "stack")+
           theme(legend.position = "none")    + 
           labs(title = "Places of Internet use, by child’s gender (%)", 
                x = NULL, y = NULL)  +
           scale_fill_brewer(type = "qual", palette = "PRGn")
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
                    scale_fill_brewer(type = "qual", palette = "PRGn")
                  print(p)
                }
                   }) 
  
  ############################################### 
  ## Digital Skills  ----  
  ###############################################.    
  # Create a "data_source" reactive variable
  data_digital <- reactive({
    # Return the appropriate data source depending on
    # the chosen radio button
    if (input$ind_digital == "web_digital" & input$dissag_digital == "age_digital") {
      data_dig <- website_dig %>% select("Website_app", "9–11", "12–14", "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
      
    } else if (input$ind_digital == "web_digital" & input$dissag_digital == "gender_digital") {
      data_dig <- website_dig %>% select("Website_app", Male, Female) %>%
        gather ("gender", "gender_value", "Male", "Female")  
      
    } else if (input$ind_digital == "web_digital" & input$dissag_digital == "total_digital") {
      data_dig <- website_dig %>% select("Website_app", "Total")
    }
    else if (input$ind_digital == "freq_act_digs" & input$dissag_digital == "age_digital") {
      data_dig <- freq_act_dig %>% select("Activity", "9–11",   "12–14",  "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
    } else if (input$ind_digital == "freq_act_dig" & input$dissag_digital == "gender_digital") {
      data_dig <- freq_act_dig %>% select("Activity", "Male", "Female")  %>%
        gather ("gender", "gender_value", "Male", "Female")  
    } else if (input$ind_digital == "freq_act_dig" & input$dissag_digital == "total_digital") {
      data_dig <- freq_act_dig %>% select("Activity", "Total")
    }
    else if (input$ind_digital == "conf_digital" & input$dissag_digital == "age_digital") {
      data_dig <- skill_conf_dig %>% select("Skill", "9–11",   "12–14",  "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
    }
    return(data_dig)
  })
  
  
  output$digplot <-renderPlot({
    if (input$ind_digital == "web_digital" & input$dissag_digital == "age_digital")
    {
      p1 <- ggplot(data = data_digital()) +
        geom_col(aes(x=age_value, y= Website_app, fill=age_group), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Websites or apps used by children, by age (%) ", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      
      print(p1)
    }         
    else if (input$ind_digital == "web_digital" & input$dissag_digital == "gender_digital") {
      p1 <- ggplot(data = data_digital()) +
        geom_col(aes(x=gender_value, y= Website_app, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Websites or apps used by children, by gender (%) ", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }
    else if (input$ind_digital == "web_digital" & input$dissag_digital == "total_digital") {
      p <- ggplot(data = data_digital()) +
        geom_col(aes(x=Total, y= Website_app, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Websites or apps used by children(%) ", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }   
    else if (input$ind_digital == "freq_act_digs" & input$dissag_digital == "age_digital") {
      p <- ggplot(data = data_digital()) +
        geom_col(aes(x=age_value, y= Activity, fill=age_group), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Frequency of activities practised weekly or more often, by age group (%)", 
             x = NULL, y = NULL)   +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_digital == "freq_act_dig" & input$dissag_digital == "gender_digital") {
      p <- ggplot(data = data_digital()) +
        geom_col(aes(x=gender_value, y=Activity, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Frequency of activities practised weekly or more often, by gender (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_digital == "freq_act_dig" & input$dissag_digital == "total_digital") {
      p <- ggplot(data = data_digital()) +
        geom_col(aes(x=Total, y=Activity, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Frequency of activities practised weekly or more often(%)", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }   
    else if (input$ind_digital == "conf_digital" & input$dissag_digital == "age_digital") {   
      p <- ggplot(data = data_digital()) +
        geom_col(aes(x=age_value, y= Skill, fill=age_group), position = "stack")+
        theme(legend.position = "none") +   
        labs(title = "Children who report being fairly or very confident in a digital skill (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
  })   

  ############################################### 
  ## Online Risks  ----  
  ###############################################.   
  # Create a "data_source" reactive variable
  data_risk <- reactive({
    # Return the appropriate data source depending on
    # the chosen radio button
    if (input$ind_risk == "upset_level" & input$dissag_risk == "age_risk") {
      data_risk <- upset_level %>% select("Level", "9–11", "12–14", "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
      
    } else if (input$ind_risk == "upset_level" & input$dissag_risk == "gender_risk") {
      data_risk <- upset_level %>% select("Level", Male, Female) %>%
        gather ("gender", "gender_value", "Male", "Female")  
      
    } else if (input$ind_risk == "upset_level" & input$dissag_risk == "total_risk") {
      data_risk <- upset_level %>% select("Level", "Total")
    }
    else if (input$ind_risk == "upset_freq" & input$dissag_risk == "age_risk") {
      data_risk <- upset_freq %>% select("Frequency", "9–11",   "12–14",  "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
    } else if (input$ind_risk == "upset_freq" & input$dissag_risk == "gender_risk") {
      data_risk <- upset_freq %>% select("Frequency", "Male", "Female")  %>%
        gather ("gender", "gender_value", "Male", "Female")  
    } else if (input$ind_risk == "upset_freq" & input$dissag_risk == "total_risk") {
      data_risk <- upset_freq %>% select("Frequency", "Total")
    }
    else if (input$ind_risk == "ways_exp" & input$dissag_risk == "age_risk") {
      data_risk <- means_exp %>% select("Means", "9–11",   "12–14",  "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
    } else if (input$ind_risk == "ways_exp" & input$dissag_risk == "gender_risk") {
      data_risk <- means_exp %>% select("Means", "Male", "Female")  %>%
        gather ("gender", "gender_value", "Male", "Female")  
    } else if (input$ind_risk == "ways_exp" & input$dissag_risk == "total_risk") {
      data_risk <- means_exp %>% select("Means", "Total")
    }
      else if (input$ind_risk == "way_feel" & input$dissag_risk == "gender_risk") {
      data_risk <- feeling_exp %>% select("Feeling", "Male", "Female")  %>%
        gather ("gender", "gender_value", "Male", "Female")  
    } else if (input$ind_risk == "way_feel" & input$dissag_risk == "total_risk") {
      data_risk <- feeling_exp %>% select("Feeling", "Total")
    }
    else if (input$ind_risk == "means_sex" & input$dissag_risk == "age_risk") {
      data_risk <- means_sex %>% select("Means", "9–11",   "12–14",  "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
    } else if (input$ind_risk == "means_sex" & input$dissag_risk == "gender_risk") {
      data_risk <- means_sex %>% select("Means", "Male", "Female")  %>%
        gather ("gender", "gender_value", "Male", "Female")  
    } else if (input$ind_risk == "means_sex" & input$dissag_risk == "total_risk") {
      data_risk <- means_sex %>% select("Means", "Total")
    }
    else if (input$ind_risk == "upset_level_sex" & input$dissag_risk == "age_risk") {
      data_risk <- upset_level_dis %>% select("upset_level", "9–11",   "12–14",  "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
    } else if (input$ind_risk == "upset_level_sex" & input$dissag_risk == "gender_risk") {
      data_risk <- upset_level_dis %>% select("upset_level", "Male", "Female")  %>%
        gather ("gender", "gender_value", "Male", "Female")  
    } else if (input$ind_risk == "upset_level_sex" & input$dissag_risk == "total_risk") {
      data_risk <- upset_level_dis %>% select("upset_level", "Total")
    }
    else if (input$ind_risk == "parent_aware" & input$dissag_risk == "age_risk") {
      data_risk <- parent_aware %>% select("Online_risk", "9–11",   "12–14",  "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
    } else if (input$ind_risk == "parent_aware" & input$dissag_risk == "gender_risk") {
      data_risk <- parent_aware %>% select("Online_risk", "Male", "Female")  %>%
        gather ("gender", "gender_value", "Male", "Female")  
    } else if (input$ind_risk == "parent_aware" & input$dissag_risk == "total_risk") {
      data_risk <- parent_aware %>% select("Online_risk", "Total")
    }
    
    
    
    
    return(data_risk)
  })
  
  
  output$riskplot <-renderPlot({
    if (input$ind_risk == "upset_level" & input$dissag_risk == "age_risk"){
      p1 <- ggplot(data = data_risk()) +
        geom_col(aes(x=age_value, y= Level, fill=age_group), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Children’s level of being upset by exposure to harmful content online, by age (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      
      print(p1)
    }         
    else if (input$ind_risk == "upset_level" & input$dissag_risk == "gender_risk") {
      p1 <- ggplot(data = data_risk()) +
        geom_col(aes(x=gender_value, y= Level, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Children’s level of being upset by exposure to harmful content online, by gender (%) ", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }
    else if (input$ind_risk == "upset_level" & input$dissag_risk == "total_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=Total, y= Level, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Children’s level of being upset by exposure to harmful content online (%) ", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }   
    else if (input$ind_risk == "upset_freq"  & input$dissag_risk == "age_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=age_value, y= Frequency, fill=age_group), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "How often children felt upset by hateful and degrading messages online, by age (%)", 
             x = NULL, y = NULL)   +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_risk == "upset_freq"  & input$dissag_risk == "gender_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=gender_value, y=Frequency, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "How often children felt upset by hateful and degrading messages online, by age gender (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_risk == "upset_freq"  & input$dissag_risk == "total_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=Total, y=Frequency, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "How often children felt upset by hateful and degrading messages online (%)", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }   
    else if (input$ind_risk == "ways_exp"  & input$dissag_risk == "age_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=age_value, y= Means, fill=age_group), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Ways in which children were exposed to sexual content, by age (%) ", 
             x = NULL, y = NULL)   +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_risk == "ways_exp"  & input$dissag_risk == "gender_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=gender_value, y=Means, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Ways in which children were exposed to sexual content, by gender (%) ", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_risk == "ways_exp"  & input$dissag_risk == "total_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=Total, y=Means, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Ways in which children were exposed to sexual content(%) ", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }   
    else if (input$ind_risk == "way_feel"  & input$dissag_risk == "gender_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=gender_value, y=Feeling, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "How children felt after seeing sexual content online, by gender (%) ", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_risk == "way_feel"  & input$dissag_risk == "total_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=Total, y=Feeling, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "How children felt after seeing sexual content online (%)", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }  
    else if (input$ind_risk == "means_sex"  & input$dissag_risk == "age_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=age_value, y= Means, fill=age_group), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Means by which children saw sexual content online, by age (%)", 
             x = NULL, y = NULL)   +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_risk == "means_sex"  & input$dissag_risk == "gender_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=gender_value, y=Means, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Means by which children saw sexual content online, by gender (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_risk == "means_sex"  & input$dissag_risk == "total_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=Total, y=Means, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Means by which children saw sexual content online", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }  
    else if (input$ind_risk == "upset_level_sex"  & input$dissag_risk == "age_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=age_value, y= upset_level, fill=age_group), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "How children felt after seeing sexual content, by age (%)", 
             x = NULL, y = NULL)   +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_risk == "upset_level_sex"  & input$dissag_risk == "gender_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=gender_value, y=upset_level, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "How children felt after seeing sexual content, by gender (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_risk == "upset_level_sex"  & input$dissag_risk == "total_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=Total, y=upset_level, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "How children felt after seeing sexual content", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }     
    else if (input$ind_risk == "parent_aware"  & input$dissag_risk == "age_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=age_value, y= Online_risk, fill=age_group), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parents’ awareness of children’s experience of online risks, by child’s age (%)", 
             x = NULL, y = NULL)   +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_risk == "parent_aware"  & input$dissag_risk == "gender_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=gender_value, y= Online_risk, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parents’ awareness of children’s experience of online risks, by child’s gender(%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p)
    }
    else if (input$ind_risk == "parent_aware"  & input$dissag_risk == "total_risk") {
      p <- ggplot(data = data_risk()) +
        geom_col(aes(x=Total, y= Online_risk, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parents’ awareness of children’s experience of online risks", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }  
    
  })  
  
  
  ############################################### 
  ## Parent mediation  ----  
  ###############################################.   
  # Create a "data_source" reactive variable
  data_parent <- reactive({
    # Return the appropriate data source depending on
    # the chosen radio button
    if (input$ind_parent == "parent_med" & input$btchparent == "age_parent") {
      data_parent <- parent_med %>% select("Mediation", "9–11", "12–14", "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
      
    } 
    else if (input$ind_parent == "parent_med" & input$btchparent == "gender_parent") {
      data_parent <- parent_med %>% select("Mediation", Male, Female) %>%
        gather ("gender", "gender_value", "Male", "Female")  
      
    } 
    else if (input$ind_parent == "parent_med" & input$btchparent == "total_parent") {
      data_parent <- parent_med %>% select("Mediation", "Total")
    }
    else if (input$ind_parent == "child_act" & input$btchparent == "age_parent") {
      data_parent <- child_act %>% select("Activity", "9–11", "12–14", "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
      
    } else if (input$ind_parent == "child_act" & input$btchparent == "gender_parent") {
      data_parent <- child_act %>% select("Activity", Male, Female) %>%
        gather ("gender", "gender_value", "Male", "Female")  
      
    } else if (input$ind_parent == "child_act" & input$btchparent == "total_parent") {
      data_parent <- child_act %>% select("Activity", "Total")
    }
    else if (input$ind_parent == "proh_act" & input$btchparent == "age_parent") {
      data_parent <- proh_act %>% select("Activity", "9–11", "12–14", "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
      
    } else if (input$ind_parent == "proh_act" & input$btchparent == "gender_parent") {
      data_parent <- proh_act %>% select("Activity", Male, Female) %>%
        gather ("gender", "gender_value", "Male", "Female")  
      
    } else if (input$ind_parent == "proh_act" & input$btchparent == "total_parent") {
      data_parent <- proh_act %>% select("Activity", "Total")
    }
    else if (input$ind_parent == "monitor" & input$btchparent == "age_parent") {
      data_parent <- monitor %>% select("Monitoring_activity", "9–11", "12–14", "15–17") %>%
        gather("age_group", "age_value", "9–11", "12–14", "15–17")
      
    } else if (input$ind_parent == "monitor" & input$btchparent == "gender_parent") {
      data_parent <- monitor %>% select("Monitoring_activity", Male, Female) %>%
        gather ("gender", "gender_value", "Male", "Female")  
      
    } else if (input$ind_parent == "monitor" & input$btchparent == "total_parent") {
      data_parent <- monitor %>% select("Monitoring_activity", "Total")
    }
    
    
    
    
    
    return(data_parent)
  })
  
  
  output$parentplot <-renderPlot({
    if (input$ind_parent == "parent_med" & input$btchparent == "age_parent") {
      p1 <- ggplot(data = data_parent()) +
        geom_col(aes(x=age_value, y= Mediation, fill=age_group), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parental active mediation as reported by children, by child’s age (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      
      print(p1)
    }         
    else if (input$ind_parent == "parent_med" & input$btchparent == "gender_parent") {
      p1 <- ggplot(data = data_parent()) +
        geom_col(aes(x=gender_value, y= Mediation, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parental active mediation as reported by children, by child’s gender (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }
    else if (input$ind_parent == "parent_med" & input$btchparent == "total_parent") {
      p <- ggplot(data = data_parent()) +
        geom_col(aes(x=Total, y= Mediation, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parental active mediation as reported by children", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }  
    else if (input$ind_parent == "child_act" & input$btchparent == "age_parent") {
      p1 <- ggplot(data = data_parent()) +
        geom_col(aes(x=age_value, y= Activity, fill=age_group), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Activities that children can do at any time, by age (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      
      print(p1)
    }         
    else if (input$ind_parent == "child_act" & input$btchparent == "gender_parent") {
      p1 <- ggplot(data = data_parent()) +
        geom_col(aes(x=gender_value, y= Activity, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Activities that children can do at any time, by gender (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }
    else if (input$ind_parent == "child_act" & input$btchparent == "total_parent") {
      p <- ggplot(data = data_parent()) +
        geom_col(aes(x=Total, y= Activity, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Activities that children can do at any time (%)", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }  
    else if (input$ind_parent == "proh_act" & input$btchparent == "age_parent") {
      p1 <- ggplot(data = data_parent()) +
        geom_col(aes(x=age_value, y= Activity, fill=age_group), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Activities that parents prohibit their children from engaging in, by child’s age (%) ", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      
      print(p1)
    }         
    else if (input$ind_parent == "proh_act" & input$btchparent == "gender_parent") {
      p1 <- ggplot(data = data_parent()) +
        geom_col(aes(x=gender_value, y= Activity, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Activities that parents prohibit their children from engaging in, by child’s gender (%) ", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }
    else if (input$ind_parent == "proh_act" & input$btchparent == "total_parent") {
      p <- ggplot(data = data_parent()) +
        geom_col(aes(x=Total, y= Activity, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Activities that parents prohibit their children from engaging in (%) ", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }  
    else if (input$ind_parent == "monitor" & input$btchparent == "age_parent") {
      p1 <- ggplot(data = data_parent()) +
        geom_col(aes(x=age_value, y= Monitoring_activity, fill=age_group), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parental monitoring activities practised often or very often, by child’s age (%) ", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      
      print(p1)
    }         
    else if (input$ind_parent == "monitor" & input$btchparent == "gender_parent") {
      p1 <- ggplot(data = data_parent()) +
        geom_col(aes(x=gender_value, y= Monitoring_activity, fill=gender), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parental monitoring activities practised often or very often, by child’s gender (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }
    else if (input$ind_parent == "monitor" & input$btchparent == "total_parent") {
      p <- ggplot(data = data_parent()) +
        geom_col(aes(x=Total, y= Monitoring_activity, fill = Total), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parental monitoring activities practised often or very often  (%)", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p)
    }  
   
    
  })  
  
  
  # Data  ----  
  ###############################################.
  
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "ch1" = radioButtons("rb_acc", "Access to Internet",
                                choices = c("Reasons for limited access to Internet" = "lim_internet",
                                            "Places of Internet use" = "place_access",
                                            "How often different devices are used to access the Internet " = "device_freq"
                                            ),
                                selected = "lim_internet"
           ),
           "ch2" = radioButtons("rb_digital", "Digital Skills",
                                choices = c("Websites or apps used by children" = "child_act",
                                            "Frequency of activities practised weekly or more often" = "freq_act_dig",
                                            "Most popular websites or apps among children" = "website_dig",
                                            "Children and parents who report being fairly or very confident in
a digital skill" = "skill_conf_dig"),
                                selected = "child_act"
           ),

           "ch3" = radioButtons("rb_risk", "Online Risks",
 choices = c("Children’s level of being upset by exposure to harmful content online" = "upset_lev",
"How often children felt upset by hateful and degrading messages online" = "upset_freq",
"Ways in which children were exposed to sexual content" = "means_exp",
"How children felt after seeing sexual content online" = "feeling_exp",
"Means by which children saw sexual content online" = "means_sex",
"How children felt after seeing sexual content"= "upset_lev_dis"
                                                     ),
                                         selected = "upset_lev"
           ),
           "ch4" = radioButtons("rb_parent", "Parental mediation",
                                choices = c("Parents’ awareness of children’s experience of online risks" = "parent_aware",
                                            "Parental active mediation as reported by children" = "parent_med",
                                            "Activities that parents prohibit their children from engaging in" = "proh_act",
                                            "Parental monitoring activities practised often or very often" = "monitor"),
                                selected = "parent_aware"
           ))
  })
  

  ### Reactive value for selected dataset ----
  datasetInput <- reactive({
    if (input$input_type == "ch1"){
      switch(input$rb_acc,
             "lim_internet" = lim_internet,
             "place_access" = place_access,
             "device_freq" = device_freq)
  } 
      else if(input$input_type == "ch2"){
      switch(input$rb_digital,
             "child_act" = child_act, 
             "freq_act_dig" = freq_act_dig,
             "website_dig" = website_dig,
             "skill_conf_dig" = skill_conf_dig)
      } 
    else if(input$input_type == "ch3"){
      switch(input$rb_risk,
             "upset_lev" = upset_lev, 
             "upset_freq" = upset_freq,
             "means_exp" = means_exp,
             "feeling_exp" = feeling_exp,
             "means_sex" = means_sex,
             "upset_lev_dis" = upset_lev_dis)
    } 
    else if(input$input_type == "ch4"){
      switch(input$rb_parent,
             "parent_aware" = parent_aware, 
             "parent_med" = parent_med,
             "proh_act" = proh_act,
             "monitor" = monitor)
    } 

          })
  
  ### Table of selected dataset ----

  output$table <- DT::renderDataTable(
    datasetInput(), options = list(
      lengthChange = FALSE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
        "}"))
  )
  ### Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "keyfindings.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "keyfindings.Rmd")
      file.copy("keyfindings.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
      # copy generated report
      file.copy(file, paste("rmd/", Sys.time(), ".html"))
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Parents vs Children tab  ----  
  ###############################################.   
  # Create a "data_source" reactive variable
  chparent <- reactive({
    # Return the appropriate data source depending on
    # the chosen radio button
    if (input$ind_parentchildren == "skill" & input$btchparent == "parent") {
      chparent <- skill_conf_dig %>% select("Skill", "Parents")
    } 
    else if (input$ind_parentchildren == "skill" & input$btchparent == "children") {
      chparent <- skill_conf_dig %>% select("Skill", "Children")
      
    } 
    else if (input$ind_parentchildren == "skill" & input$btchparent == "all") {
      chparent <- skill_conf_dig %>% select("Skill", "Parents", "Children") %>%
        gather("Type", "Value", "Children", "Parents")
    }
    
    else if (input$ind_parentchildren == "harm" & input$btchparent == "parent") {
      chparent <- harm_exp %>% select("Experience", "Parents")
    }
    else if (input$ind_parentchildren == "harm" & input$btchparent == "children") {
      chparent <- harm_exp %>% select("Experience", "Children")
      
    } 
    else if (input$ind_parentchildren == "harm" & input$btchparent == "all") {
      chparent <- harm_exp %>% select("Experience", "Parents", "Children") %>%
        gather("Type", "Value", "Children", "Parents") 
    }
    else if (input$ind_parentchildren == "intlim" & input$btchparent == "parent") {
      chparent <- int_limit_parent_child %>% select("Internet_limit", "Parents")
      
    } 

    else if (input$ind_parentchildren == "intlim" & input$btchparent == "children") {
      chparent <- int_limit_parent_child %>% select("Internet_limit", "Children") 
      
    } 
    else if (input$ind_parentchildren == "intlim" & input$btchparent == "all") {
      chparent <- int_limit_parent_child %>% select("Internet_limit", "Parents", "Children") %>%
        gather("Type", "Value", "Children", "Parents") 
      
    } 
    

    else if (input$ind_parentchildren == "control" & input$btchparent == "parent") {
      chparent <- parent_control %>% select("Parent_control", "Parents")
      
    } 
    else if (input$ind_parentchildren == "control" & input$btchparent == "children") {
      chparent <- parent_control %>% select("Parent_control", "Children")
      
    } 
    else if (input$ind_parentchildren == "control" & input$btchparent == "all") {
      
      chparent <- parent_control %>% select("Parent_control", "Parents", "Children") %>%
        gather("Type", "Value", "Children", "Parents")

    }
    
    else if (input$ind_parentchildren == "monitor" & input$btchparent == "parent") {
      chparent <- parent_monitor %>% select("Subject", "Parents")
      
    } 
    else if (input$ind_parentchildren == "monitor" & input$btchparent == "children") {
      chparent <- parent_monitor %>% select("Subject", "Children")
      
    } 
    else if (input$ind_parentchildren == "monitor" & input$btchparent == "all") {
      chparent <- parent_monitor %>% select("Subject", "Parents", "Children") %>%
        gather("Type", "Value", "Children", "Parents")
    }
    
 return(chparent)
  })
  
  output$chparentplot <-renderPlot({
    
    if (input$ind_parentchildren == "skill" & input$btchparent == "parent") {
    
        p1 <- ggplot(data = chparent()) +
        geom_col(aes(Parents, Skill, fill = "#0073C2FF"), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parents who report being fairly or very confident in a digital skill  (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      
      print(p1)
    }   
    
    else if (input$ind_parentchildren == "skill" & input$btchparent == "children") {
    
        p1 <- ggplot(data = chparent()) +
        geom_col(aes(Children, Skill, fill = "#0073C2FF"), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Children who report being fairly or very confident in a digital skill  (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }
    
    else if (input$ind_parentchildren == "skill" & input$btchparent == "all") {
    
        p1 <- ggplot(data = chparent()) +
        geom_col(aes(x=Value, y= Skill, fill = Type), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parents and children who report being fairly or very confident in a digital skill  (%)", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p1)
    }  
    
    else if (input$ind_parentchildren == "harm" & input$btchparent == "parent") {
      
        p1 <- ggplot(data = chparent()) +
        geom_bar(aes(Parents, Experience, fill = "#0073C2FF"), stat = "identity", position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Harmful online experiences according to parents (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }         
    else if (input$ind_parentchildren == "harm" & input$btchparent == "children") {
      
       p1 <- ggplot(data = chparent()) +
        geom_bar(aes(Children, Experience, fill = "#0073C2FF"), stat = "identity", position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Harmful online experiences according to parents (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }
    else if (input$ind_parentchildren == "harm"  & input$btchparent == "all") {
        p1 <- ggplot(data = chparent()) +
          geom_col(aes(x=Value, y= Experience, fill = Type), position = "stack")+
          theme(legend.position = "none")    + 
          labs(title = "Harmful online experiences according to parents and children (%)", 
               x = NULL, y = NULL)  +
          scale_color_manual(values = pal_simd_bar)
        print(p1)
    }  
    
    else if (input$ind_parentchildren == "intlim" & input$btchparent == "parent") {
      
      p1 <- ggplot(data = chparent()) +
        geom_bar(aes(Parents, Internet_limit, fill = "#0073C2FF"), stat = "identity", position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Children’s time limits for Internet, according to parents (%) ", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }         
    else if (input$ind_parentchildren == "intlim" & input$btchparent == "children") {
      
      p1 <- ggplot(data = chparent()) +
        geom_bar(aes(Children, Internet_limit, fill = "#0073C2FF"), stat = "identity", position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Children’s time limits for Internet, according to children (%) ", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }
    else if (input$ind_parentchildren == "intlim"  & input$btchparent == "all") {
      p1 <- ggplot(data = chparent()) +
        geom_col(aes(x=Value, y= Internet_limit, fill = Type), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Children’s time limits for Internet, according to children and parents (%) ", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p1)
    } 
    else if (input$ind_parentchildren == "control" & input$btchparent == "parent") {
      
      p1 <- ggplot(data = chparent()) +
        geom_bar(aes(Parents, Parent_control, fill = "#0073C2FF"), stat = "identity", position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parental controls over children’s Internet use, according to parents (%) ", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }         
    else if (input$ind_parentchildren == "control" & input$btchparent == "children") {
      
      p1 <- ggplot(data = chparent()) +
        geom_bar(aes(Children, Parent_control, fill = "#0073C2FF"), stat = "identity", position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parental controls over children’s Internet use, according to children (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }
    else if (input$ind_parentchildren == "control"  & input$btchparent == "all") {
      p1 <- ggplot(data = chparent()) +
        geom_col(aes(x=Value, y= Parent_control, fill = Type), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "Parental controls over children’s Internet use, according to children and parents (%) ", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p1)
    } 
    
    else if (input$ind_parentchildren == "monitor" & input$btchparent == "parent") {
      
      p1 <- ggplot(data = chparent()) +
        geom_bar(aes(Parents, Subject, fill = "#0073C2FF"), stat = "identity", position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "How children are subject to parental monitoring, often or very often, according to parents (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }         
    else if (input$ind_parentchildren == "monitor" & input$btchparent == "children") {
      
      p1 <- ggplot(data = chparent()) +
        geom_bar(aes(Children, Subject, fill = "#0073C2FF"), stat = "identity", position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "How children are subject to parental monitoring, often or very often, according to children (%)", 
             x = NULL, y = NULL)  +
        scale_fill_brewer(type = "qual", palette = "PRGn")
      print(p1)
    }
    else if (input$ind_parentchildren == "monitor"  & input$btchparent == "all") {
      p1 <- ggplot(data = chparent()) +
        geom_col(aes(x=Value, y= Subject, fill = Type), position = "stack")+
        theme(legend.position = "none")    + 
        labs(title = "How children are subject to parental monitoring, often or very often, according to children and parents (%)", 
             x = NULL, y = NULL)  +
        scale_color_manual(values = pal_simd_bar)
      print(p1)
    } 
    
    
  })  
 
  
  ############################################### 
  } #server closing bracket

#########################  END ----