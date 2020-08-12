#Code to create One Click Away Shiny profile platform
# This script includes the user-interface definition of the app.
source("online_act.R")
###############################################.
## Header ---- 
###############################################.
tagList( #needed for shinyjs
   useShinyjs(),  # Include shinyjs
   introjsUI(),   # Required to enable introjs scripts
   navbarPage(id = "intabset", #needed for landing page
              title = div(tags$a(img(src="unicef_logo.png", height=40), href= "https://www.unicef.org/albania/"),
                          style = "position: relative; top: -5px;"), # Navigation bar
              windowTitle = "One Click Away", #title for browser tab
              theme = shinytheme("cerulean"), #Theme of the app (blue navbar)
              collapsible = TRUE, #tab panels collapse into menu in small screens
              header =         
                 tags$head( #CSS styles
                    tags$link(rel="shortcut icon", href="unicef_icon.ico"), #Icon for browser tab
                    includeCSS("www/styles.css"),
                    HTML("<base target='_blank'>") # to make external links open a new tab
                 ),
              ###############################################.
              ## Landing page ---
              ###############################################.
              tabPanel(
                 title = " Home",
                 icon = icon("home"),
                 mainPanel(
                    width = 11,
                    style = "margin-left:4%; margin-right:4%",
                    introBox(
                       fluidRow(column(7, (
                          h3("Welcome to One Click Away", style = "margin-top:0px;")
                       )),
                       (column(
                          4,
                          actionButton(
                             "btn_landing",
                             label = "Help: Take tour of the tool",
                             icon = icon('question-circle'),
                             class = "down"
                          )
                       ))),
                       data.step = 1,
                       data.intro = (
                          p(
                             h4("Welcome to One Click Away"),
                             h5(
                                "The survey explores the experiences of children across various dimensions of their
use of the Internet and generates and sustains a rigorous cross-national comparative
evidence base. In addition, the study explores the Internet use of parents and to
what extent they mediate their children’s online experiences."
                             ),
                             br(),
                             h5("There are different ways to navigate around the tool."),
                             h5(
                                "Different visualisations can be opened using the menu bar (the blue strip) at the top of the screen."
                             ),
                             img(src = 'introjs_tabset_panel.PNG', width =
                                    300),
                             br(),
                             h5(
                                "The 'Home' option in the menu bar will return to the profiles tool homepage."
                             ),
                             style = "color:0E3E5D; font-size:20px"
                          )
                       ),
                       data.position = "left"
                    ),
                    fluidRow(
                       #Summary box
                       column(
                          6,
                          class = "landing-page-column",
                          br(),
                          #spacing
                          introBox(
                             lp_main_box(
                                image_name = "internet_access",
                                button_name = 'jump_to_int_acc',
                                title_box = "Internet Access",
                                description = 'A high level view of an area across a set of indicators'
                             ),
                             data.step = 2,
                             data.intro = h5(
                                "The profile summary allows you to look at multiple indicators within an area at the same time"
                             ),
                             data.position = "bottom-right-aligned"
                          )
                       ),
                       #Table box
                       column(
                          6,
                          class = "landing-page-column",
                          br(),
                          #spacing
                          introBox(
                             # tour of the tool
                             lp_main_box(
                                image_name = "online_act",
                                button_name = 'jump_to_digital',
                                title_box = "Digital Skills",
                                description = 'View and download the data behind the tool'
                             ),
                             data.step = 6,
                             data.intro = h5("The 'Data' window can be used to filter and download profiles data")
                          )
                       )
                    ),
                    #2nd row of boxes
                    fluidRow(
                       br(),
                       #spacing
                       column(
                          8,
                          style = "padding-left: 0px; padding-right: 0px;",
                          introBox(
                             #tour of the rank and trend tabs
                             data.step = 3,
                             data.intro = h5(
                                "The trend and rank charts allow detailed exploration of one indicator at a time."
                             ),
                             #Trend plot box
                             column(
                                6,
                                class = "landing-page-column",
                                lp_main_box(
                                   image_name = "online_risk",
                                   button_name = 'jump_to_risk',
                                   title_box = "Online Risks",
                                   description = 'Look at how an indicator changes over time'
                                )
                             ),
                             #Rank/map plot box
                             column(
                                6,
                                class = "landing-page-column",
                                lp_main_box(
                                   image_name = "parent_mediation",
                                   button_name = 'jump_to_parent',
                                   title_box = "Parental mediation",
                                   description = 'Compare geographical variation for an indicator'
                                )
                             )
                          )
                       ),
                       #introBox 3 close
                       #Inequalities box
                       column(4, class = "landing-page-column",
                              introBox(
                                 data.step = 7,
                                 data.intro = h5(
                                    "The inequalities module allows exploration of deprivation effects for a selection of indicators from the main profiles tool."
                                 ),
                                 lp_main_box(
                                    image_name = "landing_button_health_inequality",
                                    button_name = 'jump_to_demo',
                                    title_box = "Demography",
                                    description = 'Explore how an indicator varies with deprivation'
                                 )
                              )) #introBox 7 close
                    ),
                    # fluid row close
                    # end of landing page second row
                    # third row of landing page
                    fluidRow(
                       introBox(
                          data.step = 8,
                          # tour around the tool
                          data.intro = h5(
                             "There are also options to find out information such as detailed descriptions of the profile indicators, indicator update schedules and links to evidence for action briefings"
                          ),
                          #Key Findings
                          column(
                             4,
                             class = "landing-page-column",
                             lp_about_box(
                                image_name = "landing_button_related_links",
                                button_name = 'jump_to_kfind',
                                title_box = "Key Findings",
                                description = 'Key Findings'
                             ),
                             #Methodology
                             div(
                                class = "landing-page-box-about",
                                div("Methodology", title =
                                       "Methodology", class = "landing-page-box-title"),
                                div(class = "landing-page-about-icon", div(
                                   img(src = "landing_button_other_profile.png", class = "centerabout")
                                )),
                                actionButton(
                                   'jump_to_method',
                                   'Methodology',
                                   onclick =
                                      "window.open('https://www.unicef.org/albania/documents/one-click-away', '_blank')",
                                   class = "landing-page-button",
                                   
icon = icon("arrow-circle-right", "icon-lp")))),
column(4, class="landing-page-column", 

#Bibliography
lp_about_box(image_name= "child_identity", button_name = 'btn_indicator_updates', 
title_box = "Bibliography", 
description = 'Bibliography'),

#Evidence
lp_about_box(image_name= "landing_button_resources", button_name = 'jump_to_evidence', 
title_box = "Evidence for Action", 
description = 'Evidence for Action')),
column(4, class="landing-page-column",

#Data
lp_about_box(image_name= "landing_button_data_table",
button_name = 'jump_to_data', title_box = "Data", 
description = 'Find out about data used'),
                                          
#About
lp_about_box(image_name= "landing_button_about_2", button_name = 'jump_to_about', 
title_box = "About", description = 'About dashboard One Click Away'))
                              ) #Close IntroBox
                           )#Fluidrow bracket
                 ) #main Panel bracket
              ),# tab panel bracket
              ###############################################.
              ## Access to Internet ----
              ###############################################.
              tabPanel("Access to Internet", icon = icon("list-ul"), value = "acc_int",
                       introBox(
                          fluidPage(
                             titlePanel("Access to Internet"),
                             sidebarLayout(
                                sidebarPanel(
                                    radioButtons("ind_access", "Choose indicator:",
  c("Reasons for limited access to Internet" = "reason_access",
     "Places of Internet use" = "places_access",
     "Internet access frequency on different devices" = "freq_access")),
  br(),
  selectInput("dissag_access", "Choose metrics:",
              c("By age" = "age_access",
                "By gender" = "gender_access",
                "Total" = "total_access"), width  ='60%'),
  width = 4
                                    
                                    ),
  mainPanel(

                                    plotOutput("accplot"),   width = 8))
     
                                 ))
                       
              ), #Tab panel bracket
              
              ###############################################.
              ## Digital Skills ----
              ###############################################.
              tabPanel("Digital Skills", icon = icon("area-chart"), value = "digital",
                       introBox(
                          fluidPage(
                             titlePanel("Online activities and digital skills"),
                             sidebarLayout(
                                sidebarPanel(
            radioButtons("ind_digital", "Choose indicator:",
            c("Websites or apps used by children" = "web_digital",
             "Frequency of activities practised weekly or more often" = "freq_act_dig",
             "Children confidence in a digital skill" = "conf_digital")),
                                   br(),
                                   selectInput("dissag_digital", "Choose metrics:",
                                               c("By age" = "age_digital",
                                                 "By gender" = "gender_digital",
                                                 "Total" = "total_digital"), width  ='60%'),
                                   width = 4
                                   
                                ),
                                mainPanel(
                                   
                                   plotOutput("digplot"),   width = 8))
                             
                          ))
                       
              ), #Tab panel bracket
              ###############################################.
              ## Online risks ---- 
              ###############################################.
              tabPanel("Online risks", icon = icon("signal"), value = "risk", #Tab panel bracket
              introBox(
                 fluidPage(
                    titlePanel("Online risks and potential harm"),
                    sidebarLayout(
                       sidebarPanel(
                          radioButtons("ind_risk", "Choose indicator:",
c("Children’s level of being upset by exposure to harmful content online" = "upset_level",
"How often children felt upset by hateful and degrading messages online" = "upset_freq",
"Ways in which children were exposed to sexual content" = "ways_exp",
"How children felt after seeing sexual content online" = "way_feel",
"Means by which children saw sexual content online" = "means_sex",
"Children level of feeling upset after seeing sexual content"= "upset_level_sex",
"Parents’ awareness of children’s experience of online risks" = "parent_aware"
)),
                          br(),
                          selectInput("dissag_risk", "Choose metrics:",
                                      c("By age" = "age_risk",
                                        "By gender" = "gender_risk",
                                        "Total" = "total_risk"), width  ='60%'),
                          width = 5
                          
                       ),
                       mainPanel(
                          
                          plotOutput("riskplot"),   width = 7))
                    
                 ))

), #Tab panel bracket
              ## Parental mediation ---- 
              ###############################################.
              tabPanel("Parental mediation", icon = icon("balance-scale"), value = "parent", #Tab panel bracket
                       introBox(
                          fluidPage(
titlePanel("Parental mediation of children’s online experience"),
                             sidebarLayout(
                                sidebarPanel(
                                   radioButtons("ind_parent", "Choose indicator:",
c("Parental active mediation as reported by children" = "parent_med",
"Activities that children can do at any time" = "child_act",
"Activities that parents prohibit their children from engaging in" = "proh_act",
"Parental monitoring activities practised often or very often" = "monitor"
                                                )),
                                   br(),
                                   selectInput("dissag_parent", "Choose metrics:",
                                               c("By age" = "age_parent",
                                                 "By gender" = "gender_parent",
                                                 "Total" = "total_parent"), width  ='60%'),
                                   width = 5
                                   
                                ),
                                mainPanel(
                                   
                                   plotOutput("parentplot"),   width = 7))
                             
                          ))
              ), #Tab panel bracket              
              
              
              
              ## Demography ----
              ###############################################.
              tabPanel("Demography", icon = icon("table"), value = "table"), #Tab panel bracket  
              
              ###############################################.             
              ##############NavBar Menu----
              ###############################################.
              #Starting navbarMenu to have tab with dropdown list
              navbarMenu("Info", icon = icon("info-circle"),
                         ###############################################.
                         ## Key Findings ----
                         ###############################################.
                         tabPanel("Key Findings", value = "kfind",
                                  fluidPage(
                                     
                                           
                                     includeMarkdown("keyfindings.Rmd"))
                                     
                                 
              ), #Tab panel bracket   
                         ###############################################.             
                        ##############Data----    
                         ###############################################. 
                         tabPanel("Data", value = "data",
                                  fluidPage(
                                     
                                     titlePanel("Downloading Data"),
                                     
                                     fluidRow(
                                        
                                        column(3, wellPanel(
                                           selectInput("input_type", "Choose study chapter",
                                                       c( "Access to Internet" = "ch1",
                                                         "Digital Skills" = "ch2",
                                                         "Online risks" = "ch3",
                                                         "Parental mediation" = "ch4" 
                                                       )
                                           ),
                                        br(),
                                        br(),
                                           # This outputs the dynamic UI component
                                           uiOutput("ui")
                                        )),
                                        
                                        column(3,
                                               tags$p("Dynamic input value:"),
                                               verbatimTextOutput("dynamic_value")
                                        )
                                     )
                                        
                                     )

                                 
                         ), #Tab panel bracket
                         ############## Methodology ----    
                         ###############################################.      
                         tabPanel("Methodology", value = "method"), #Tab panel bracket
                        ###############################################.   
                         ###############################################.             
                         ##############Peers and community----    
                         ###############################################.
                         tabPanel("Bibliography", value = "biblio"), #tabPanel bracket
                         ############## Tour of the tool----    
                         ###############################################.
                         tabPanel("Tour of the tool", value = "tour"), #tab panel bracket
                         ###############################################.       
                         ## About ----
                         ###############################################.
                         tabPanel("About", value = "about")#Tab panel
                         ###############################################.
              )# NavbarMenu bracket
   ), #Bracket  navbarPage
   div(style = "margin-bottom: 30px;"), # this adds breathing space between content and footer
   ###############################################.             
   ##############Footer----    
   ###############################################.
   #Copyright warning
   tags$footer(column(6, "© Unicef Albania 2020"), 
               column(2, tags$a(href="mailto:eraco@unicef.org", tags$b("Contact us!"), 
                                class="externallink", style = "color: white; text-decoration: none")), 
               column(3), 
               column(1, actionLink("twitter_share", label = "Share", icon = icon("twitter"),
                                    style= "color:white;", onclick = sprintf("window.open('%s')", 
                                                                             "https://twitter.com/albania_unicef/"))), 
               style = "
   position:fixed;
   text-align:center;
   left: 0;
   bottom:0;
   width:100%;
   z-index:1000;  
   height:30px; /* Height of the footer */
   color: white;
   padding: 10px;
   font-weight: bold;
   background-color: #1995dc"
   ) 
   ################################################.
) #bracket tagList
###END