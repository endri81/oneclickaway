#Code to create One Click Away Shiny profile platform
# This script includes the user-interface definition of the app.
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
                                description = 'How children access the Internet'
                             ),
                             data.step = 2,
                             data.intro = h5(
                                "This section describes how children access the Internet, when they first start using
it, the devices they use most often to get online, where they usually access the
Internet, how much time they spend online, and any difficulties and restrictions they
might face when trying to do so."
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
                                description = 'Online activities children like'
                             ),
                             data.step = 3,
                             data.intro = h5("Here we show how children use the Internet, which activities they practise
online and the applications and websites they like and visit more often. It also looks
at children’s and parents’ online competencies, in terms of what they feel they are
good at online and what they find difficult.")
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

                             #Online Risks
                             column(
                                6,
                                class = "landing-page-column",
                                introBox(
                                   #tour of the rank and trend tabs
                                   data.step = 4,
                                   data.intro = h5(
                                      "Here we explore the risks related to potentially harmful
content and contact with people through the Internet, such as talking to strangers
online or meeting face-to-face people who were first encountered in the digital space"
                                   ),
                                lp_main_box(
                                   image_name = "online_risk",
                                   button_name = 'jump_to_risk',
                                   title_box = "Online Risks",
                                   description = 'Risks related to internet use'
                                ))
                             ),
                             #Rank/map plot box
                             column(
                                6,
                                class = "landing-page-column",
                                introBox(
                                   #tour of the rank and trend tabs
                                   data.step = 5,
                                   data.intro = h5(
                                      "This section describes the importance of parents in children’s well-being and online
safety. As parents are the ones generally expected to both enable their children’s
Internet use and protect them from harm, particular attention is given to the way in
which the children’s experiences of the Internet are mediated. "
                                   ),
                                lp_main_box(
                                   image_name = "parent_mediation",
                                   button_name = 'jump_to_parent',
                                   title_box = "Parental mediation",
                                   description = 'Importance of parents in children wellbeing'
                                ))
                             )),
                       #introBox 3 close
                       #Inequalities box
                       column(4, class = "landing-page-column",
                              introBox(
                                 data.step = 6,
                                 data.intro = h5(
                                    "Here we compare some of the results, based on the perceptions
of children and parents for different study parts."
                                 ),
                                 lp_main_box(
                                    image_name = "landing_button_health_inequality",
                                    button_name = 'jump_to_parentchildren',
                                    title_box = "Parents vs Children",
                                    description = 'Perceptions of children and parents'
                                 )
                              ) #introBox 7 close
                    )),
                    # fluid row close
                    # end of landing page second row
                    # third row of landing page
                    fluidRow(
                       introBox(
                          data.step = 7,
                          # tour around the tool
                          data.intro = h5(
                             "There are also options to find out information such as key findings, methodology, bibliography or download data"
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
                                div("Methodology", title =
                                       "Methodology", class = "landing-page-box-title"),
                                div(
                                   lp_about_box(
                                      image_name = "landing_button_technical_resources",
                                      button_name = 'jump_to_method',
                                      title_box = "Methodology",
                                      description = 'Methodology'
                                   )
                                ))),
                          column(4, class="landing-page-column", 
 #Bibliography
                                 lp_about_box(image_name= "child_identity", button_name = 'jump_to_biblio', 
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
title_box = "About", description = 'About dashboard One Click Away')

 )

)

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
                                sidebarPanel( width = 4,
                                   radioButtons("ind_access", "Choose indicator:",
                                                c("Reasons for limited access to Internet" = "reason_access",
                                                  "Places of Internet use" = "places_access",
                                                  "Internet access frequency on different devices" = "freq_access")),
                                   br(),
                                   selectInput("dissag_access", "Choose metrics:",
                                               c("By age" = "age_access",
                                                 "By gender" = "gender_access",
                                                 "Total" = "total_access"), width  ='60%')
                                ),
                                mainPanel(
                                   
                                   plotlyOutput("accplot",  width = "auto")))
                             
                          ))
                       
              ), #Tab panel bracket
              
              ###############################################.
              ## Digital Skills ----
              ###############################################.
              tabPanel("Digital Skills", icon = icon("www/area-chart"), value = "digital",
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
                                   
                                   plotlyOutput("digplot"),   width = 8))
                             
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
                                   
                                   plotlyOutput("riskplot"),   width = 7))
                             
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
                                   
                                   plotlyOutput("parentplot"),   width = 7))
                             
                          ))
              ), #Tab panel bracket              
              
              
              
              ## Demography ----
              ###############################################.
              tabPanel("Parents vs Children", icon = icon("table"), value = "parentchildren",
                       introBox(
                          fluidPage(
                             titlePanel("Parents vs Children"),
                             sidebarLayout(
                                sidebarPanel(
                                   radioButtons("ind_parentchildren", "Choose indicator:",
                                                c("Children and parents who report being fairly or very confident in a digital skill " = "skill",
                                                  "Harmful online experiences according to parents and children " = "harm",
                                                  "Children’s time limits for Internet, according to children and parents" = "intlim",
                                                  "Parental controls over children’s Internet use, according to children and parents" = "control",
                                                  "How children are subject to parental monitoring" = "monitor")),
                                   br(),
                                   selectInput("btchparent", "Choose metrics:",
                                               c("Parent" = "parent",
                                                 "Children" = "children",
                                                 "All" = "all"), width  ='60%'),
                                   width = 5
                                   
                                ),
                                mainPanel(
                                   
                                   plotlyOutput("chparentplot"),   width = 7))
                             
                          ))),#Tab panel bracket  
              
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
                                     tags$iframe(src = './keyfindings.html', # put myMarkdown.html to /www
                                                 width = '100%', height = '800px', 
                                                 frameborder = 0, scrolling = 'no'
                                     ))                              
                                  
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
                                                          "Parental mediation" = "ch4")
                                           ),
                                           br(),
                                           br(),
                                           # This outputs the dynamic UI component
                                           uiOutput("ui"),
                                           # Button
                                           downloadButton("downloadData", "Download")
                                        )),
                                        
                                        column(8,
                                               DT::dataTableOutput("table")
                                        )
                                     )
                                     
                                  )
                                  
                                  
                         ), #Tab panel bracket
                         ############## Methodology ----    
                         ###############################################.      
                         tabPanel("Methodology", value = "method",
                                  
                                  
                                  fluidPage( 
                                     tags$iframe(src = './methodology.html', # put myMarkdown.html to /www
                                                 width = '100%', height = '800px', 
                                                 frameborder = 0, scrolling = 'no'
                                     ))  
                                  
                                  
                         ), #Tab panel bracket
                         ###############################################.   
                         ##############Bibliography----    
                         ###############################################.
                         tabPanel("Bibliography", value = "biblio", 
                                  fluidPage( 
                                     tags$iframe(src = './biblio.html', # put myMarkdown.html to /www
                                                 width = '100%', height = '800px', 
                                                 frameborder = 0, scrolling = 'no'
                                     ))  
                                  
                         ),#tabPanel bracket
                         ############## Tour of the tool----    
                         ###############################################.
                         ###############################################.       
                         ## About ----
                         ###############################################.
                         tabPanel("About", value = "about",  description = 'About One Click Away',
                                  fluidPage( 
                                     tags$iframe(src = './about.html', # put myMarkdown.html to /www
                                                 width = '100%', height = '800px', 
                                                 frameborder = 0, scrolling = 'no'
                                     )) ) #Tab panel
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