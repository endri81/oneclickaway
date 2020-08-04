# Code to create OneClickAway Shiny profile platform
# In this script include packages, functions, datasets and anyting that will be 
# used both by UI and server

############################.
##Packages ----
############################.
library(shiny)
library(shinyBS) #modals
library(shinythemes) # layouts for shiny
library(dplyr) # data manipulation
library(ggplot2) #data visualization
library (DT) # for data tables
library(leaflet) #javascript maps
library(plotly) #interactive graphs
library(shinyWidgets) # for extra widgets
library(tibble) # rownames to column in techdoc
library(shinyBS) #modals
library(shinyjs)
library(shinydashboard) #for valuebox on techdoc tab
library(sp)
library(lubridate) #for automated list of dates in welcome modal
library(shinycssloaders) #for loading icons, see line below
library(rmarkdown)
library(flextable) #for tech document table
library(webshot) #to download plotly charts
library(rintrojs) # for help intros
# As well as webshot phantomjs is needed l to download Plotly charts
# https://github.com/rstudio/shinyapps-package-dependencies/pull/180
if (is.null(suppressMessages(webshot:::find_phantom()))) {
  webshot::install_phantomjs()
}



#Function to wrap titles, so they show completely when saving plot in ggplot
title_wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

#Function to create plot when no data available
plot_nodata <- function(height_plot = 450) {
  text_na <- list(x = 5, y = 5, text = "No data available" , size = 20,
                  xref = "x", yref = "y",  showarrow = FALSE)
  
  plot_ly(height = height_plot) %>%
    layout(annotations = text_na,
           #empty layout
           yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')) %>% 
    config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
} 

#Function to create plot when no data available for ggplot visuals
plot_nodata_gg <- function() {
  ggplot()+
    xlab("No data available")+
    scale_x_discrete(position = "top")+
    theme(panel.background = element_blank(),
          axis.title.x=element_text(size=20, colour ='#555555'))
}


# UI for heatmap and snapshot plots
sum_ui <- function(title, plot_name) {
  tagList(
    h5(title, style="color: black; text-align: center; font-weight: bold;"),
    div(align = "center", withSpinner(plotlyOutput(plot_name, height = "auto")))
  ) }

# Indicator definition boxes for indicator definition tab
ind_def_box <- function(label, text_output) {
  div(class="definitionbox",
      p(paste(label), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
      h5(style = "color: black", textOutput(text_output)))
}

#Creating big boxes for main tabs in the landing page (see ui for formatting css)
lp_main_box <- function(title_box, image_name, button_name, description) {
  div(class="landing-page-box",
      div(title_box, class = "landing-page-box-title"),
      div(description, class = "landing-page-box-description"),
      div(class = "landing-page-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
      actionButton(button_name, NULL, class="landing-page-button")
      )
}


#Creating small boxes for further information in the landing page (see ui for formatting css)
lp_about_box <- function(title_box, image_name, button_name, description) {

  div(class="landing-page-box-about",
      div(title_box, class = "landing-page-box-title"),
      div(class = "landing-page-about-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
          (actionButton(button_name, NULL,
                   class="landing-page-button",
                   icon = icon("arrow-circle-right", "icon-lp"),title=description)))
}


  
###############################################.
## Palettes ----
###############################################.   
#Palette for SIMD.
pal_simd_bar <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031')
pal_simd_trend <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031', '#FF0000')

#Palette for map
pal_map <- c('#2c7bb6','#abd9e9', '#ffffbf','#fdae61','#d7191c')

###############################################.
## Plot parameters ----
###############################################.

#Common parameters for plots
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14), 
                    showline = TRUE, tickangle = 270, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4, 
                    tickfont = list(size=14), titlefont = list(size=14)) 

font_plots <- list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')


