# Code to create OneClickAway Shiny profile platform
# In this script include packages, functions, datasets and anyting that will be 
# used both by UI and server

############################.
##Packages ----
############################.
library(shiny)
library(shinyBS) #modals
library(gridExtra)
library(ggthemes)
library(readr)
library(shinythemes) # layouts for shiny
library(rmdformats)
library(tidyverse) # data manipulation
library (DT) # for data tables
library(plotly) #interactive graphs
library(shinyWidgets) # for extra widgets
library(tibble) # rownames to column in techdoc
library(shinyBS) #modals
library(shinyjs)
library(shinydashboard) #for valuebox on techdoc tab
library(shinycssloaders) #for loading icons, see line below
library(rmarkdown)
library(flextable) #for tech document table
library(rintrojs) # for help intros
library(rmarkdown) # for help intros
library(knitr)
library(knitrBootstrap)
library(ggplot2)
library(thematic)
library(ggeasy)
library(RColorBrewer)


thematic_on(
  bg = "#2FA4E7", fg = "#FFFAFA", accent = "#0CE3AC", font = "Lato") 






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
pal_simd_bar <- c("#DB5BFF", "#EFBDFF", "#1E90FF", "#14ADB4", "#ADD8E6", "#77DD77", "#83C442", "#A4D467", "#D9FFB1", "#FF5353", "#FF9933", "#FFC797", "#FDFD96")



###############################################.
## Plot parameters ----
###############################################.

#Common parameters for plots
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14), 
                    showline = TRUE, tickangle = 270, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4, 
                    tickfont = list(size=14), titlefont = list(size=14)) 

font_plots <- list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')


## Data Loading
### Internet Access
lim_internet <- read_csv("data/lim_internet.csv")
device_freq <- read_csv("data/device_freq.csv")
place_access <- read_csv("data/place_access.csv")

## Digital Skills

freq_act_dig <- read_csv("data/freq_act_dig.csv")
website_dig <- read_csv("data/website_dig.csv")
skill_conf_dig <- read_csv("data/skill_conf_dig.csv")


## Online risks

parent_perc <- read_csv("data/parent_perc.csv")
means_sex <- read_csv("data/means_sex.csv")
means_exp <- read_csv("data/means_exp.csv")
parent_aware <- read_csv("data/parent_aware.csv")
upset_level <- read_csv("data/upset_level.csv")
upset_level_dis <- read_csv("data/upset_level_dis.csv")
upset_freq <- read_csv("data/upset_freq.csv")
feeling_exp <- read_csv("data/feeling_exp.csv")



## Parental mediation

child_act <- read_csv("data/child_act.csv")
parent_med <- read_csv("data/parent_med.csv")
proh_act <- read_csv("data/proh_act.csv")
monitor <- read_csv("data/monitor.csv")



## Parents vs children

harm_exp <- read_csv("data/harm_exp.csv", na = "0")
int_limit_parent_child <- read_csv("data/int_limit_parent_child.csv", na = "0")
parent_control <- read_csv("data/parent_control.csv", na = "0")
parent_monitor <- read_csv("data/parent_monitor.csv", na = "0")






















