#Code to create ScotPHO's Shiny profile platform
#This script includes the data manipulation necessary to produce data in the way
#the Shiny app needs.

############################.
##Filepaths ----
############################.
  lookups <- "./OneClickAway/data/"

############################.
##Packages ----
############################.
library(dplyr) 
library(tidyr)
library(readr)
library(data.table) #reading data
library(zoo) # dealing with dates
library(lubridate) #for automated list of dates in welcome modal


###############################################.
## Functions ----
