#Code to create One Click Away Shiny profile platform
#This script includes the data manipulation necessary to produce data in the way
#the Shiny app needs.

############################.
##Filepaths ----
############################.

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
