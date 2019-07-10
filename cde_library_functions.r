#  CDE Mapping Library and Functions

# If running for the first time, you must install packages first
#source("./cde_install_packages.r") 

library(tidyverse)
library(proto)
library(gsubfn)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(tigris)
library(reticulate)
library(RQGIS)
library(tidycensus)
library(sf)
library(forcats)
library(leaflet)
library(ggmap)
library(curl)

# Functions

# need to change meal type from breakfast/lunch/snack/milk to abbrev

rename_meal_types <-  function(variable){case_when(
  variable == "Breakfast" ~"SBP",
  variable == "Lunch" ~"NSLP", 
  variable ==  "Snack" ~"ASP",
  variable == "Milk" ~"SMP",
  variable == "SFSP_BREAKFAST_SELFPREP" ~"SFSP_BREAKFAST",
  variable == "SFSP_BREAKFAST_VENDED" ~"SFSP_BREAKFAST",
  variable == "SFSP_SNACK_AM_SELFPREP" ~"SFSP_SNACK",
  variable == "SFSP_SNACK_PM_SELFPREP" ~"SFSP_SNACK",
  variable == "SFSP_LUNCH_SELFPREP" ~"SFSP_LUNCH",
  variable == "SFSP_LUNCH_VENDED" ~"SFSP_LUNCH",
  variable == "SFSP_SELFPREP" ~"SFSP_UNKNOWN",
  variable == "SFSP_LUNCH_SELFPREP & SFSP_AM SNACK_SELFPREP" ~"SFSP_MIXED",
  variable == "SFSP_SUPPER_SELFPREP" ~"SFSP_SUPPER" )
}


# Function for doing a month and specific meal type from a dataframe      
Make_Monthly <- function(df, needed_months, meal_types){
  filter(df,
         claim_month == needed_months &
           meal_type == meal_types)
}
# Make a plot by variable and map title
districts_plot <- function(df, variable, map_title){
  plot(df[variable], main = map_title)
}



# function to change SY1011 to 2010 (as.numeric)
SY_YY_XX <- function(variable) {
  str_replace(variable, "^SY", "") %>%            
    str_remove(pattern = "\\d\\d$") %>% 
    as.numeric() + 2000
}

Score_District_Size <- function(size_variable){
  case_when(
    size_variable < 50 ~0.33,
    between(size_variable, 50, 500) ~0.67,
    between(size_variable, 501, 1000) ~1,
    between(size_variable, 1001, 5000) ~1.34,
    between(size_variable, 5001, 10000) ~1.67,
    size_variable > 10000 ~2)
  
}

Pad_to_character <-  function(input_variable){
  str_pad(input_variable, width = 4, 
          side = "left", pad = "0")}

Score_ruralurban <- function(variable){
  case_when(
    variable == "Rural" ~0,
    variable == "Urban" ~1,
    variable == "Frontier" ~-1)
}


id_to_sy <- function(id){
  2009 + id
}


na_function <- function(x){(na_count <-sapply(x, # place the dataframe here
                   function(y) sum(length(which(is.na(y))))) %>%
  data.frame())

}


x_to_true <- function( variable) {
  case_when(
    variable == "X" ~TRUE,
    variable == "x" ~TRUE,
  )
}

### Interesting ACS variables
# B09010