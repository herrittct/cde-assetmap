# Messing around with guidance from Data Camp
# https://s3.amazonaws.com/assets.datacamp.com/production/course_6839/slides/chapter4.pdf



library(tigris)
library(RQGIS)
library(tidycensus)
library(sf)
library(dplyr)
library(lwgeom)
library(stringr)
options(tigris_class = "sf")
tigris_use_cache = TRUE
apikey <- "69d8405e34b271517d234dcd4689e8df75836eff"


colorado_income <- get_acs(geography = "school district (unified)",
                           variables = "B19013_001","B09010",
                           state = "CO", key = apikey)

colorado_income <- get_acs(geography = "school district (unified)",
                           variables = "B09010",
                           state = "CO", key = apikey)

colorado_school <- school_districts(state = "CO",
                                    type = "unified",
                                    class = "sf")
merge(colorado_school,
      colorado_income,
      by = "GEOID")


co_school_joined <- left_join(colorado_school,
                              colorado_income,
                              by = "GEOID")



# What I need to work on is taking the names apart and trying to match them automatically
# Create a new variable in profiles from the geoID so that all the districts match


