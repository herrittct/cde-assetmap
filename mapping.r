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
                           variables = "B19013_001",
                           state = "CO", key = apikey)

colorado_school <- school_districts(state = "CO",
                                    type = "unified",
                                    class = "sf")

co_school_joined <- left_join(colorado_school,
                              colorado_income,
                              by = "GEOID")


# Plot out using GIS all the different SFAs with whatever data we want to show



plot(super_df["free_perc"], main="Percentage Free Students")
plot(super_df["redu_perc"], main= "Percentage Reduced Students")
plot(super_df["free_and_red_perc"], main= "Percentage Free or Reduced")
plot(super_df["sbp"], main= "Has School Breakfast Program")
plot(super_df["day4"], main= "Has 4 day school week")
plot(super_df["sfsp"], main = "Has sfsp")
plot(super_df["smp"], main = "Has Special Milk Progam")
plot(super_df["nslp"], main = "Has NSLP")
plot(super_df["snack"], main= "Has Afterschool Snack Program")
plot(super_df["student_count"], main= "Number of Students")

# What I need to work on is taking the names apart and trying to match them automatically
# Create a new variable in profiles from the geoID so that all the districts match

