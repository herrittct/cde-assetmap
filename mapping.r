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

co_school_joined <- dplyr::rename(co_school_joined, 
                           sfa_name = "NAME.x") %>% 
  mutate(sfa_name = str_to_lower(sfa_name))

co_school_joined <-
  mutate(co_school_joined, sfa_name_short7 = substr(sfa_name,1,7))

# Not currently using the below substrings but may need to refine and use later
co_school_joined <-
  mutate(co_school_joined, sfa_name_short10 = substr(sfa_name,1,10))

co_school_joined <-
  mutate(co_school_joined, sfa_name_short4 = substr(sfa_name,1,4))
  
# Trying to add in data from our CDE information
# THIS ONE WORKS!! For most of the districts 167/178
super_df <- inner_join(co_school_joined,
                       profiles,
                       by = "sfa_name_short7")


# This one causes problems but points out another problem regarding non-pub
super_df4 <- inner_join(co_school_joined,
                        profiles,
                        by = "sfa_name_short4")


super_df10 <- inner_join(co_school_joined,
                        profiles,
                        by = "sfa_name_short10")


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

head(co_school_joined)





