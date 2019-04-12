# get the sfa profiles from excel sheet 
library(readxl)
library(tidyverse)
library(dplyr)
library(stringr)

# Get the CDE provided spreadsheet into R

profiles <- read_excel("./data/blueprint_designed/SFA_profile_Feb_2019.xls", 
                       col_types = c("text", "text", "text","logical", 
                                     "numeric","numeric", "numeric", "logical",
                                     "logical", "logical", 
                                     "logical","logical"))

# Modify the standard excell with updated variable names
# Create a variety of new variables

profiles <- profiles %>% 
  dplyr::rename(CDE_AGREEMENT = "Sponsor #", 
         day4 = "4 Day School Week",
         student_count = "Pupil Count",
         free_perc = "Free %",
         redu_perc = "Reduced-Price %", 
         sbp = "Breakfast", 
         nslp = "Lunch",
         smp = "Milk",
         snack = "Snack",
         sfsp = "SFSP",
         sfa_name = "Sponsor Name") %>% 
  mutate(sfa_name = str_to_lower(sfa_name)) %>% 
  mutate(free_students = free_perc * student_count) %>% # count of free students
  mutate(redu_students = redu_perc * student_count) %>% # count of redu students
  mutate(free_and_red_perc = free_perc + redu_perc) # combined free & redu


# The above actually takes the raw excel into a semi-usable dataframe
profiles <- filter(profiles, Profile == "Public")

# Import all the schools with GEOID!
# Must have current directory above data

site_all <- read_excel("./data/CDE_to_GEOID.xlsx", col_types = "text") %>% 
  mutate(GEOID = ACTUAL_GEOID) %>% 
  select(GEOID, CDE_AGREEMENT, everything(), -ACTUAL_GEOID)

site_all <- group_by(site_all,
                      CDE_AGREEMENT)

# Gets all the districts with their site count and GEOID
(districts <- summarise(site_all, 
                       count_schools = n(),
                       GEOID = first(GEOID)))

# Combines districts with GEOID number
geoid_districts <- left_join(districts, profiles, by = "CDE_AGREEMENT")


source("./mapping.r")
### This WORKS!!
combined_geo_dist <- left_join(co_school_joined, geoid_districts, by = "GEOID")

plot(combined_geo_dist["nslp"], main = "NSLP Updated", key.pos = 3 )
plot(combined_geo_dist["sfsp"], main = "SFSP 2018")

# I am thinking that there is something to do with how the df is getting merged 
# that is making it lose the ability to map, no idea why though. 


