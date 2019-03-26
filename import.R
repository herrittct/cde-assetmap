# get the sfa profiles from excel sheet 
library(readxl)
library(tidyverse)
library(dplyr)

profiles <- read_excel("./data/SFA_profile_Feb_2019.xls", 
                       col_types = c("text", "text", "text","logical", 
                                     "numeric","numeric", "numeric", "logical",
                                     "logical", "logical", 
                                     "logical","logical")) %>% 
  dplyr::rename(sfa_number = "Sponsor #", 
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
  mutate(free_students = free_perc * student_count) %>% 
  mutate(redu_students = redu_perc * student_count) %>% 
  mutate(free_and_red_perc = free_perc + redu_perc) %>% 
  mutate(prog_count = 0) %>% 
  mutate(prog_opportunity = 0) %>% 
  mutate(sfa_name_short7 = substr(sfa_name, 1, 7)) %>% 
  mutate(sfa_name_short4 = substr(sfa_name, 1, 4)) %>% 
  mutate(sfa_name_short10 = substr(sfa_name, 1, 10))

# The above actually takes the raw excel into a semi-usable dataframe
profiles <- filter(profiles, Profile == "Public")


#profiles <- mutate(profiles, "prog_opportunity" = if (count))

profiles$prog_count <-  
  if (profiles["sbp"] == TRUE){
    mutate(profiles, prog_count = prog_count + 1)
  }
  
# I want to specify a set of variables that I anticipate calling a number of times
#programs <- filter(profiles, sbp == TRUE) %>% 
#  filter(nslp == TRUE)


# What I want to do is calculate the number of programs they are doing
for i in programs
if smp == TRUE {
  mutate(profiles$prog_count + 1)
} else {
  mutate(fofiles$prog_opportunity +1)
  }

profiles %>% 
  group_by(free_perc > 0.85)
# I do not want to filter out these districts
programs <- profiles[sbp, nslp, smp, snack, sfsp]

programs_present <- filter(profiles, programs %in% TRUE)


#four_day <- filter(profiles, day4 == "X")
four_day <- filter(profiles, day4 == TRUE)
four_day  
dim(four_day)
has_nslp <- filter(profiles, nslp == "X")
has_nslp

has_smp <- filter(profiles, smp == "X")
has_smp


profiles
head(profiles)
profiles$sbp <- mutate(profiles, sbp = as_factor(sbp,))
profiles
  
day4.sfa <- filter(profiles, "4 Day School Week")
day4.sfa

class("Breakfast")



library(tidyverse)
library(sf)
library(tigris)
options(tigris_class = "sf")

#get all colorado school districts shape geometry etc. from the tigris package ----
colorado_school_districts <- 
  school_districts(state = "CO",
                   type = "unified")

#write it a file for use anytime and so it doesn't have to be continually downloaded for the report ----
write_rds(colorado_school_districts, "colorado_school_districts.rds")  

#there is about a billion, roughly, ways to go about this, here is one simple one. 

colorado_school_districts %>% 
  ggplot(aes(fill = str_detect(NAME, params$district))) +
  scale_fill_manual(values = c("white", "darkgreen")) +
  geom_sf() +
  guides(fill = "none") +
  labs(title = str_c(params$district, " Unified School District")) +
  theme_minimal()


head(colorado_school_districts)
dim(colorado_school_districts)
union(colorado_school_districts, profiles)
dim(union(colorado_school_districts, profiles))

union(colorado_school_districts,profiles)
