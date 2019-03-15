# get the sfa profiles from excel sheet 
profiles <- read_excel("./data/SFA_profile_Feb_2019.xls") %>% 
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
  mutate(free_students = free_perc * student_count) %>% 
  mutate(redu_students = redu_perc * student_count) 

four_day <- filter(profiles, day4 == "X")
four_day  
dim(four_day)

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

head(colorado_school_districts)
dim(colorado_school_districts)
union(colorado_school_districts, profiles)
dim(union(colorado_school_districts, profiles))
