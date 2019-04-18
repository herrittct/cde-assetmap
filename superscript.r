#Super Script!
  
#I want to run all of the analysis needed from one script, a large combination of all the above scripts

# import.r
# mapping.r
# mealcount.rwar
# four_day.r
# mealcountimport.r

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
library(lwgeom)
library(forcats)




# From mapping.r





# Messing around with guidance from Data Camp
# https://s3.amazonaws.com/assets.datacamp.com/production/course_6839/slides/chapter4.pdf


options(tigris_class = "sf")
tigris_use_cache = TRUE
apikey <- "69d8405e34b271517d234dcd4689e8df75836eff"

# this is a stand in for census variables 
colorado_income <- get_acs(geography = "school district (unified)",
                           variables = "B19013_001",
                           state = "CO", key = apikey)

colorado_school <- school_districts(state = "CO",
                                    type = "unified",
                                    class = "sf")

co_school_joined <- left_join(colorado_school,
                              colorado_income,
                              by = "GEOID")








# From import.r

# get the sfa profiles from excel sheet 
# Get the CDE provided spreadsheet into R

# really should do this like I have on the otherones with a named list
# then import from there but that is not a priority today

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




### This WORKS!!
combined_geo_dist <- left_join(co_school_joined, geoid_districts, by = "GEOID")

plot(combined_geo_dist["nslp"], main = "NSLP Updated", key.pos = 3 )
plot(combined_geo_dist["sfsp"], main = "SFSP 2018")

# I am thinking that there is something to do with how the df is getting merged 
# that is making it lose the ability to map, no idea why though. 


#---------------------------------------------------------------

  


#-----------------------------------------------------------

#from mealcount.r


# 
# library(lubridate)
# library(forcats)
# library(tidyverse)

# Import the monthly meal count data to see what is looks like
meal_count_excel <- paste0("C:/Users/CHerr/OneDrive/Documents/CDEMapping",
                           "/cde-assetmap/data/school_year_programs/",
                           "Monthlymealcount_1718.xls")

raw_monthly_meal_count <- read_excel(meal_count_excel)

# This all works to change the SFA number to a 4 digit character
monthly_meal_count <- rename(raw_monthly_meal_count,
                             sfa_num = "SFA #",
                             site_num = "Site #",
                             sfa_name = "School Food Authority Name",
                             site_name = "Site Name",
                             claim_date = "Claim Date",
                             days_served_count = "DaysServedQty",
                             rate_level = "Rate Level",
                             meal_type = "Meal Type") %>% 
  select(everything(), -"Earning Type") %>% 
  mutate(claim_date = ymd(claim_date),
         year = year(claim_date),
         month = month(claim_date))

# Creates characters that can match with with the other character plots 
monthly_meal_count$sfa_num <- str_pad(monthly_meal_count$sfa_num,
                                      width = 4, 
                                      side = "left",
                                      pad = "0")
monthly_meal_count$site_num <- str_pad(monthly_meal_count$site_num,
                                       width = 4, 
                                       side = "left",
                                       pad = "0")

# this splits it but makes it complicated becuase it is the same school year
meal_count2017 <- filter(monthly_meal_count, year == 2017)
meal_count2018 <- filter(monthly_meal_count, year == 2018)


# Need to mutate the meal type into different vectors

  
  # Maps the total meals served over the course of the year by each sfa
  # This kinda stuff is really cool
  monthly_meal_count %>% 
  group_by(sfa_num) %>% 
  ggplot() +
  geom_point(aes(y = Total_Meals_Served, x = claim_date), position = "jitter") +
  facet_wrap(~rate_level, nrow = 2) +
  geom_smooth(aes(y = Total_Meals_Served, x = claim_date))

meal_count2017 %>% 
  ggplot() +
  geom_point(aes(y = Total_Meals_Served, x = claim_date), position = "jitter")

monthly_meal_count %>% 
  ggplot() +
  geom_point(aes(y = Total_Meals_Served, x = claim_date), position = "jitter")


#------------------------------------------------------------------------



# four_day
# library(tidyverse)
# library(readxl)

directory_four_day <- list.dirs("./data/four_day")
file_four_day <- list.files("./data/four_day")
path_four_day <- paste0(directory_four_day,"/", file_four_day)

path_four_day
# creates col_type = col_name list to be used for import. 
column_names_four_day <- c("text" = "school_year",
                           "text" = "sfa_num",
                           "text" = "sfa_name", 
                           "text" = "site_num",
                           "text" ="site_name",
                           "text" ="meal_type",
                           "numeric" = "pk_12_enrollment",
                           "text" = "free_perc",
                           "text" = "redu_perc",
                           "text" = "free_and_redu_perc",
                           "text" = "four_day",
                           "text" = "county",
                           "text" = "urban_rural")

# imports the file with appropriate types and names
imported_four_day <- read_excel(path_four_day, 
                                col_names = column_names_four_day,
                                col_types = names(column_names_four_day),
                                skip = 1)

# creates a working file to prevent need to reload. 
# Selects only the variables needed for this analysis
working_four_day <- imported_four_day %>% 
  select(school_year, 
         sfa_num, 
         site_num, 
         four_day, 
         urban_rural)

# This works to mutate the variable into usable numeric class
mutate_four_day <- mutate(working_four_day, week_length = case_when(
  four_day == "T-F" ~4, 
  four_day == "M-Th" ~4, 
  four_day == "N" ~5)) 

# Summarize by week length
sum_mutate_four_day <- group_by(mutate_four_day, school_year, sfa_num) %>% 
  summarize(week_length = mean(week_length))

# create the score 4-day score and removes extra variables
score_four_day <- mutate(sum_mutate_four_day, score_4day = case_when(
  week_length == 5 ~1,
  week_length == 4 ~0)) %>% 
  select(everything(),-week_length)


#------------------------------------------------------------------------

#------------------------------------------------------------------------

# Meal counting importing

# Import all the meal counts AHH so many observations!
# library(gsubfn)
# library(readxl)
# library(tidyverse)
# library(dplyr)
# library(stringr)
# library(lubridate)

# need to change meal type from breakfast/lunch/snack/milk to abbrev
# needs to be done to the overall starting point eventually but can start here
rename_meal_types <-  function(variable){case_when(
  variable == "Breakfast" ~"SBP",
  variable == "Lunch" ~"NSLP", 
  variable ==  "Snack" ~"ASP",
  variable == "Milk" ~"SMP")
}


# Function for doing a month and specific meal type from a dataframe      
Make_Monthly <- function(df, needed_months, meal_types){
  filter(df,
         claim_month == needed_months &
           meal_type == meal_types)
}

list.files("./data/meal_count", pattern = "^SY")
meal_count_directory <-  "./data/meal_count/"
count_files <- list.files("./data/meal_count", pattern = "^SY")

# creates a list of the matching files path
count_files <- paste0(meal_count_directory,count_files)

# Creates list of with col_type = col_name, use names() to use the col_type
meal_count_column_unknown <- c("text" = "sfa_num",
                               "text" = "sfa_name", 
                               "text" = "site_num",
                               "text" ="site_name", 
                               "date"= "claim_date", 
                               "text" ="meal_type",
                               "text" = "earning_type",
                               "text" = "rate_level",
                               "text" = "provision",
                               "numeric" = "count_days_served", 
                               "numeric" = "free_meals_served", 
                               "numeric" = "reduced_meals_served", 
                               "numeric" = "paid_meals_served", 
                               "numeric" = "total_meals_served")

# Imports all the files into a large list of lists
import_meal_count <-lapply(count_files, 
                           read_excel,
                           range = "a2:n50000",
                           col_names = meal_count_column_unknown,
                           col_types = names(meal_count_column_unknown)
)

# Combines all the lists into a single dataframe, removes all the NAs 
# created by the large loaded range from the excels
huge_meal_count <- bind_rows(import_meal_count) %>% 
  filter(!is.na(sfa_num))

rm(import_meal_count)
# creates the sfa num format and site number needed for matching 0000
# creates the school year for the claim.  
district_meal_count <- mutate(huge_meal_count, 
                              sfa_num_char = str_pad(sfa_num, width = 4, 
                                                     side = "left", pad = "0"),
                              site_num_char = str_pad(site_num, width = 4, 
                                                      side = "left", pad = "0"),
                              claim_year = year(claim_date),
                              claim_month = month(claim_date),
                              school_year = year(claim_date) + 
                                (month(claim_date) <= 7),
                              meal_type = rename_meal_types(meal_type))


# remove and reorder a number of un-needed variables
district_meal_count <- select(district_meal_count, sfa_num_char, site_num_char, 
                              school_year, meal_type,
                              everything(), -sfa_num, -site_num, 
                              -claim_date, -earning_type,
                              -claim_year)

# Take the monthly claims into a yearly claim
by_month <- group_by(district_meal_count, school_year,
                     claim_month, sfa_num_char, site_num_char, meal_type ) %>% 
  summarize(total_meals = sum(total_meals_served),
            total_days = sum(count_days_served),
            adp_average = total_meals/total_days)

# makes a df with district/year/mealtype
by_district <- group_by(district_meal_count,
                        school_year, 
                        sfa_num_char,
                        meal_type) %>% 
  summarize(total_meals = sum(total_meals_served),
            total_days = sum(count_days_served),
            adp_average = total_meals/total_days)
by_district_17 <- filter(by_district, school_year == 2017)

#testing mapping
testing_99 <-  left_join(combined_geo_dist, by_district_17,
                         by = c("CDE_AGREEMENT" = "sfa_num_char"))

# Makes a df with sites/year/meal
by_site <- group_by(district_meal_count,
                    school_year,
                    sfa_num_char, 
                    site_num_char,
                    meal_type) %>% 
  summarize(total_meals = sum(total_meals_served),
            total_days = sum(count_days_served),
            adp_average = total_meals/total_days) 



# May want to filter specific year out
#filter(between(school_year, 2016, 2018))

#   
# testing_maple <- district_meal_count %>% 
#   filter(sfa_num_char == "0010") %>% 
#   group_by(school_year,
#            sfa_num_char, 
#            site_num_char,
#            meal_type) %>% 
#   summarize(total_meals = sum(total_meals_served),
#             total_days = sum(count_days_served),
#             adp_average = total_meals/total_days) 
# testing_maple_count <- filter(raw_student_count, 
#                               sfa_num == "0010")
# 
# testing_maple_join <- left_join(testing_maple_count, testing_maple, 
#                        by = c("school_year_num" = "school_year", 
#                               "site_num" = "site_num_char", 
#                               "meal_type", 
#                               "sfa_num" = "sfa_num_char" )) 
# 
# filter(testing_maple_join, site_num == "0503")
# filter(by_site, site_num == "0503")

# testing_5$total_meals <- sort(testing_5$total_meals, na.last = FALSE)



# Join by school year, site_number and meal type
# # testing_5 <- left_join(raw_student_count, by_site, 
#                        by = c("school_year_num" = "school_year", 
#                               "site_num" = "site_num_char", 
#                               "meal_type", 
#                               "sfa_num" = "sfa_num_char" )) 
# 
# testing_5$total_meals <- sort(testing_5$total_meals, na.last = FALSE)


# Scatter plot 
#ggplot(data = by_district) +
#  geom_point(mapping=aes(x = claim_month, y = adp_average), position = "jitter")

# na_count <-sapply(testing_5, function(y) sum(length(which(is.na(y)))))
# (na_count <- data.frame(na_count))
# range(district_meal_count$claim_month)
# range(district_meal_count$claim_year)

# i want to take each month for each district and map the adp for 15-18 
# steps to do that connect by_district with geometry

short_by_dist <- filter(by_district, 
                        school_year == 2017)

combined_test1 <- left_join(combined_geo_dist, short_by_dist,
                            by = c("CDE_AGREEMENT" = "sfa_num_char", "site_num", "meal_type")) 

january_lunch_by_dist <- filter(combined_test1,
                                claim_month == 1, 
                                meal_type == "Lunch") 

#  i want to iterate through all months, all meal types, 
# and make plots of each, then make into a gif!


# Want to make adp more relevant across the state, adp/student
january_lunch_by_dist <- mutate(january_lunch_by_dist ,
                                lunch_adp_score = adp_average / student_count) # this needs to go into the larger dataframe not monthly. 

#filter(x, month(y),meal_type(z)) %>% 
#plot(x[variable], main = paste0(y, z, variable)

# Function for doing a month and specific meal type from a dataframe      
Make_Monthly <- function(df, needed_months, meal_types){
  filter(df,
         claim_month == needed_months &
           meal_type == meal_types)
}

# function makes a plot using the specified dataframe, req variable and the title of map
districts_plot <- function(df, variable, map_title){
  plot(df[variable], main = map_title)
}

plot(january_lunch_by_dist["lunch_adp_score"], main = "ADP per student January")
plot(january_lunch_by_dist["adp_average"], main = "ADP January")

districts_plot(january_lunch_by_dist, "lunch_adp_score", "testing")

testing_1 <- Make_Monthly(combined_test1, 3, "Breakfast")


# This makes a yearly meal count by program for each district
yearly_by_district <- group_by(district_meal_count,
                               school_year, 
                               sfa_num_char,
                               meal_type) %>% 
  summarize(total_meals = sum(total_meals_served),
            total_days = sum(count_days_served),
            adp_average = total_meals/total_days,
            free_meals = sum(free_meals_served),
            reduced_meals = sum(reduced_meals_served),
            paid_meals = sum(paid_meals_served)
        
                            )


# Use the above to make meal scores for each program, I also need to have student count per year
# Where can I get the student count?

#Get student count yearly as well as F&R yearly between 16-19, start here build older later.

# directory_student_count <- "./data/student_count/"
# file_student_count <- list.files(directory_student_count)
# 
# column_names_student_count <- c("text" = "school_year",
#                                 "text" = "sfa_num",
#                                 "text" = "sfa_name", 
#                                 "text" = "site_num",
#                                 "text" ="site_name",
#                                 "text" ="meal_type",
#                                 "numeric" = "pk_12_enrollment",
#                                 "text" = "free_perc",
#                                 "text" = "redu_perc",
#                                 "text" = "free_and_redu_perc",
#                                 "text" = "four_day",
#                                 "text" = "county",
#                                 "text" = "urban_rural")
# read_excel(paste0(directory_student_count, file_student_count))

# All the above is already input via four_day comes in as import_four_day

raw_student_count <- select(imported_four_day,
                            school_year:site_name, 
                            meal_type:redu_perc) %>% 
  filter( !is.na(pk_12_enrollment)) %>% 
  mutate(school_year_num = SY_YY_XX(raw_student_count$school_year))


# need to convert SYXXYY to 20XX
# need to get each years program numbers
   
# function to change SY1011 to 2010 (as.numeric)
SY_YY_XX <- function(variable) {
  str_replace(variable, "^SY", "") %>%            
    str_remove(pattern = "\\d\\d$") %>% 
    as.numeric() + 2000
    
}
# left_join(raw_student_count,by_district, by = )

        