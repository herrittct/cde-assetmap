# Meal counting importing

# Import all the meal counts AHH so many observations!
library(gsubfn)
library(readxl)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)


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
                                (month(claim_date) <= 7)
                              )


by_month <- group_by(district_meal_count, school_year,
                     claim_month, sfa_num_char, site_num_char, meal_type ) %>% 
  summarize(total_meals = sum(total_meals_served),
            total_days = sum(count_days_served),
            adp_average = total_meals/total_days)

by_district <- group_by(district_meal_count,
                        school_year, 
                        claim_month,
                        sfa_num_char,
                        meal_type) %>% 
  summarize(total_meals = sum(total_meals_served),
            total_days = sum(count_days_served),
            adp_average = total_meals/total_days)

ggplot(data = by_district) +
  geom_point(mapping=aes(x = claim_month, y = adp_average), position = "jitter")

na_count <-sapply(by_month, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
range(district_meal_count$claim_month)
range(district_meal_count$claim_year)

# i want to take each month for each district and map the adp for 15-18 
# steps to do that connect by_district with geometry

short_by_dist <- filter(by_district, 
                        school_year == 2017)

combined_test1 <- left_join(combined_geo_dist, short_by_dist,
          by = c("CDE_AGREEMENT" = "sfa_num_char")) 

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

districts_plot(january_lunch_by_dist, "adp_per_stud", "testing")

testing_1 <- Make_Monthly(combined_test1, 3, "Breakfast")


# Need to make yearly meal count by districts

# Steps take each school combine all meal counts for all month for each program
# Combine all the schools into meal counts for each program for the district
# Do this by school district
