# four_day
library(tidyverse)
library(readxl)

directory_four_day <- list.dirs("./data/four_day")
list.dirs("./data/")
list.dirs("./data/four_day")
list.files("./data/four_day")
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


