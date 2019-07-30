# CDE Four Day Import and Clean

directory_four_day <- list.dirs("./data/four_day")
file_four_day <- list.files("./data/four_day")
path_four_day <- paste0(directory_four_day,"/", file_four_day)

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
                                skip = 1) %>% 
  filter(!is.na(pk_12_enrollment))

# creates a working file to prevent need to reload. 
# Selects only the variables needed for this analysis
working_four_day <- imported_four_day %>% 
  select(school_year, 
         sfa_num, 
         site_num, 
         four_day, 
         urban_rural,
         pk_12_enrollment,
         meal_type)

mutate_four_day <- mutate(working_four_day, week_length = case_when(
  four_day == "T-F" ~4, 
  four_day == "M-Th" ~4, 
  four_day == "N" ~5),
  school_year = SY_YY_XX(school_year))


# Summarize by week length
sum_mutate_four_day <- group_by(mutate_four_day, school_year, sfa_num, meal_type) %>% 
  summarize(week_length = mean(week_length),
            program_student_population = sum(pk_12_enrollment))

# create the score 4-day score and removes extra variables
# This is actually a number of different important variables
# going to seperate out each
Scored_four_day <- ungroup(sum_mutate_four_day)%>% 
  mutate(score_4day = case_when(
  week_length == 5 ~1,
  week_length == 4 ~0)) %>% 
  select(sfa_num, score_4day) %>% 
  unique.data.frame()

program_population <- ungroup(sum_mutate_four_day) %>% 
  select(school_year, sfa_num, meal_type, program_student_population)
  
urban_rural_district <- ungroup(working_four_day) %>% 
  select(sfa_num, urban_rural) %>% 
  unique.data.frame()


# Extra elements to remove
# 
rm("mutate_four_day",
   "sum_mutate_four_day",
   "working_four_day",
   "directory_four_day",
   "file_four_day",
   "path_four_day",
   "imported_four_day")

# Elements to Keep
# Scored_four_day
# program_population



