# CDE Mapping Full

# Call all needed librarys and created functions
source("./cde_library_functions.r")


# Loads the shape files for all the districts
# Also connects GEOID to SFA Number and provides the number of schools in the district
# Creates GEOID dataframe with district name, number, number of schools
# and simplified (smaller) location geometry
source("./cde_shape_file_load.r")
# Output named: co_school_joined
# Output level: By district


# Loads all meal counts for NSLP, SMP, ASP, SBP, SFSP, combines into one dataframe
# No Analysis is conducted yet
source("./cde_mealcounts.r")
# Output named: site_meal_count
# Output level: by school year, site_num, meal_type, 
# Score components available, NSLP ADP, SMP ADP, SBP ADP, SFSP ADP, SFSP Sites


# Loads four day school week data, extracts out with SFA and score
# Also loads total number of students participating in each program for each district
source("./cde_fourday.r")
# Output named: program_population
# Output level: by school year, sfa_num, meal_type

# Output named: Scored_four_day 
# Output level: by district

# Output named: urban_rural_district
# Output level: by district
# Score Components Available: four_day score, urban_rural


# Loads and determines school distric total student population for year 18-19
# Also extracts combined free and reduced percentage
source("./cde_studentcount.r")
# Output named: student_count_total
# Output level: by district
# Score components available: Free and Reduced Score


# Loads 2017 CEP certifications and calculated CEP Score based on partial or complete participation in CEP programs
source("./cde_cep.r")
# Output named: Scored_cep
# Output level: by district

# Output named: full_cep
# Output level: by district
# Score Components available: CEP Score

# combine all district level indicators first
Combined_district_level <- Scored_cep %>% 
  left_join(student_count_total, by = "sfa_num") %>% 
  left_join(urban_rural_district, by = "sfa_num") %>% 
  left_join(Scored_four_day, by = "sfa_num") %>% 
  filter(!is.na(sfa_name))

combined_geo_district <- left_join(
  co_school_joined, Combined_district_level, by = "sfa_num") %>% 
  select(GEOID,NAME, sfa_num, cep_score, Scored_eligibility, score_4day, everything())


# This one working

combined_sfa_program_level <- site_meal_count %>%
  filter(sfa_num <7999,
         count_days_served >0) %>% # filter our non-public sfas and non-active 
  group_by(school_year, sfa_num, meal_type, site_num) %>%
  summarise_at(vars(total_meals_served, count_days_served), list(sum)) %>% 
  #sum total meals served and number of days served for each site for each program
  mutate(site_count = 1) %>%
  # create variable that will determine the number of unique sites serving each program
  summarise_if(is.numeric, sum) %>%
  # sum the rest of the variables if numeric
  mutate(adp_calculated = total_meals_served / count_days_served) 
  # Calculate the adp for each program total meals served/ number of days served

# takes above 
scored_adp_programs <- combined_sfa_program_level %>% 
  left_join(student_count_total, by = c("sfa_num", "school_year")) %>% 
  mutate(adp_per_student = adp_calculated / district_population,
         average_service_site = count_days_served / site_count) %>% 
  ungroup()


district_adp <-  scored_adp_programs %>% 
  group_by(school_year, sfa_num) %>% 
  summarise(Scored_adp = sum(adp_per_student, na.rm = TRUE),
            district_population = mean(district_population),
            Scored_eligibility = mean(Scored_eligibility))





testing_complete_join <- left_join(combined_geo_district,
          district_adp,
          by = c("sfa_num", "school_year")) %>%
  mutate(Scored_district_size = Score_District_Size(District_population),
         Scored_rural_urban = Score_ruralurban(urban_rural)) %>% 
  filter(school_year < 2018)


testing_99 <- testing_complete_join %>% 
  group_by(school_year, sfa_num) %>% 
  mutate(total_score = sum(c(cep_score,
                             score_4day,
                             Scored_adp,
                             Scored_rural_urban,
                            Scored_eligibility.y, 
                            Scored_district_size), na.rm = TRUE))


# Testing to get Program info
profiles <- read_excel("./data/blueprint_designed/SFA_profile_Feb_2019.xls", 
                       col_types = c("text", "text", "text","logical", 
                                     "numeric","numeric", 
                                     "numeric", "logical",
                                     "logical", "logical", 
                                     "logical","logical"))

profiles <- profiles %>% 
  dplyr::rename(sfa_num = "Sponsor #", 
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
mapping_profiles <- select(profiles,1, 
       8:12) %>% 
left_join(x = testing_99, y = ., by =)
end_time <- Sys.time()
