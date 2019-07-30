# CDE Meal Counts between 2010-2018 school years

list.files("./data/meal_count", pattern = "^SY")
meal_count_directory <-  "./data/meal_count/"
count_files <- list.files("./data/meal_count", pattern = "^SY")

sfsp_path <- "./data/sfsp/SFSP_Meal_Counts_2013-2018.xls"


# creates a list of the matching files path
count_files <- paste0(meal_count_directory,count_files)


# Creates list of with col_type = col_name, use names() to use the col_type
# First is for the larger meal counts without SFSP
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

# Second is for SFSP specifically
# Create variable names and types, skips unnessesary columns
sfsp_var_names <- c("date" = "claim_date",
                    "skip" = "revision_nbr",
                    "text" = "sfa_num",
                    "text" = "sfa_name",
                    "text" = "SponsorTypeDesc",
                    "numeric" = "sfsp_site_count",
                    "numeric" = "count_days_served",
                    "skip" = "x", "skip" = "x", "skip" = "x", "skip" = "x",
                    "numeric" = "free_meals_served",
                    "numeric" = "total_meals_served",
                    "skip" = "x", "skip" = "x", "skip" = "x", "skip" = "x",
                    "skip" = "x", "skip" = "x", "skip" = "x", "skip" = "x",
                    "skip" = "x", "skip" = "x", "skip" = "x", "skip" = "x",
                    "text" = "meal_type",
                    "text" = "site_num",
                    "text" = "site_name")



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


# Selects only SFA sites
sfsp_sheets <- sfsp_path %>%
  excel_sheets() %>% 
  set_names() %>% 
  map_df(~ read_excel(sfsp_path, sheet = .x, skip = 7,
                      col_names = sfsp_var_names,
                      col_types = names(sfsp_var_names),
                      na = "0"),
         .id = "sheet") %>% 
  filter(SponsorTypeDesc == "School Food Authority",
         !is.na(sfsp_site_count)) %>% 
  select(-sheet, -SponsorTypeDesc)


site_meal_count <- bind_rows(sfsp_sheets, huge_meal_count) %>% 
  mutate(sfa_num = Pad_to_character(sfa_num), #for matching sfas 
         site_num = Pad_to_character(site_num), # for matching sites
         meal_type = rename_meal_types(meal_type), # consistent meal typing
         claim_month = month(claim_date),         # determine claim month
         school_year = year(claim_date) - # determine school year claim belongs to
           (month(claim_date) <= 7)) %>% 
  select(school_year, sfa_num, site_num, meal_type,
         sfsp_site_count:claim_month, claim_date)
 

# reduces working memory load by removing the imported dataframe
rm("import_meal_count", "meal_count_column_unknown",
   "meal_count_directory", "count_files", "huge_meal_count", "sfsp_sheets")

