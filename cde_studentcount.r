# Student Count for the whole district not just with programs


(student_count_files_names <- list.files("./data/student_count/all_years"))
student_count_directory <-  "./data/student_count/all_years/"

# creates a list of the matching files path
student_count_files <- paste0(student_count_directory,student_count_files_names)

student_count_column_unknown <- c("skip" = "x",
                                  "skip" = "x", 
                               "text" = "sfa_num",
                               "text" ="sfa_name", 
                               "numeric"= "district_population", 
                               "skip" = "x",
                               "skip" = "x",
                               "skip" = "x",
                               "skip" = "x",
                               "numeric" = "free_percent", 
                               "numeric" = "reduced_percent", 
                               "numeric" = "Scored_eligibility")
student_count_na <- c("*", "N/A" , "")

file_to_school_year <- stringr::str_extract(student_count_files_names, pattern = "^20\\d\\d")

# Imports all the files into a large list of lists
# student_count_total <-lapply(student_count_files, 
#                              read_excel,
#                              range = "a4:l300",
#                              col_names = student_count_column_unknown,
#                              col_types = names(student_count_column_unknown),
#                              na = student_count_na) %>% 
#   bind_rows(.id = "school_year") %>% 
#   filter(!is.na(sfa_num), sfa_num < 7999,
#          !is.na(sfa_name)) %>% 
#   mutate(school_year = as.numeric(school_year),
#          school_year = id_to_sy(school_year))

# Testing Stage below
# Imports all the files into a large list of lists
student_count_total <-lapply(student_count_files, 
                           read_excel,
                           range = "a4:l300",
                           col_names = student_count_column_unknown,
                           col_types = names(student_count_column_unknown),
                           na = student_count_na) %>% 
  bind_rows(.id = "id") %>% 
  filter(!is.na(sfa_num), sfa_num < 7999,
         !is.na(sfa_name)) %>% 
  mutate(school_year = as.numeric(file_to_school_year[as.numeric(id)])) 

rm("student_count_na", "student_count_column_unknown",
   "student_count_files", "student_count_directory")


