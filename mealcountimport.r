# Meal counting importing

# Import all the meal counts AHH so many observations!
library(gsubfn)
library(readxl)
library(tidyverse)
library(dplyr)
library(stringr)



ls()
ls("~/data/meal_count")
list.dirs()
list.files("./data/meal_count", pattern = "^SY")
meal_count_directory <-  "./data/meal_count/"
count_files <- list.files("./data/meal_count", pattern = "^SY")

# creates a list of the matching files path
count_files <- paste0(meal_count_directory,count_files)

# This is literally everything I need to import all the excels 
my_data <- (lapply(count_files,read_excel))
big_meal_count <- bind_rows(my_data)

mutate(my_data, provision_type = as.character(my_data$ProvisionType))
class(my_data[[4]]$ProvisionType)       

# great little function for cleaning up names
my_custom_name_repair <- function(nms) tolower(gsub(space(),
                                                    "", gsub("[.]", "_", nms)))

testing <-lapply("./data/meal_count/SY10-11.xls", 
                 read_excel, n_max = 5, 
                 .name_repair = my_custom_name_repair
)

# Modified to work the way I really want it to
my_custom_name_repair2 <- function(nms) str_to_lower(
  gsub("\\s", "_",gsub("[.]", "_", nms))) 

testing2 <-lapply("./data/meal_count/SY10-11.xls", 
                 read_excel, n_max = 5, 
                 .name_repair = my_custom_name_repair2
)


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


import_meal_count <-lapply(count_files, 
                  read_excel,
                  range = "a2:n50000",
                  col_names = meal_count_column_unknown,
                  col_types = names(meal_count_column_unknown)
                  )

huge_meal_count <- bind_rows(import_meal_count) %>% 
  filter(!is.na(sfa_num))

district_meal_count <- mutate(huge_meal_count, 
                              sfa_num_char = str_pad(sfa_num, width = 4, 
                                                     side = "left", pad = "0"),
                              site_num_char = str_pad(site_num, width = 4, 
                                                      side = "left", pad = "0")
                              )


