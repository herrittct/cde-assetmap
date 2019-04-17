# SFSP

sfsp_path <- "./data/sfsp/" # place the file storage here
(sfsp_file <- list.files(sfsp_path))
sfsp_file_path <- paste0(sfsp_path,sfsp_file)

# Names and types for each column in the excel file
column_names_sfsp <- c("text" = "sfa_num",
                                "text" = "sfa_name",
                                "text" = "site_num",
                                "text" ="site_name",
                                "numeric" = "meal_2017_sfsp",
                                "numeric" = "meal_2018_sfsp",
                                "numeric" = "change_meal_count_sfsp",
                                "numeric" = "change_meal_percent_sfsp",
                                "numeric" = "adp_2017_sfsp",
                                "numeric" = "adp_2018_sfsp",
                                "numeric" = "change_adp_count_sfsp",
                                "numeric" = "change_adp_percent_sfsp")

# Imports raw excel using above column names and types skipping the header line
raw_sfsp <- read_excel(sfsp_file_path, 
           col_names = column_names_sfsp,
           col_types = names(column_names_sfsp),
           skip = 1)  

# Creates a function to pad character inputs to 4 from the left with 0's
Pad_to_character <-  function(input_variable){
  str_pad(input_variable, width = 4, 
          side = "left", pad = "0")}

# Creates padded sfa_num & site_num to compare with other data sources
imported_sfsp <- mutate(raw_sfsp,
                        sfa_num_char = Pad_to_character(sfa_num),
                        site_num_char = Pad_to_character(site_num)) %>% 
  select(sfa_num_char, site_num_char, everything())

# Removes all the temporary variables that we wont be using
rm(raw_sfsp, column_names_sfsp, sfsp_file, sfsp_file_path, sfsp_path)

sfsp_1718 <- group_by(imported_sfsp, 
                      sfa_num_char) %>% 
  summarize(site_count_sfsp = n(),
            meal_count_sfsp_2017 = sum(meal_2017_sfsp),
            meal_count_sfsp_2018 = sum(meal_2018_sfsp),
            adp_sfsp_2017 = sum(adp_2017_sfsp),
            adp_sfsp_2018 = sum(adp_2018_sfsp))
combined_sfsp_geodist <- left_join(combined_geo_dist,sfsp_1718, 
                                   by = c("CDE_AGREEMENT" = "sfa_num_char"))            
