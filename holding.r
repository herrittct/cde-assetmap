# Nice functions I have learned along the way

# Modified to work the way I really want it to
my_custom_name_repair2 <- function(nms) str_to_lower(
  gsub("\\s", "_",gsub("[.]", "_", nms))) 

# great little function for cleaning up names
my_custom_name_repair <- function(nms) tolower(gsub(space(),
                                                    "", gsub("[.]", "_", nms)))



testing <-lapply("./data/meal_count/SY10-11.xls", 
                 read_excel, n_max = 5, 
                 .name_repair = my_custom_name_repair
)



testing2 <-lapply("./data/meal_count/SY10-11.xls", 
                  read_excel, n_max = 5, 
                  .name_repair = my_custom_name_repair2
)
# This is literally everything I need to import all the excels 
my_data <- (lapply(count_files,read_excel))
big_meal_count <- bind_rows(my_data)

mutate(my_data, provision_type = as.character(my_data$ProvisionType))
class(my_data[[4]]$ProvisionType)       

