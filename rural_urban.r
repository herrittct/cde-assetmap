# Rural/Urban 
# Can Also do 4 day at the same time

# There are a couple ways to get rural/urban
# Use what was provided by the data guy, fastest, easiest, should make that happen first
file_cde_rural_urban <- paste0("C:/Users/CHerr/OneDrive/Documents/",
                               "CDEMapping/cde-assetmap/data/four_day/",
                               "Sponsor-Site_Data_SY1819_To-SY1617.xlsx")
file_cde_rural_urban
raw_cde_rural_urban <- read_excel(file_cde_rural_urban) %>% 
  rename(urban_rural = "Urban/Rural",
         four_day = "4-Days_School_Week",
         site_num = "Site_No",
         free_percent = "Free%",
         redu_percent = "Reduced%",
         free_redu_percent = "Free_and_Reduced%") %>% 
  mutate(free_percent = as.numeric(sub("%", "", free_percent))/100,
         redu_percent = as.numeric(sub("%", "", redu_percent))/100,
         free_redu_percent = as.numeric(sub("%", "", free_redu_percent))/100
         )

cde_rural_urban <-
  select(raw_cde_rural_urban, 
         "SchoolYear":"site_num",
         "four_day",
         -"CustomerName", 
         "urban_rural") 
  

cde_rural_urban$four_day <- as_factor(cde_rural_urban$four_day)
cde_rural_urban$urban_rural <- as_factor(cde_rural_urban$urban_rural)
cde_rural_urban$SchoolYear <- as_factor(cde_rural_urban$SchoolYear)

# Filter out only the poudre schools
testing_poudre_site <- filter(raw_cde_rural_urban,
                         Agreement == 1550)

testing_poudre_dist <- group_by(testing_poudre_site, site_num, SchoolYear) %>% 
  summarise(program_count = n(),
            free_perc = mean(free_percent))
# Make some "Interesting" Plots... not really but they plot
ggplot(testing_poudre_dist) + 
  geom_point(aes(x= free_perc, y = SchoolYear))

ggplot(testing_poudre_dist) +
  geom_point(aes(x = SchoolYear , y = program_count), position = position_jitter(width = 0.1, height = 0.1))

# Filter out all the poudre meal counts from monthly meal count
testing_poudre_mealcount <- filter(monthly_meal_count,
                                   sfa_num == 1550) %>% 
  arrange(site_num) %>% 
  group_by(site_num, year, meal_type) %>% 
  summarise(days_served = sum(days_served_count),
            meals_served = sum(Total_Meals_Served),
            adp = meals_served / days_served)
                                 