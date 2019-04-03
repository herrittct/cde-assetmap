library(lubridate)
library(forcats)
library(tidyverse)

# Import the monthly meal count data to see what is looks like
meal_count_excel <- paste0("C:/Users/CHerr/OneDrive/Documents/CDEMapping",
                           "/cde-assetmap/data/school_year_programs/",
                           "Monthlymealcount_1718.xls")

raw_monthly_meal_count <- read_excel(meal_count_excel)

# This all works to change the SFA number to a 4 digit character
monthly_meal_count <- rename(raw_monthly_meal_count,
                             sfa_num = "SFA #",
                             site_num = "Site #",
                             sfa_name = "School Food Authority Name",
                             site_name = "Site Name",
                             claim_date = "Claim Date",
                             days_served_count = "DaysServedQty",
                             rate_level = "Rate Level",
                             meal_type = "Meal Type") %>% 
  select(everything(), -"Earning Type") %>% 
  mutate(claim_date = ymd(claim_date),
         year = year(claim_date),
         month = month(claim_date))

# Creates characters that can match with with the other character plots 
monthly_meal_count$sfa_num <- str_pad(monthly_meal_count$sfa_num,
                                  width = 4, 
                                  side = "left",
                                  pad = "0")
monthly_meal_count$site_num <- str_pad(monthly_meal_count$site_num,
                                       width = 4, 
                                       side = "left",
                                       pad = "0")

# this splits it but makes it complicated becuase it is the same school year
meal_count2017 <- filter(monthly_meal_count, year == 2017)
meal_count2018 <- filter(monthly_meal_count, year == 2018)


# Need to mutate the meal type into different vectors
monthly_meal_count <- 

# Maps the total meals served over the course of the year by each sfa
# This kinda stuff is really cool
monthly_meal_count %>% 
  group_by(sfa_num) %>% 
  ggplot() +
  geom_point(aes(y = Total_Meals_Served, x = claim_date), position = "jitter") +
  facet_wrap(~rate_level, nrow = 2) +
  geom_smooth(aes(y = Total_Meals_Served, x = claim_date))

meal_count2017 %>% 
  ggplot() +
  geom_point(aes(y = Total_Meals_Served, x = claim_date), position = "jitter")

monthly_meal_count %>% 
  ggplot() +
  geom_point(aes(y = Total_Meals_Served, x = claim_date), position = "jitter")


