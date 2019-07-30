### Draft 4/17/2019


library(tidyverse)
library(proto)
library(gsubfn)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(tigris)
library(reticulate)
library(RQGIS)
library(tidycensus)
library(sf)
library(forcats)
library(leaflet)
library(ggmap)
library(curl)



options(tigris_class = "sf")
tigris_use_cache = TRUE
apikey <- "69d8405e34b271517d234dcd4689e8df75836eff"

# Functions

# need to change meal type from breakfast/lunch/snack/milk to abbrev

rename_meal_types <-  function(variable){case_when(
  variable == "Breakfast" ~"SBP",
  variable == "Lunch" ~"NSLP", 
  variable ==  "Snack" ~"ASP",
  variable == "Milk" ~"SMP")
}


# Function for doing a month and specific meal type from a dataframe      
Make_Monthly <- function(df, needed_months, meal_types){
  filter(df,
         claim_month == needed_months &
           meal_type == meal_types)
}
# Make a plot by variable and map title
districts_plot <- function(df, variable, map_title){
  plot(df[variable], main = map_title)
}



# function to change SY1011 to 2010 (as.numeric)
SY_YY_XX <- function(variable) {
  str_replace(variable, "^SY", "") %>%            
    str_remove(pattern = "\\d\\d$") %>% 
    as.numeric() + 2000
}

Score_District_Size <- function(size_variable){
  case_when(
    size_variable < 50 ~0.33,
    between(size_variable, 51, 500) ~0.67,
    between(size_variable, 501, 1000) ~1,
    between(size_variable, 1001, 5000) ~1.34,
    between(size_variable, 5001, 10000) ~1.67,
    size_variable > 10000 ~2)
  
}

Pad_to_character <-  function(input_variable){
  str_pad(input_variable, width = 4, 
          side = "left", pad = "0")}

# na_count <-sapply(final_district_dataframe, function(y) sum(length(which(is.na(y)))))
# (na_count <- data.frame(na_count))
# range(district_meal_count$claim_month)
# range(district_meal_count$claim_year)
 

# # CDE Community Asset Mapping
# 
# This is a document created for the Colorado Department of Education School Nutrition unit in association with the Colorado Health Foundation "Blueprint to End Hunger"
# 
# The purpose of this is to quickly and easily combine and utilize existing public data regarding child nutrition programs in Colorado and present that data in a form that can help to inform the decisions at the state level to provide assistance, resources, and technical support to districts identified as priority areas.  
# 
# This project is currently in progress and therefore errors may be present.  
# 
# 
# ### Gathering the shape geometry
# 
# The a general variable is first gathered from the American Community Survey, this is matched with similar information from the US Census and Tigris package to determine the shape and location of each school district. 


colorado_income <- get_acs(geography = "school district (unified)",
                           variables = "B19013_001",
                           state = "CO", key = apikey)

colorado_school <- school_districts(state = "CO",
                                    type = "unified",
                                    class = "sf")

co_school_joined <- left_join(colorado_school,
                              colorado_income,
                              by = "GEOID")

# Remove 
rm("colorado_income", 'colorado_school')
 

# ### School District Profiles
# 
# Next school district profiles, a file provided by the CDE Nutrition Unit is imported to be used to support information regarding 4 day school weeks, program participation and Sponsor or agreement number for each district, this number is standardized accross datasources and will be the matching element for each subsequent data injection. 


profiles <- read_excel("./data/blueprint_designed/SFA_profile_Feb_2019.xls", 
                       col_types = c("text", "text", "text","logical", 
                                     "numeric","numeric", 
                                     "numeric", "logical",
                                     "logical", "logical", 
                                     "logical","logical"))

profiles <- profiles %>% 
  dplyr::rename(CDE_AGREEMENT = "Sponsor #", 
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


# The above actually takes the raw excel into a semi-usable dataframe
profiles <- filter(profiles, Profile == "Public")
 
# ### Match SFA Agreement number to GEOID
# 
# The census and CDE have different numbering protocols therefore a library was build to match each SFA GEOID to the cooresponding CDE Agreement/Sponsor number.  This next step brings that data into the environment and combines it with the previous profiles.  Finally it connects both the profiles and GEOID data and shape files into a shape frame data frame called combined_geo_dist


# Import all the schools with GEOID!
# Must have current directory above data

site_all <- read_excel("./data/CDE_to_GEOID.xlsx", col_types = "text") %>% 
  mutate(GEOID = ACTUAL_GEOID) %>% 
  select(GEOID, CDE_AGREEMENT, everything(), -ACTUAL_GEOID)

site_all <- group_by(site_all,
                     CDE_AGREEMENT)

# Gets all the districts with their site count and GEOID
(districts <- summarise(site_all, 
                        count_schools = n(),
                        GEOID = first(GEOID)))

# Combines districts with GEOID number
geoid_districts <- left_join(districts, profiles, by = "CDE_AGREEMENT")

### This WORKS!!
combined_geo_dist <- left_join(co_school_joined, geoid_districts, 
                               by = "GEOID")

#remove
rm("site_all", "districts", "geoid_districts")
#  
# ## Using monthly meal counts to identify yearly trends 
# 
# 
# This is using only a single year of data, the next provides the same information from 2010-2018 school years.  

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

# remove
rm("meal_count_excel", "raw_monthly_meal_count")
 


list.files("./data/meal_count", pattern = "^SY")
meal_count_directory <-  "./data/meal_count/"
count_files <- list.files("./data/meal_count", pattern = "^SY")

# creates a list of the matching files path
count_files <- paste0(meal_count_directory,count_files)

# Creates list of with col_type = col_name, use names() to use the col_type
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

# reduces working memory load by removing the imported dataframe
rm("import_meal_count", "meal_count_column_unknown",
   "meal_count_directory", "count_files")

# creates the sfa num format and site number needed for matching 0000
# creates the school year for the claim.  
district_meal_count <- mutate(huge_meal_count, 
                              sfa_num_char = str_pad(sfa_num, width = 4, 
                                                     side = "left", pad = "0"),
                              site_num_char = str_pad(site_num, width = 4, 
                                                      side = "left", pad = "0"),
                              claim_year = year(claim_date),
                              claim_month = month(claim_date),
                              school_year = year(claim_date) + 
                                (month(claim_date) <= 7),
                              meal_type = rename_meal_types(meal_type))


# remove and reorder a number of un-needed variables
district_meal_count <- select(district_meal_count, sfa_num_char, site_num_char, 
                              school_year, meal_type,
                              everything(), -sfa_num, -site_num, 
                              -earning_type, -claim_year)

 
# 
# ## Meal Count over time
# 
# Three different representations of the way meal counts have changed over time since 2010-2018
# 

district_meal_count %>% 
  group_by(sfa_num_char) %>% 
  ggplot() +
  geom_point(aes(y = total_meals_served, x = claim_date), position = "jitter") +
  facet_wrap(~rate_level, nrow = 2) +
  geom_smooth(aes(y = total_meals_served, x = claim_date))

district_meal_count %>% 
  ggplot() +
  geom_point(aes(y = total_meals_served, x = claim_date), position = "jitter")

district_meal_count %>% 
  group_by(sfa_num_char) %>% 
  ggplot() +
  geom_point(aes(y = total_meals_served, x = claim_date), position = "jitter") +
  facet_wrap(~provision) +
  geom_smooth(aes(y = total_meals_served, x = claim_date))
 

# ## Four Day School Weeks
# 
# The proliferation of 4 day schools weeks is seen as a threat to food security for students who depend on child nutrition programs for daily meals.  The proportion of school districts utilizing 4 days school weeks is significant.  




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
                                skip = 1)

# creates a working file to prevent need to reload. 
# Selects only the variables needed for this analysis
working_four_day <- imported_four_day %>% 
  select(school_year, 
         sfa_num, 
         site_num, 
         four_day, 
         urban_rural)

mutate_four_day <- mutate(working_four_day, week_length = case_when(
  four_day == "T-F" ~4, 
  four_day == "M-Th" ~4, 
  four_day == "N" ~5),
  school_year = SY_YY_XX(school_year))


# Summarize by week length
sum_mutate_four_day <- group_by(mutate_four_day, school_year, sfa_num, urban_rural) %>% 
  summarize(week_length = mean(week_length))

# create the score 4-day score and removes extra variables
score_four_day <- mutate(sum_mutate_four_day, score_4day = case_when(
  week_length == 5 ~1,
  week_length == 4 ~0)) %>% 
  select(everything(),-week_length)
# need to convert following uses of score_four_day to final_four_day
final_four_day <- score_four_day 


# Extra elements to remove
# mutate_four_day
# score_four_day
# sum_mutate_four_day
# working_four_day
# directory_four_day
# file_four_day
# path_four_day

# Elements to Keep
# imported_four_day
# final_four_day
 

# ### Refining Meal Counts
# 
# Condensing monthly meal counts which can be instructive on the large scale to condensed yearly counts by SFA, Year, and Meal program
# 
# Also begins work determining ADP of programs. 


# Take the monthly claims into a yearly claim
by_month <- group_by(district_meal_count, school_year,
                     claim_month, sfa_num_char, site_num_char, meal_type ) %>%
  summarize(total_meals = sum(total_meals_served),
            total_days = sum(count_days_served),
            adp_average = total_meals/total_days)

# makes a df with district/year/mealtype
by_district <- group_by(district_meal_count,
                        school_year, 
                        sfa_num_char,
                        meal_type,
                        provision) %>% 
  summarize(total_meals = sum(total_meals_served),
            total_days = sum(count_days_served),
            adp_average = total_meals/total_days,
            free_meals = sum(free_meals_served),
            reduced_meals = sum(reduced_meals_served),
            paid_meals = sum(paid_meals_served))

by_district_17 <- filter(by_district, school_year == 2017)

# Makes a df with sites/year/meal
by_site <- group_by(district_meal_count,
                    school_year,
                    sfa_num_char,
                    site_num_char,
                    meal_type) %>%
  summarize(total_meals = sum(total_meals_served),
            total_days = sum(count_days_served),
            adp_average = total_meals/total_days)

 
# # Monthly ADP over time
# 
# This Graph shows the ADP of all districts by month between 2010-2018

ggplot(data = by_month) +
  geom_point(mapping=aes(x = claim_month,
                         y = adp_average), position = "jitter") +
  labs(
    title = "District ADP average by month 2010-2019",
    x = "Claim Month",
    y = "Average ADP"
  )

rm("by_month")
 

raw_student_count <- select(imported_four_day,
                            school_year:site_name, 
                            pk_12_enrollment:redu_perc) %>% 
  filter( !is.na(pk_12_enrollment)) %>% 
  mutate(school_year_num = SY_YY_XX(.$school_year),
         free_perc = parse_number(free_perc)/100,
         redu_perc = parse_number(redu_perc)/100)

# I need to take the different district sizes, compare them to the list above




working_student_count <- group_by(raw_student_count, 
                                  sfa_num, school_year_num) %>% 
  summarize(pk_12_enrollment_total = sum(pk_12_enrollment),
            free_perc = mean(free_perc),
            redu_perc = mean(redu_perc),
            district_size_score = Score_District_Size(pk_12_enrollment_total))


ggplot(data = working_student_count) +
  geom_histogram(mapping=aes(x = district_size_score)) 

final_student_count <- working_student_count
#Can now remove the imported_four_day
rm("imported_four_day", "raw_student_count")
 

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

# Creates padded sfa_num & site_num to compare with other data sources
imported_sfsp <- mutate(raw_sfsp,
                        sfa_num_char = Pad_to_character(sfa_num),
                        site_num_char = Pad_to_character(site_num)) %>% 
  select(sfa_num_char, site_num_char, everything())



sfsp_1718 <- group_by(imported_sfsp, 
                      sfa_num_char) %>% 
  summarize(site_count_sfsp = n(),
            meal_count_sfsp_2017 = sum(meal_2017_sfsp),
            meal_count_sfsp_2018 = sum(meal_2018_sfsp),
            adp_sfsp_2017 = sum(adp_2017_sfsp),
            adp_sfsp_2018 = sum(adp_2018_sfsp))


combined_sfsp_geodist <- left_join(combined_geo_dist,sfsp_1718, 
                                   by = c("CDE_AGREEMENT" = "sfa_num_char"))            


# Removes all the temporary variables that we wont be using
rm("raw_sfsp", "column_names_sfsp", "sfsp_file", "sfsp_file_path", "sfsp_path", "sfsp_1718")
 
### Create the final dataframe with all the required components



combined_geodist1 <- left_join(combined_sfsp_geodist, final_four_day, 
                               by = c("CDE_AGREEMENT" = "sfa_num")) 
combined_geodist2 <- left_join(combined_geodist1,final_student_count, 
                               by = c("CDE_AGREEMENT" = "sfa_num",
                                      "school_year" = "school_year_num"))

final_district_dataframe <- left_join(combined_geodist2, by_district, 
                                      by = c("CDE_AGREEMENT" = "sfa_num_char",
                                             "school_year" = "school_year"))


rm("combined_geodist2",
   "combined_geodist1",
   "combined_sfsp_geodist",
   "final_student_count",
   "final_four_day")


trim_variables_final <- c("STATEFP", "UNSDLEA", "LSAD", "HIGRADE","LOGRADE", "MTFCC", "SDTYP", "FUNCSTAT", "ALAND", "AWATER", "NAME.y", "NAME.x")
trim_variables_tentative <- c("INTPTLAT", "INTPTLON", "variable", "estimate", "moe")
# need to trim variables from final district dataframe
final_district_dataframe <- select(final_district_dataframe,
                                   GEOID, school_year, meal_type, score_4day, district_size_score, everything(), -trim_variables_final, -trim_variables_tentative)


 
## Calculate the risk scores for all districts overtime

# program_adp <- function(dataframe){
#   temp <- paste0(as.character(dataframe$meal_type), "adp")
#   mutate(dataframe, 
#    = adp_average) %>% 
#     rename(temp = )
# }

# breakdown master into program specific info?
# can easily make a map for each program with program info
# Can calculate SFSP score
# Can calculate Program scores

final_district_dataframe <- mutate(final_district_dataframe, score_urban_rural = case_when(
  urban_rural == "Rural" ~1,
  urban_rural == "Urban" ~2
))

final_district_dataframe <- mutate(final_district_dataframe, 
                                   score_cep = case_when(
                                     provision == "2" ~.5,
                                     provision == "CEO" ~1,
                                     is.na(provision) ~0
                                   ))

final_district_dataframe <- mutate(final_district_dataframe, 
                                   score_sfsp_sites = case_when(
                                     !is.na(site_count_sfsp) ~c(site_count_sfsp / count_schools),
                                     is.na(site_count_sfsp) ~0), 
                                   score = score_4day + district_size_score + score_urban_rural+ score_cep + score_sfsp_sites + free_perc.y + redu_perc.y) 




scores_final <- group_by(final_district_dataframe, 
                         school_year,
                         CDE_AGREEMENT) 


# temp <- paste0(as.character(final_district_dataframe$meal_type), "_adp")
# mutate(final_district_dataframe, names(temp) = adp_average)

#urban/rural calculation
# mutate(final_district_dataframe, score_urban_rural = case_when(
#   urban_rural == "Rural" ~1,
#   urban_rural == "Urban" ~2
# ))
# 
# filter(final_district_dataframe,
#        meal_type == "NSLP",
#        school_year == 2018)
# 
# #CEP calculation
# mutate(final_district_dataframe, 
#       score_cep = case_when(
#         provision == "2" ~.5,
#         provision == "CEO" ~1,
#         is.na(provision) ~0
#       ))
# 
# # SFSP score
# adp summer/adp_nslp
# summer_sites/normal sites
# 
# 
# gather(final_district_dataframe, key = )
# mutate(final_district_dataframe,
#        nslp_adp = 
#          if (final_district_dataframe$meal_type == "NSLP"){
#            
#          }
#        score_sfsp_adp = adp_sfsp_2018/)
# 
# mutate(final_district_dataframe, score_sfsp_sites = case_when(
#   !is.na(site_count_sfsp) ~c(site_count_sfsp / count_schools),
#   is.na(site_count_sfsp) ~0))
# 
# 
# #total score
# 
# score_cep + score_urban_rural + score_4day + district_size_score + free_perc.y + redu_perc.y +sfs
# 
# 
# program_adp(final_district_dataframe)
# 
# 
# score <- function(){
#   program_adp()
#   sfsp
#   four_day
#   
# }

final_district_dataframe_popups <- paste0("District Name: ", final_district_dataframe$sfa_name, "<br>",
                                          "Free and Reduced Percent: ", round(final_district_dataframe$free_and_red_perc, digits = 3), "<br>",
                                          #"ADP Average: ", round(final_district_dataframe$adp_average, digits = 3), "<br>",
                                          "Program Scores: ", final_district_dataframe$meal_type, "<br>",
                                          "SFSP Site Count: ", final_district_dataframe$site_count_sfsp, "<br>",
                                          "SFSP Average ADP: ", final_district_dataframe$adp_sfsp_2018)

sfsp_pal <- colorNumeric(palette = "plasma", 
                         domain = final_district_dataframe$adp_sfsp_2017)
final_district_dataframe %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet() %>% 
  setView(lng = -106, lat = 39.6, zoom = 6.5) %>% 
  addProviderTiles("Esri.WorldGrayCanvas") %>% 
  addPolygons(popup = final_district_dataframe_popups,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.6,
              color = ~ sfsp_pal(adp_sfsp_2017),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", 
            pal = sfsp_pal, 
            values = ~ adp_sfsp_2017,
            title = "adp_sfsp_2017",
            opacity = 1)



 

final_district_dataframe_popups <- paste0("District Name: ", final_district_dataframe$sfa_name, "<br>",
                                          "Free and Reduced Percent: ", round(final_district_dataframe$free_and_red_perc, digits = 3), "<br>",
                                          "Score: ", final_district_dataframe$score, "<br>", 
                                          #"ADP Average: ", round(final_district_dataframe$adp_average, digits = 3), "<br>",
                                          "School Breakfast Program: ", final_district_dataframe$sbp, "<br>",
                                          "National School Lunch Program: ", final_district_dataframe$nslp, "<br>",
                                          "Afterschool Snack Program: ", final_district_dataframe$snack, "<br>",
                                          "Special Milk Program", final_district_dataframe$smp, "<br>"
)

score_pal <- colorNumeric(palette = "plasma", 
                          domain = final_district_dataframe$score)
final_district_dataframe %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas") %>% 
  addPolygons(popup = final_district_dataframe_popups,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.6,
              color = ~ score_pal(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", 
            pal = score_pal, 
            values = ~ score,
            title = "Evaluated Risk Score",
            opacity = 1)
 



# Couple things want multiple layers, SFSP, NSLP, Scores, etc. 

# Need to restrict sizes and movement

# 

final_district_dataframe_popups_sfsp <- paste0(
  "District Name: ", final_district_dataframe$sfa_name, "<br>",
  "Free and Reduced Percent: ", round(final_district_dataframe$free_and_red_perc, digits = 3), "<br>",
  "SFSP Site Count: ", final_district_dataframe$site_count_sfsp, "<br>",
  "SFSP Average ADP: ", final_district_dataframe$adp_sfsp_2018)

sfsp_pal <- colorNumeric(palette = "plasma", 
                         domain = final_district_dataframe$adp_sfsp_2017)
# final_district_dataframe %>% 
#   st_transform(crs = "+init=epsg:4326") %>% 
#   leaflet() %>% 
#   addProviderTiles("Esri.WorldGrayCanvas") %>% 



final_district_dataframe_popups_score <- paste0(
  "District Name: ", final_district_dataframe$sfa_name, "<br>",
  "Free and Reduced Percent: ", round(final_district_dataframe$free_and_red_perc, digits = 3), "<br>",
  "Score: ", round(final_district_dataframe$score, digits = 3), "<br>", 
  "School Breakfast Program: ", final_district_dataframe$sbp, "<br>",
  "National School Lunch Program: ", final_district_dataframe$nslp, "<br>",
  "Afterschool Snack Program: ", final_district_dataframe$snack, "<br>",
  "Special Milk Program: ", final_district_dataframe$smp)

score_pal <- colorNumeric(palette = "plasma", 
                          domain = final_district_dataframe$score)
layer_map <- final_district_dataframe %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet(width = "100%" ,options = leafletOptions(minZoom = 6)) %>% 
  addProviderTiles("Esri.WorldGrayCanvas") %>% 
  setView(lng = -106, lat = 39.6, zoom = 6.5) %>% 
  addPolygons(popup = final_district_dataframe_popups_score,
              group = "Score",
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.6,
              color = ~ score_pal(score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", 
            group = "Score",
            pal = score_pal, 
            values = ~ score,
            title = "Evaluated Risk Score",
            opacity = 1) %>% 
  addPolygons(popup = final_district_dataframe_popups_sfsp,
              group = "SFSP",
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.6,
              color = ~ sfsp_pal(adp_sfsp_2017),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", 
            group = "SFSP",
            pal = sfsp_pal, 
            values = ~ adp_sfsp_2017,
            title = "adp_sfsp_2017",
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Score", "SFSP"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup("SFSP")

# htmlwidgets::saveWidget(layer_map, "layermap.html")

