# SFSP Site Count
# Year and program level
mutate(testing_complete_join,
       Scored_sfsp_sites =  site_count / count_schools )

# District Size Score
# Year and Distric levl
mutate(testing_complete_join,
       Scored_district_size = Score_District_Size(District_population))

# Rural Urban Score
# District Level
mutate(testing_complete_join,
       Scored_rural_urban = Score_ruralurban(urban_rural))

# Interesting thought, average
# (count_days_served  / count_schools) /12


# score_components <- list(Scored_rural_urban,
#                       Scored_district_size,
#                       Scored_eligibility,
#                       cep_score,
#                       Score_4day,
#                       Scored_sfsp_sites,
#                       Scored_program_adp)

# All together now
testing_stage2 <- testing_complete_join %>%
  mutate(#Scored_program_sites =  site_count / count_schools, #district
    Scored_district_size = Score_District_Size(district_population), #district
    Scored_rural_urban = Score_ruralurban(urban_rural), # district
    #adp_score = ((total_meals_served / count_days_served ) /
    adp_per_student = district_population) # program then district



renew <- site_meal_count %>% 
  group_by(school_year, sfa_num, meal_type, site_num) %>% 
  summarise_at(vars(total_meals_served, count_days_served), funs(sum)) %>%
  filter(school_year > 2016) %>% 
  left_join(student_count_total)

testing_99 <- select(combined_sfa_program_level,
                     sfa_num, school_year, adp_calculated, meal_type) %>% 
  left_join(program_population,  by = c("school_year", "sfa_num", "meal_type")) %>% 
  mutate(adp_per_student = adp_calculated / program_student_population) #%>% 



# combine all site level indicators to district program level
combined_sfa_program_level <- site_meal_count %>% 
  group_by(school_year, sfa_num, meal_type) %>% 
  summarise_if(is.numeric, sum) %>% 
  left_join(program_population, by = c("school_year", "sfa_num", "meal_type")) %>% 
  select(-claim_month, -sfsp_site_count) %>% 
  # Added when found score error
  mutate(adp_calculated = total_meals_served / count_days_served) %>% 
  group_by(school_year, sfa_num, meal_type) %>% 
  summarise_if(is.numeric, sum)



# testing_stage3 <- testing_complete_join %>%
#   group_by(school_year, sfa_num) %>%
#   summarize_if(is.numeric, sum, na.rm = TRUE)
#   summarize(Scored_program_adp = sum(adp_score),
#             Totalled_score = sum(c(Scored_rural_urban,
#                                  Scored_district_size,
#                                  Scored_eligibility,
#                                  cep_score,
#                                  score_4day,
#                                  # Scored_sfsp_adp, # not yet calculated
#                                  Scored_program_sites,
#                                  Scored_program_adp), na.rm = TRUE))



testing_filtering_ <- function(df, variable, sy) {
  for(cat in unique(df[[variable]])){
    print(cat)
    df <- df
    variable <- as.character(variable)
    sy <- sy
    tigris::plot(df[variable])
    Filter_program_year(df, cat, sy)
  }
}


testing_filtering_(combined_sfa_program_level, "meal_type", 2017)

for (cat in unique(testing_101[["meal_type"]])) {
  filter(testing_101, meal_type == cat)
  plot(testing_101["adp_calculated"], main = cat)
}
testing_101 <- left_join(co_school_joined,combined_sfa_program_level, by = "sfa_num") %>% 
  filter(school_year == 2017)


tigris::plot(testing_101["meal_type"], main = "meal type")

combined_sfa_program_level %>% 
  filter(meal_type == "NSLP",
         school_year == 2017) %>% 
  left_join(co_school_joined , by = "sfa_num")


combined_sfa_program_level %>% 
  Filter_program_year("NSLP", 2017) %>% 
  
  
  
  Filter_program_year <- function(df, meal, sy){
    filter(df, meal_type == meal,
           school_year == sy)
  }





raw_shapes <- co_school_joined

mapping_for_programs <- geo_join(co_school_joined, scored_adp_programs, by_sp = "sfa_num", by_df = "sfa_num") 
mapping_for_programs <- left_join(co_school_joined, scored_adp_programs, by = "sfa_num")
plot(mapping_for_programs[3])
# This works to iterate through each variable
for (n in names(mapping_for_programs)) {
  print(n)
  plot(mapping_for_programs[n])
}

for (n in unique(mapping_for_programs[["meal_type"]])) {
  print(n)
  plot(mapping_for_programs["meal_type"])
}

mapping_for_programs %>% 
  filter(meal_type == "NSLP",
         school_year == 2017) %>%
  
  mapping_for_programs %$%  
  plot(main = "NSLP 2017")

for (cat in unique(mapping_for_programs[["meal_type"]])) {
  meal_type <- cat
  
  plot(testing_101[[meal_type]], main = meal_type)
}

# NSLP
# Base data coming from combined_sfa_program_level
# Has the data at the district level for each school
# Potential interesting thing might be min/max of each district but that is for later

# Do it the dumb way now, improve later
testing_2017 <- left_join(co_school_joined,combined_sfa_program_level, by = "sfa_num") %>% 
  filter(school_year == 2017) 

testing_nslp <- testing_2017 %>% 
  filter(meal_type == "NSLP")  
plot(testing_nslp["meal_type"])
testing_smp <- testing_2017 %>% 
  filter(meal_type == "SMP")  
plot(testing_smp["meal_type"])


ggplot(data= testing_99)

testing_99 %>% 
  leaflet() %>% 
  addProviderTiles()

base_district_map <- testing_99 %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas") %>% 
  addPolygons(fillColor = "white",
              color = "black",
              weight = 1)
changing_color_pallet <- function(changing_variable){
  colorNumeric(palette = "plasma",
               domain = changing_variable)
}
pal_1 <- changing_color_pallet(testing_99$cep_score)

base_district_map %>% 
  addPolygons(popup= testing_99$District_population,
              color = pal_1(testing_99$District_population) )


combined_sfsp_geodist_popups <- paste0("District Name: ", combined_sfsp_geodist$NAME.x, "<br>",
                                       "Free and Reduced Percent: ", round(combined_sfsp_geodist$free_and_red_perc, digits = 3), "<br>",
                                       #"ADP Average: ", round(combined_sfsp_geodist$adp_average, digits = 3), "<br>",
                                       "Program Scores: ", combined_sfsp_geodist$meal_type, "<br>",
                                       "SFSP Site Count: ", combined_sfsp_geodist$site_count_sfsp, "<br>",
                                       "SFSP Average ADP: ", combined_sfsp_geodist$adp_sfsp_2018)

sfsp_pal <- colorNumeric(palette = "plasma", 
                         domain = c(1, 100))
m <- combined_sfsp_geodist %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas") %>% 
  addPolygons(popup = combined_sfsp_geodist_popups,
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

qmap("Poudre School District")
plot(mapping_for_programs[]) +
  labs(title = "Testing labs")


