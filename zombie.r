library(rmapshaper)
library(tidyverse)
library(sf)
library(readxl)
library(leaflet)
 
# retreived from 
shapes_co2 <- read_sf(dsn = "./data/shape_file18", layer = "tl_2018_08_unsd") %>% 
  select(GEOID, 
         NAME, 
         geometry)

site_all <- read_excel("./data/CDE_to_GEOID.xlsx", col_types = "text") %>% 
  mutate(GEOID = ACTUAL_GEOID) %>% 
  select(GEOID, CDE_AGREEMENT, everything(), -ACTUAL_GEOID)

site_all <- group_by(site_all,
                     CDE_AGREEMENT) %>% 
  summarise(count_schools = n(),
            GEOID = first(GEOID))

# Gets all the districts with their site count and GEOID


zombie_source <- select(site_all,
                        CDE_AGREEMENT, 
                        count_schools, 
                        GEOID) %>% 
  rename("sfa_num" = "CDE_AGREEMENT")


rework_frame <- left_join(shapes_co2, zombie_source, by = "GEOID" ) %>% 
  #st_transform(crs = "+proj=longlat +datum=NAD83 +no_defs") # Get error message
  st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  rmapshaper::ms_simplify()

leaflet(rework_frame) %>%
  addTiles() %>% 
  #addProviderTiles("Esri.WorldGrayCanvas") %>% 
  addPolygons(popup = rework_frame$NAME,
              fillColor = "white",
              color = "black",
              weight = 1)


# Redoing the SFSP on 4/23/19
sfsp_path <- "./data/sfsp/SFSP_Meal_Counts_2013-2018.xls"




sfsp_var_names <- c("date" = "claim_date",
                    "text" = "revision_nbr",
                    "text" = "AgreementNbr",
                    "text" = "sfa_name",
                    "text" = "SponsorTypeDesc",
                    "numeric" = "SiteQty",
                    "numeric" = "DaysServedQty",
                    "skip" = "x", "skip" = "x", "skip" = "x", "skip" = "x",
                    "numeric" = "MlsServedFree",
                    "numeric" = "MlsServedTotal",
                    "skip" = "x", "skip" = "x", "skip" = "x", "skip" = "x",
                    "skip" = "x", "skip" = "x", "skip" = "x", "skip" = "x",
                    "skip" = "x", "skip" = "x", "skip" = "x", "skip" = "x",
                    "text" = "MealTypeCode",
                    "text" = "siteNbr",
                    "text" = "SiteName")

sfsp_sheets <- sfsp_path %>%
  excel_sheets() %>% 
  set_names() %>% 
  map_df(~ read_excel(sfsp_path, sheet = .x, skip = 7,
                      col_names = sfsp_var_names,
                      col_types = names(sfsp_var_names),
                      na = "0"),
         .id = "sheet") %>% 
  filter(SponsorTypeDesc == "School Food Authority",
         !is.na(SiteQty)) %>% 
  mutate(sfa_num_char = Pad_to_character(AgreementNbr),
         site_num_char = Pad_to_character(siteNbr),
         MealTypeCode = rename_meal_types(MealTypeCode),
         claim_month = month(claim_date),
         school_year = year(claim_date) - 
           (month(claim_date) <= 7)) %>% 
  #group_by(SiteNbr, MealTypeCode, sfa_num)
  group_by(MealTypeCode, sfa_num_char, school_year) %>% 
  select(school_year, SiteQty:claim_month, claim_date,-SiteName, -siteNbr) %>% 
  rename(meal_type = MealTypeCode,
         count_days_served = DaysServedQty,
         total_meals_served = MlsServedTotal,
         free_meals_served = MlsServedFree,
         sfsp_site_count = SiteQty)

filter(sfsp_sheets, str_view_all(meal_type,"^SFSP_BREAKFAST"))

testing_zombine <- full_join(district_meal_count, sfsp_sheets)






sfsp_sites_summarized <- summarize(sfsp_sheets,
                                   site_count = sum(SiteQty),
                                   day_count = sum(DaysServedQty),
                                   total_meals = sum(MlsServedTotal),
                                   free_meals = sum(MlsServedFree),
                                   calc_adp = total_meals / day_count)




ggplot(data = sfsp_sites_summarized) +
  geom_point(mapping = aes(x = total_meals, y = site_count)) +
  facet_grid(~school_year)

ggplot(data = sfsp_sites_summarized) +
  geom_point(mapping = aes(x = total_meals, y = site_count)) 

rework_frame <- left_join(rework_frame, sfsp_sites_summarized, by = "sfa_num")

pal_rework <- colorBin(palette = "viridis",
                            domain = rework_frame[["calc_adp"]], n = 5)

rework_frame %>% 
  leaflet() %>% 
  addTiles() %>% 
  #addProviderTiles("Esri.WorldGrayCanvas") %>% 
  addPolygons(popup = rework_frame$NAME,
              fillColor = ~pal_rework(calc_adp),
              color = "black",
              weight = 1) %>% 
  addLegend(pal = pal_rework,
            values = ~calc_adp)

## Currently does not work
map_a_datapoint <- function(df, datapoint){

pal_mapping<- colorBin(palette = "viridis",
                       domain = df[[datapoint]])
leaflet(df) %>% 
  addTiles() %>% 
  addPolygons(popup = df[[datapoint]],
              fillColor = ~pal_mapping(datapoint),
              color = "black",
              weight = 1)  
}

map_a_datapoint(rework_frame, datapoint = "site_count")
