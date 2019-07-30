# CDE Mapping Shape File Load

# retreived from 
shapes_co2 <- read_sf(dsn = "./data/shape_file18", layer = "tl_2018_08_unsd") %>% 
  select(GEOID, 
         NAME, 
         geometry)

# Load the GEOID to CDE Identifier data
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


co_school_joined <- left_join(shapes_co2, zombie_source, by = "GEOID" ) %>% 
  #st_transform(crs = "+proj=longlat +datum=NAD83 +no_defs") # Get error message
  st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  rmapshaper::ms_simplify()

rm("zombie_source", "site_all", "shapes_co2")
