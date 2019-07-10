# Plotting
plot(combined_geo_dist["nslp"], main = "NSLP Updated" )
plot(combined_geo_dist["sfsp"], main = "SFSP 2018")



# Plot out using GIS all the different SFAs with whatever data we want to show


#plot(testing_99["free_perc"], main="Percentage Free Students")
#plot(testing_99["redu_perc"], main= "Percentage Reduced Students")
#plot(testing_99["free_and_red_perc"], main= "Percentage Free or Reduced")
#plot(testing_99["sbp"], main= "Has School Breakfast Program")
plot(testing_99["score_4day"], main= "Has 4 day school week") +
     legend(x = ., horiz = TRUE, legend = c("4 Day", "5 Day"))
#plot(testing_99["sfsp"], main = "Has sfsp")
#plot(testing_99["smp"], main = "Has Special Milk Progam")
#plot(testing_99["nslp"], main = "Has NSLP")
#plot(testing_99["snack"], main= "Has Afterschool Snack Program")
plot(testing_99["District_population"], main= "Number of Students")
plot(testing_99["Scored_district_size"], main = "District Size Score")
library(leaflet)
leaflet(co_school_joined) %>% 
  addProviderTiles("Stamen.TonerLite", group = "Stamen.Toner") %>% 
  addPolygons(weight = 1,
              popup = co_school_joined[["NAME"]]) %>% 
  addLabelOnlyMarkers(labelOptions = (noHide = TRUE),
                      label = co_school_joined[["NAME"]])


library(polylabelr)
poi(co_school_joined[["geometry"]])
library(geojsonio)
centroid(co_school_joined)

testing_geoson <- co_school_joined %>% 
  select(geometry) %>% 
  geojson_list()
testing_centroid <- centroid(testing_geoson)
poi(testing_geoson[[1]], testing_geoson[[2]])

leaflet(co_school_joined) %>% 
  addTiles() %>% 
  addGeoJSON(testing_geoson)


gCentroid(co_school_joined)



plot(combined_geo_district[4:6]) 


ggplot(testing_99) +
  geom_point(aes(x = total_score, y = Scored_rural_urban), position = "jitter")


# What are the alterable score portions?
ggplot(testing_99) +
  geom_point(aes(y = total_score, x = Scored_eligibility.y),
             position = "jitter") +
  facet_grid(~Scored_district_size)


# Showing how district size and rural/urban interact
ggplot(testing_99) +
  geom_point(aes(x = total_score, y = Scored_eligibility.y)) +
  facet_grid(Scored_district_size ~ score_4day, switch = "x")

ggplot(testing_99) +
  geom_point(aes(x = total_score, y = Scored_adp)) +
  facet_grid(~Scored_rural_urban, switch = "x")










  score_pal <- colorNumeric(palette = "inferno",
                            domain = testing_99[["total_score"]])
 
  score_popup <-  paste0(
    "District Name: ",
    testing_99[["NAME"]],  "<br>",
    "Score: ",
    round(testing_99[["total_score"]], digits = 3), "<br>",
    "Free and Reduced Percent: ",
    round(testing_99[["Scored_eligibility.y"]], digits = 3), "<br>",
     "Total program score:  ",
    round(testing_99[["Scored_adp"]], digits = 3),  "<br>",
    "Rural/Urban Score:  ",
    testing_99[["Scored_rural_urban"]], "<br>",
    "District size Score: ",
    testing_99[["Scored_district_size"]], "<br>",
    "Four Day Score: ",
    testing_99[["score_4day"]], "<br>",
    "CEP Score: ", testing_99[["cep_score"]])
 
 
 
  leaflet(testing_99) %>%
    addProviderTiles("Stamen.TonerLite", group = "Stamen.Toner") %>%
    addPolygons(weight = 1,
                group = "Score",
                stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.25,
                color = ~ score_pal(total_score),
                popup = score_popup)  %>%
    addLegend(pal = score_pal,
              values = ~total_score)
 
library(tmap)
  tm_shape(testing_99) +
    tm_polygons("total_score") +
    tm_facets(along = "school_year") 
  +
    tmap_animation(filename = "testing_2.mpg",
                   delay = 50, loop = TRUE, restart.delay = 100,
                   width = 800)

ttm()
tm_shape(testing_101) +
  tm_polygons("adp_calculated") +
  tm_layout(main.title = "Calculated ADP NSLP 2017", legend.outside = TRUE )

  
  
#   library(htmlwidgets)
#   library(htmltools)
#   
#   tag.map.title <- tags$style(HTML("
#   .leaflet-control.map-title { 
#     transform: translate(-50%,20%);
#     position: fixed !important;
#     left: 50%;
#     text-align: center;
#     padding-left: 10px; 
#     padding-right: 10px; 
#     background: rgba(255,255,255,0.75);
#     font-weight: bold;
#     font-size: 28px;
#   }
# "))
#   
#   title <- tags$div(
#     tag.map.title, HTML("CDE Scoring")
#   )  
#   
map_leaflet <- leaflet(testing_99) %>%
      addProviderTiles("Stamen.TonerLite", group = "Stamen.Toner") %>%
      addPolygons(weight = 1,
                  group = "Score",
                  stroke = TRUE,
                  smoothFactor = 1,
                  fillOpacity = 0.25,
                  color = ~ score_pal(total_score),
                  popup = score_popup)  %>%
      addLegend(pal = score_pal,
                values = ~total_score) %>% 
        addControl(title, position = "topleft", className="map-title")



leaflet(testing_99) %>% 
  addtiles() %>% 
  addPolygons( color = ~
    
  )


qmap("colorado")
DenverMap <- ggmap(
  get_map("denver", zoom = 14, color = "bw"),
  extent = "device", legend = "topleft"
)


plot(testing_99) +
  legend()


get_map(location = "Denver")
google_key(key)


site_meal_count %>% 
  group_by(sfa_num) %>% 
  ggplot() +
  geom_point(aes(y = total_meals_served, x = claim_date), position = "jitter") +
  facet_wrap(~rate_level, nrow = 2) +
  geom_smooth(aes(y = total_meals_served, x = claim_date))

site_meal_count %>% 
  ggplot() +
  geom_point(aes(y = total_meals_served, x = claim_date), position = "jitter")

site_meal_count %>% 
  group_by(sfa_num, school_year) %>% 
  ggplot() +
  geom_point(aes(y = total_meals_served, x = claim_date), position = "jitter") +
  facet_wrap(~provision) +
  geom_smooth(aes(y = total_meals_served, x = claim_date))

site_meal_count %>% 
  group_by(sfa_num) %>% 
  ggplot() +
  geom_point(aes(y = total_meals_served, x = claim_date), position = "jitter") +
  facet_wrap(~meal_type)
