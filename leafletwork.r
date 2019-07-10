# Leaflet
library(leaflet)
library(ggmap)



base_colorado_map <- raw_shapes %>% 
   st_transform(crs = "+init=epsg:4326") %>% 
  leaflet(options = 
                               leafletOptions(minZoom = 6, dragging = TRUE)) %>%
   addProviderTiles("Esri.WorldGrayCanvas") %>% 
  setView(lng = -106, lat = 39.6, zoom = 7) %>% 
  addPolygons(data = raw_shapes,
              fillColor = "blue",
              color =  "#b2aeae",
              fillOpacity = 0.7,
              weight =  1,
              smoothFactor = 0.2)

pal <- colorQuantile(palette = "viridis",
                     domain = co_school_joined$estimate, n = 10)

short_co_schools <- head(colorado_school)

class(short_co_schools$geometry)

leaflet() %>% 
  addPolygons(base_colorado_map ,short_co_schools)

geocode("Colorado", key = key)

fitBounds()
setView
dragging = FALSE
minZoom
MaxZoom
maxBounds
addPopups()


leaflet() 
  
# From https://juliasilge.com/blog/using-tidycensus/
  
co_pop <- get_acs(geography = "county", 
                    variables = "B01003_001", 
                    state = "CO",
                    geometry = TRUE,
                    key = apikey)

pal <- colorNumeric(palette = "plasma", 
                    domain = co_pop$estimate)

co_pop %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "County Populations",
            opacity = 1)


# this actually makes a map in leaflet with the SFA shapes

pal2 <- colorNumeric(palette = "plasma", 
                    domain = colorado_school$ALAND)


colorado_school %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal2(ALAND)) %>%
  addLegend("bottomright", 
            pal = pal2, 
            values = ~ ALAND,
            title = "County Populations",
            opacity = 1)

pal3 <- colorNumeric(palette = "plasma", 
                     domain = combined_geo_dist$student_count)


popup_testing <- paste0("District Name: ", combined_geo_dist$NAME.x, "<br>",
                "Free and Reduced Percent: ", round(combined_geo_dist$free_and_red_perc, digits = 3))

combined_geo_dist %>% 
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = popup_testing,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal3(student_count)) %>%
  addLegend("bottomright", 
            pal = pal3, 
            values = ~ student_count,
            title = "Student Count",
            opacity = 1)

popup_testing99 <- paste0("District Name: ", testing_99$NAME.x, "<br>",
                        "Free and Reduced Percent: ", round(testing_99$free_and_red_perc, digits = 3), "<br>",
                        "ADP Average: ", round(testing_99$adp_average, digits = 3), "<br>",
                        "Program Scores: ", testing_99$meal_type)

testing_99 %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = popup_testing99,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.6,
              color = ~ pal3(student_count),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", 
            pal = pal3, 
            values = ~ student_count,
            title = "Student Count",
            opacity = 1)

combined_sfsp_geodist_popups <- paste0("District Name: ", combined_sfsp_geodist$NAME.x, "<br>",
                                       "Free and Reduced Percent: ", round(combined_sfsp_geodist$free_and_red_perc, digits = 3), "<br>",
                                       #"ADP Average: ", round(combined_sfsp_geodist$adp_average, digits = 3), "<br>",
                                       "Program Scores: ", combined_sfsp_geodist$meal_type, "<br>",
                                       "SFSP Site Count: ", combined_sfsp_geodist$site_count_sfsp, "<br>",
                                       "SFSP Average ADP: ", combined_sfsp_geodist$adp_sfsp_2018)

sfsp_pal <- colorNumeric(palette = "plasma", 
                                domain = combined_sfsp_geodist$adp_sfsp_2017)
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

saveWidget(m, "mapsfsp.html", selfcontained = FALSE)


