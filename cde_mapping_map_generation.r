options(scipen=999)

# All Maps

# #static
# NSLP
# SBP
# ASP
# SMP
# SFSP
# F/R%
# ISP%
# Scores
# 
# # Leaflet
# F/R% (num free, num redu, % free, % redu)
# ISP% (near, participating CEP percent)
# Score( All score components)
# 

# Get Data
mapping_profiles <- mutate(mapping_profiles,
       nslp_sbp = !is.na(nslp) & !is.na(sbp),
       nslp_sbp_asp = !is.na(nslp) & !is.na(sbp) & !is.na(snack),
       nslp_sbp_asp_sfsp = !is.na(nslp) & !is.na(sbp) & !is.na(snack) & !is.na(sfsp)) 


mapping_profiles2 <- mapping_profiles %>% 
        mutate(free_percent = round(free_percent, 3))


# Leaflet Map Popups
# CEP Layer Popups
cep_profiles_popups <- paste0(
        "District Name: ", mapping_profiles$sfa_name, "<br>",
        "CEP Participation Score: ", round(mapping_profiles[["cep_score"]]*100, digits = 1), "<br>",
        "Identified Student Percentage: ", round(mapping_profiles[["isp_district"]]*100, digits = 1), "%","<br>",
        "Eligible or Near Participation Requirement: ", 
        !is.na(mapping_profiles[["near_eligible_cep_district"]]) | !is.na(mapping_profiles[["cep_eligible_district"]]) ,
        "<br>",
        "CEP Participation: ", !is.na(mapping_profiles[["cep_score"]]),"<br>",
        "Students Receiving Free Benefits: ",round(mapping_profiles[["free_percent"]]*100, digits = 1), "%","<br>",
        "Students Receiving Reduced Price Benefits: ",round(mapping_profiles[["reduced_percent"]]*100, digits = 1), "%", "<br>")

cep_pal <- colorNumeric(palette = "plasma", 
                         domain = mapping_profiles$cep_score)

# Programs Layer Popups
mapping_profiles_popups_programs <- paste0(
        "District Name: ", mapping_profiles$sfa_name, "<br>",
        "Score: ", round(mapping_profiles$total_score, digits = 3), "<br>", 
        "Free and Reduced Percent: ", round(mapping_profiles$Scored_eligibility.y*100, digits = 1),"%", "<br>",
        "School Breakfast Program: ", !is.na(mapping_profiles[["sbp"]]), "<br>",
        "National School Lunch Program: ", !is.na(mapping_profiles[["nslp"]]), "<br>",
        "Afterschool Snack Program: ", !is.na(mapping_profiles[["snack"]]), "<br>",
        "Special Milk Program: ", !is.na(mapping_profiles[["smp"]]), "<br>",
        "Summer Food Service Program: ", !is.na(mapping_profiles[["sfsp"]]), "<br>"
        )

programs_pal <- colorFactor(palette = "plasma", 
                         domain = mapping_profiles$nslp_sbp_asp_sfsp)
        
# Score Layer Popups 
mapping_profiles_popup_score_components <- paste0("District Name: ", mapping_profiles[["sfa_name"]], "<br>",
       "Score: ", round(mapping_profiles[["total_score"]], digits = 3), "<br>", 
       "CEP Score: ", mapping_profiles[["cep_score"]],"<br>",
       "4-day Week Score: ", mapping_profiles[["score_4day"]],"<br>",
       "Program ADP Score: ", round(mapping_profiles[["Scored_adp"]], digits = 3),"<br>",
       "Rural/Urban Score: ", mapping_profiles[["Scored_rural_urban"]],"<br>",
       "Free and Reduced Eligible Score: ", mapping_profiles[["Scored_eligibility.y"]], "<br>",
       "District Size Score:", mapping_profiles[["Scored_district_size"]])

total_score_pal <- colorNumeric(palette = "plasma", 
                                domain = mapping_profiles[["total_score"]])

isp_pal <- colorNumeric(palette = "plasma", 
                        domain = mapping_profiles$isp_district*100)

# Actual Leaflet Map

score_cep_leaflet <-          
        mapping_profiles %>% 
        leaflet(options = leafletOptions(minZoom = 6)) %>% 
        addProviderTiles("Esri.WorldGrayCanvas") %>% 
        addPolygons(popup = mapping_profiles_popups_programs ,
                    group = "Programs",
                    stroke = TRUE,
                    weight = 1,
                    smoothFactor = 0,
                    color = "#FFFFFF",
                    fillOpacity = 0.4,
                    dashArray = '20 5',
                    fillColor = ~ programs_pal(nslp_sbp_asp_sfsp)) %>%
        addLegend("topleft",
                  group = "Programs",
                  pal = programs_pal,
                  values = ~nslp_sbp_asp_sfsp,
                  title = "Has all 4 programs <br> NSLP <br> SBP <br> ASP <br> SFSP",
                  opacity = 1) %>%
        addPolygons(popup = cep_profiles_popups ,
                    group = "CEP",
                    stroke = TRUE,
                    weight = 1,
                    smoothFactor = 0,
                    color = "#FFFFFF",
                    fillOpacity = 0.4,
                    dashArray = '20 5',
                    fillColor = ~ isp_pal(isp_district*100)) %>%
        addLegend("topleft", 
                  group = "CEP",
                  pal = isp_pal, 
                  values = ~ isp_district*100,
                  title = "Identified <br> Student <br> Percentage",
                  opacity = 1) %>%
        addPolygons(popup = mapping_profiles_popup_score_components,
                    group = "Total Score",
                    stroke = TRUE,
                    weight = 1,
                    smoothFactor = 0,
                    color = "#FFFFFF",
                    fillOpacity = 0.4,
                    dashArray = '20 5',
                    fillColor = ~ total_score_pal(total_score)) %>%
        addLegend("topleft", 
                  group = "Total Score",
                  pal = total_score_pal, 
                  values = ~ total_score,
                  title = "Total Score",
                  opacity = 1) %>% 
        addLayersControl(
                overlayGroups = c("Total Score", "CEP", "Programs"),
                options = layersControlOptions(collapsed = FALSE, position = "topleft")) %>% 
        hideGroup(c("CEP", "Programs"))

# Make into an html widget
htmlwidgets::saveWidget(score_cep_leaflet,
                          file = "Totalscore_CEP_Programs-leaflet.html", 
                          selfcontained= TRUE)

### Plots of different variables
mapping_profiles %>% 
        filter(school_year == 2017) %>% 
        ggplot() +
        geom_histogram(aes(x = Scored_adp)) +
        facet_grid(~urban_rural) 

mapping_profiles %>% 
        filter(school_year == 2017) %>% 
        ggplot() +
        geom_histogram(aes(x = Scored_district_size )) +
        facet_wrap(~nslp_sbp_asp_sfsp) + 
        labs(title = "Public School Districts Currently Operating All Four \nKey Programs Compared to Scored District Size",
             subtitle = "Key Programs: NSLP, SBP, ASP, SFSP. Operating in at least 1 school",
             x = "District Size score",
             y = "Number of Districts",
             caption = " Based on 2017 data") +
        theme_minimal() +
        theme(plot.title = element_text(size = 20, face = "bold"))

mapping_profiles %>% 
        filter(school_year == 2017) %>% 
        ggplot() +
        geom_histogram(aes(x = Scored_district_size )) +
        facet_wrap(~ifelse(nslp_sbp_asp_sfsp,
                           "Has All Four Programs",
                           "Does Not Have All Programs")) + 
        labs(title = "Public School Districts Currently Operating All Four \nKey Programs Compared to Scored District Size",
             subtitle = "Key Programs: NSLP, SBP, ASP, SFSP.\nOperating in at least 1 school",
             x = "District Size score",
             y = "Number of Districts",
             caption = " Based on 2017 data") +
        theme_minimal() +
        theme(plot.title = element_text(size = 20, face = "bold"))

mapping_profiles %>% 
        filter(school_year == 2016) %>% 
        ggplot() +
        geom_histogram(aes(x = Scored_district_size )) +
        facet_wrap(~nslp_sbp_asp) + 
        labs(title = "Has 3 key programs compared to scored district size",
             subtitle = " NSLP, SBP, ASP",
             x = "District Size score",
             y = "Number of Districts",
             caption = " Based on 2016 data") +
        theme_minimal()

mapping_profiles %>% 
        filter(school_year == 2016) %>% 
        ggplot() +
        geom_histogram(aes(x = Scored_district_size )) +
        facet_wrap(~nslp_sbp) + 
        labs(title = "Has primary programs compared to scored district size",
             subtitle = " NSLP, SBP",
             x = "District Size score",
             y = "Number of Districts",
             caption = " Based on 2016 data")

mapping_profiles %>% 
        filter(school_year == 2016) %>% 
        ggplot() +
        #geom_histogram(aes(x = Scored_rural_urban)) +
        geom_histogram(aes(x = urban_rural), stat = "count") +
        facet_wrap(~snack)

mapping_profiles %>% 
        filter(school_year == 2016) %>% 
        map_data() %>% 
        ggplot() +
        geom_map(aes(fill = score_4day))

mapping_profiles %>%
        filter(school_year == 2017) %>% 
        ggplot() + 
        geom_sf(aes(fill = nslp_sbp_asp_sfsp)) +
        labs(caption = " Based on 2017 data") +
        ggtitle(label = "Public School Districts Currently\nOperating All Four Key Programs",
                subtitle = "Key Programs: NSLP, SBP, ASP, SFSP.\nOperating in at least 1 school") +
        guides(fill = guide_legend( title = "Has All Four \nPrimary Programs",
                                    title.theme = element_text(
                                            size = 15,
                                            face = "italic",
                                            angle = 0))) +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.ticks = element_blank(),
              rect = element_blank(),
              panel.grid.major = element_line(colour = 'transparent')) 
        

        
#Final CDE Maps
size_vectors_2 <- c( "<50" = 0.33,
                     "51-500" = 0.67, 
                     "501-1000" = 1, 
                     "1001-5000" = 1.34,
                     "5001-10000" = 1.67,
                     ">10000" = 2)  

mapping_profiles_levels <- mapping_profiles %>% 
        mutate(Scored_district_size = factor(Scored_district_size, 
                                             ordered = TRUE, 
                                             levels = size_vectors_2))

# District has all 4 Child Nutrition Programs 
mapping_profiles %>%
        filter(school_year == 2017) %>% 
        ggplot() + 
        geom_sf(aes(fill = nslp_sbp_asp_sfsp)) +
        labs(caption = " Based on 2017 data") +
        ggtitle(label = "Public School Districts Currently\nOperating All Four Key Programs",
                subtitle = "Key Programs: NSLP, SBP, ASP, SFSP.\nOperating in at least 1 school") +
        guides(fill = guide_legend( title = "Has All Four \nPrimary Programs",
                                    title.theme = element_text(
                                            size = 15,
                                            face = "italic",
                                            angle = 0))) +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.ticks = element_blank(),
              rect = element_blank(),
              panel.grid.major = element_line(colour = 'transparent')) 

# District is considered Urban / Rural Map
mapping_profiles %>%
        filter(school_year == 2017) %>% 
        ggplot() + 
        geom_sf(aes(fill = urban_rural)) +
        labs(caption = " Based on 2017 data") +
        ggtitle(label = "Classified as Urban or Rural ",
                subtitle = "Showing which school districts are \nclassified Urban or Rural school districts") +
        guides(fill = guide_legend( title = "Urban or Rural \nDistrict",
                                    title.theme = element_text(
                                            size = 15,
                                            face = "italic",
                                            angle = 0))) +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.ticks = element_blank(),
              rect = element_blank(),
              panel.grid.major = element_line(colour = 'transparent')) 

# Four Day School Week Map
mapping_profiles %>%
        filter(school_year == 2017) %>% 
        mutate(four_day_tf = ifelse(score_4day==0, "Four Day", "Five Day")) %>% 
        ggplot() + 
        #geom_sf(aes(fill = score_4day)) +
        geom_sf(aes(fill = four_day_tf)) +
        labs(caption = " Based on 2017-2018 data") +
        ggtitle(label = "Four Day School Week (2017) ",
                subtitle = "The duration of the traditional school week in the stated year") +
        guides(fill = guide_legend( title = "School Week Length",
                                    title.theme = element_text(
                                            size = 15,
                                            face = "italic",
                                            angle = 0))) +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.ticks = element_blank(),
              rect = element_blank(),
              panel.grid.major = element_line(colour = 'transparent')) 

# Free Reduced Map 2017
mapping_profiles %>%
        filter(school_year == 2017) %>% 
        ggplot() + 
        geom_sf(aes(fill = Scored_eligibility.y)) +
        labs(caption = " Based on 2017-2018 data") +
        ggtitle(label = "Free & Reduced Eligible Student Percentage (2017) ",
                subtitle = "The percentage of students identified as eligible for free or reduced price meals") +
        guides(fill = guide_legend( title = "Free & Reduced \nPrice Eligible",
                                    title.theme = element_text(
                                            size = 15,
                                            face = "italic",
                                            angle = 0))) +
        scale_fill_viridis_c() +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.ticks = element_blank(),
              rect = element_blank(),
              panel.grid.major = element_line(colour = 'transparent')) 


# Free Reduced Map 2016
mapping_profiles_levels %>%
        filter(school_year == 2016) %>% 
        ggplot() + 
        geom_sf(aes(fill = Scored_eligibility.y)) +
        labs(caption = " Based on 2016-2017 data") +
        ggtitle(label = "Free & Reduced Eligible Student Percentage (2016) ",
                subtitle = "The percentage of students identified as eligible for free or reduced price meals") +
        guides(fill = guide_legend( title = "Free & Reduced \nPrice Eligible",
                                    title.theme = element_text(
                                            size = 15,
                                            face = "italic",
                                            angle = 0))) +
        scale_fill_viridis_c() +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.ticks = element_blank(),
              rect = element_blank(),
              panel.grid.major = element_line(colour = 'transparent')) 


# SBP Participation 2017
mapping_profiles_levels %>%
        filter(school_year == 2017) %>% 
        ggplot() + 
        geom_sf(aes(fill = sbp)) +
        labs(caption = " Based on 2017-2018 data") +
        ggtitle(label = "School Breakfast Program \nParticipation (2017) ",
                subtitle = "At least one school Participates in SBP") +
        guides(fill = guide_legend( title = "School Breakfast \nProgram Participation",
                                    title.theme = element_text(
                                            size = 15,
                                            face = "italic",
                                            angle = 0))) +
        scale_fill_discrete(labels = c("Participates", "Does Not Participate")) +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.ticks = element_blank(),
              rect = element_blank(),
              panel.grid.major = element_line(colour = 'transparent')) 

# SFSP Participation 2017
mapping_profiles_levels %>%
        filter(school_year == 2017) %>% 
        ggplot() + 
        geom_sf(aes(fill = sfsp)) +
        labs(caption = " Based on 2017-2018 data") +
        ggtitle(label = "Summer Food Service Program (2017) ",
                subtitle = "At least one school Participates in SFSP") +
        guides(fill = guide_legend( title = "SFSP \nProgram Participation",
                                    title.theme = element_text(
                                            size = 15,
                                            face = "italic",
                                            angle = 0))) +
        scale_fill_discrete(labels = c("Participates", "Does Not Participate")) +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.ticks = element_blank(),
              rect = element_blank(),
              panel.grid.major = element_line(colour = 'transparent')) 

# ASP Participation 2017
mapping_profiles %>%
        filter(school_year == 2017) %>% 
        ggplot() + 
        geom_sf(aes(fill = snack)) +
        labs(caption = " Based on 2017-2018 data") +
        ggtitle(label = "After School Snack Program (2017) ",
                subtitle = "At least one school Participates in ASP") +
        guides(fill = guide_legend( title = "ASP \nProgram Participation",
                                    title.theme = element_text(
                                            size = 15,
                                            face = "italic",
                                            angle = 0))) +
        scale_fill_discrete(labels = c("Participates", "Does Not Participate")) +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.ticks = element_blank(),
              rect = element_blank(),
              panel.grid.major = element_line(colour = 'transparent')) 

# CEP Eligible 2017
mapping_profiles %>%
        filter(school_year == 2017) %>% 
        ggplot() + 
        geom_sf(aes(fill = cep_eligible_district)) +
        labs(caption = " Based on 2017-2018 data") +
        ggtitle(label = "Community Eligiblity Program minimum ISP met (2017) ",
                subtitle = "The school district has at least 40% ISP indicating eligiblity for CEP provision") +
        guides(fill = guide_legend( title = "CEP \nProgram Eligibility",
                                    title.theme = element_text(
                                            size = 15,
                                            face = "italic",
                                            angle = 0))) +
        scale_fill_discrete(labels = c("Eligible", "Not Eligible")) +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.ticks = element_blank(),
              rect = element_blank(),
              panel.grid.major = element_line(colour = 'transparent')) 

# CEP Partitcipation 2017
mapping_profiles_levels %>%
        filter(school_year == 2017) %>% 
        mutate(cep_score_char = paste0(round(cep_score,2)*100,"%")) %>% 
        ggplot() + 
        #geom_sf(aes(fill = cep_score_char)) +
        geom_sf(aes(fill = cep_score)) +
        labs(caption = " Based on 2017-2018 data") +
        ggtitle(label = "Community Eligiblity Provision Participation (2017) ",
                subtitle = "Percentage of school district student population enrolled in CEP") +
        guides(fill = guide_legend( title = "Percentage of CEP \nProgram Participation",
                                    title.theme = element_text(
                                            size = 15,
                                            face = "italic",
                                            angle = 0))) +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.ticks = element_blank(),
              rect = element_blank(),
              panel.grid.major = element_line(colour = 'transparent')) 


# District Size Map
mapping_profiles_levels %>%
        filter(school_year == 2017) %>% 
        ggplot() + 
        geom_sf(aes(fill = Scored_district_size)) +
        labs(caption = " Based on 2017-2018 data") +
        ggtitle(label = "School District Population (2017) ",
                subtitle = "District Population separated into district \nsize groupings according to CDE size cutoffs") +
        guides(fill = guide_legend( title = "Student Population",
                                    title.theme = element_text(
                                            size = 15,
                                            face = "italic",
                                            angle = 0)), nrow = 6) +
        scale_fill_viridis_d(labels = c(
                "<50", "51-500","501-1000", "1001-5000",
                "5001-10000", ">10000" )) +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.ticks = element_blank(),
              rect = element_blank(),
              panel.grid.major = element_line(colour = 'transparent')) 
