# CEP Import 

x_to_true <- function( variable) {
  case_when(
    variable == "X" ~TRUE,
    variable == "x" ~TRUE,
    )
}

cep_import_names <- c("text" = "sfa_num",
                      "text" = "sfa_name",
                      "numeric" = "isp_district",
                      "text" = "cep_eligible_district",
                      "text" = "near_eligible_cep_district", 
                      "text" = "cep_participating",
                      "skip" = "x",
                      "numeric" = "sfa_schools", 
                      "numeric" = "sfa_population",
                      "numeric" = "cep_schools", 
                      "numeric" = "cep_population",
                      "skip" = "x",
                      "skip" = "x")

full_cep <- read_excel("./data/cep/2017CEPAnnualNotificationDistrictwideFinal.xlsx", 
                         skip = 8, .name_repair = "universal", n_max = 200 , na = "",
                         col_names = cep_import_names,
                         col_types = names(cep_import_names)) %>% 
  filter(!is.na(sfa_num)) %>% 
  mutate( cep_eligible_district = x_to_true(cep_eligible_district),
          near_eligible_cep_district = x_to_true(near_eligible_cep_district),
          cep_participating = x_to_true(cep_participating),
          cep_score = ((!is.na(cep_population)) * cep_population / sfa_population ))

Scored_cep <- select(full_cep, 
                     sfa_num, cep_score,cep_eligible_district,
                     near_eligible_cep_district, isp_district)
# Decrease clutter and memory space
rm("cep_import_names", "full_cep")
