library(sjPlot)
data(iris)
head(iris)

sjt.df(iris)
table

cro

install.packages("expss")


library(expss)
data(mtcars)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      drat = "Rear axle ratio",
                      wt = "Weight (1000 lbs)",
                      qsec = "1/4 mile time",
                      vs = "Engine",
                      vs = c("V-engine" = 0,
                             "Straight engine" = 1),
                      am = "Transmission",
                      am = c("Automatic" = 0,
                             "Manual"=1),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)


cro(mtcars$am, mtcars$vs)

cro_cpct(mtcars$cyl, list(total(), mtcars$am, mtcars$vs))


mapping_profiles_table_making <- mapping_profiles 

  
cro(mapping_profiles$free_percent, mapping_profiles$Scored_adp)
cro(mapping_profiles$Scored_district_size)

mapping_profiles_table_making <- apply_labels(mapping_profiles_table_making,
                                              sfa_num = "CDE Agreement Number",
                                              cep_score = "Scored CEP Percentage",
                                              scored_eligibility.x = "Profiles Eligibility",
                                              GEOID = "GEOID",
                                              NAME = "District Name",
                                              score_4day = "Four Day School Week",
                                              count_schools = " Number of Schools",
                                              cep_eligible_district  = "District Eligible for CEP",
                                              near_eligible_cep_district = "District Near Eligible for CEP") 
                                               #   "isp_district"               "id"                         "sfa_name"                  
                                               # "district_population"        "free_percent"               "reduced_percent"           
                                               # "school_year"                "urban_rural"                "Scored_adp"                
                                               # "District_population"        "Scored_eligibility.y"       "Scored_district_size"      
                                               # "Scored_rural_urban"         "total_score"                "sbp"                       
                                               # "nslp"                       "smp"                        "snack"                     
                                               # "sfsp"                       "geometry"  
                                              
  
)

cro(mapping_profiles$NAME, )
