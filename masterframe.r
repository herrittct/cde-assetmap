# What would my perfect dataframe look like?
# Each site and each program and each period 
# I think meal count datasets will be the primary (left) joined sides

# school X | Year Y | Month Q |Program Z |Free&Red rate |student Enrollment| 
# Urban/Rural| CEP Y/N | Meal Count | 4day

# School = site_id (Standard accross CDE)
# Year = match with the different data sets ymd(variable) mutate year(variable)
# Month = Match with the different data sets ymd(variable) mutate month (variable)
# Program = match with the meal count dataset
http://www.cde.state.co.us/nutrition/datarequestsmealcounts
# Free and red = match with the yearly data " PK-12 Free and Reduced Lunch" from below url
http://www.cde.state.co.us/cdereval/rvprioryearpmdata
# student enrollment= will also come from the above data 
# Urban or Rural = either by zip code tabulation areas or census tract choose one then do it
https://www.ers.usda.gov/data-products/frontier-and-remote-area-codes/frontier-and-remote-area-codes/#2010%20Frontier%20and%20Remote%20Area%20Codes%20Data%20Files
  https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes.aspx
# CEP = from the CEP file ISP, Eligible, Near Eligibile, Participating, total schools CEP, total student enrollment CEP
CEP Annual Notification
# Meal Count = for each program area total ADP for the year
# four_day = percent of school days with 4 day school weeks, get from 
# "Sponsor Site Data", includes M-th and T-F elements