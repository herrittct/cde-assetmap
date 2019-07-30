# Make California Tract "Shape" file
library(tigris)
options(tigris_class = "sf")  
caTract  <- tracts(state = "CO", cb = TRUE)  


# Read file that links tracts to "communities"
cbdLinkCA  <- read.csv("https://raw.githubusercontent.com/mcSamuelDataSci/CACommunityBurden/master/myCBD/myInfo/cbdLinkCA.csv",colClasses = "character") 


# merge Map and link file
library(dplyr)
caTract <- caTract %>% 
  left_join(cbdLinkCA, by="GEOID")

# new "Shape" file based on communites
shape_Comm  <- caTract %>% 
  group_by(county,comName) %>% 
  summarize() %>% 
  ungroup()

# Filter to one county for example
shape_Comm  <- filter(shape_Comm,county=="Alameda")


# Make dumb map with community labels
library(tmap)
tm_shape(shape_Comm) + 
  tm_polygons(col="county") + 
  tm_text("comName")


library(tmap)
tm_shape(co_school_joined) +
  tm_polygons(col = "count_schools") +
  tm_text("NAME")


#http://stackoverflow.com/questions/20241065/r-barplot-wrapping-long-text-labels

# Core wrapping function
wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}

# Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

shape_Comm <- co_school_joined %>% 
  mutate(Name2 = wrap.labels(NAME,20))

map_layer_tm <- tm_shape(shape_Comm) + 
  #tm_polygons(col="count_schools") + 
  tm_text("Name2", size = 1,
          just = "center", )
tmap_leaflet(map_layer_tm)



