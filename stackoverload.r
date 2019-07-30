df <- tibble(id= c("a","b","c","d"),
           population = c(10,200,3000,40000))
df <- mutate(df, 
             quartile = ntile(x = population, 4))
  summarize(df, quartile = ntile(2, 4)) %>% 
  group_by("id")

