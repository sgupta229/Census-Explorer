format <- function(a) {
  data <- get_acs(geography = "state", 
          variables = "B19013_001",
          shift_geo = TRUE,
          geometry = TRUE,
          year = a, 
          survey = "acs5")
  data <- data %>% 
    mutate(year = a)
  
  data
}

df <- map_df(2014:2018, format)

df <- df %>% 
  janitor::clean_names()

saveRDS(df, "data/income_time.rds")

View(df)
