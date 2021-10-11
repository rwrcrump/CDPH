
temp <- read.socrata("https://data.cityofchicago.org/resource/yhhz-zm2v.json")

na_zips <- test %>% 
  filter(
    is.na(zip_code_location.type)
    ) %>% 
  view()

test <- chi_covid_stats %>% 
  select(
    c(date,
      contains("rate")),
    -c(contains("cases"),
       contains("hospitalizations"),
       contains("male"))
    ) %>% 
  mutate_if(
    is.character, as.numeric
    ) %>% 
  rowwise() %>% 
  mutate(
    deaths_over_60 = sum(c_across(deaths_rate_age_60_69:deaths_rate_age_80)),
    deaths_under_60 = sum(c_across(deaths_rate_age_0_17:deaths_rate_age_50_59)),
    ) %>% 
  select(
    -contains("age")
    ) %>% 
  # pivot_longer(
  #   cols = "deaths_over_60":"deaths_under_60",
  #   names_to = "age",
  #   values_to = "deaths_by_age"
  #   ) %>% 
  # pivot_longer(
  #   cols = "deaths_latinx":"deaths_other_race_non_latinx",
  #   names_to = "race",
  #   values_to = "deaths_by_race"
  # ) %>% 
  # mutate(
  #   age = str_remove(age, "deaths_"),
  #   race = str_remove(race, "deaths_"),
  #   race = str_remove(race, "_non_latinx")
  #   ) %>% 
  glimpse()


test2 <- chi_covid_stats %>% 
  mutate_if(
    is.character, as.numeric
  ) %>% 
  rowwise() %>% 
  mutate(
    deaths_rate_over_60 = sum(c_across(deaths_rate_age_60_69:deaths_rate_age_80)),
    deaths_rate_under_60 = sum(c_across(deaths_rate_age_0_17:deaths_rate_age_50_59)),
  ) %>% 
  select(
    -contains("age")
  ) %>% 
  pivot_longer(
    cols = "deaths_rate_over_60":"deaths_rate_under_60",
    names_to = "age",
    values_to = "death_rate_by_age"
  ) %>% 
  mutate(
    week_ends = as.Date(floor_date(date, "week"))
  ) %>% 
  group_by(
    week_ends, age
  ) %>% 
  summarize(week_sums = sum(death_rate_by_age)
  ) 
            