# overview of recent CDPH budgets and covid stats

# Setup -------------------------------------------------------------------

library(tidyverse)
library(RSocrata)
library(scales)
library(ggthemes)
library(lubridate)

rm(list = ls())

options(scipen = 999)

setwd("D:/GitHub/CDPH")

# City Budget Data --------------------------------------------------------

# data source: data.cityofchicago.gov

# build API list

API_list_budget <- c("h9rt-tsn7", # 2019
                     "fyin-2vyd", # 2020
                     "6tbx-h7y2", # 2021
                     "ncj3-k47t") # 2022 (recommended)

# define seed year
year = 2019

# create empty data frame to populate with for loop
budget_ordinances <- data.frame()

# build data.frame for all available budget ordinances
for (i in API_list_budget) {
  
  # build API call corresponding to each year
  x <- paste0("https://data.cityofchicago.org/resource/", i, ".json")
  
  # pull dataset from data.cityofchicago.org
  y <- read.socrata(x)
  
  # add year column seeding at 2019
  y$year = year
  
  # iterate years for each API call. since these are ordered in the list above, it will generate the correct value for each fiscal year of the relevant budget ordinance
  year = year + 1
  
  # combine all annual budgets into a single large data.frame
  budget_ordinances <- bind_rows(budget_ordinances, y)
}

# standardize ordinance amount using recommended appropriation from 2022

budget_ordinances_clean <- budget_ordinances %>% 
  mutate(
    ordinance_amount = case_when(
      year == 2019 | 2020 |2021 ~ as.numeric(X_ordinance_amount_),
      year == 2022 ~ as.numeric(recommendation))
    ) %>% 
  select(-c(X_ordinance_amount_,
            appropriation,
            revised_appropriation,
            recommendation)
    ) 

# visualization
budget_ordinances_clean %>% 
  filter(department_number == "41") %>% 
  group_by(year, fund_type) %>% 
  summarize(
    total_app = round(sum(ordinance_amount)/1000000, 0)
    ) %>% 
  ggplot(
    aes(
      x = year,
      y = total_app, 
      fill = fund_type, 
      label = paste0("$", total_app)
       )
        ) +
  geom_col() + 
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(
    values = c("#fbb4ae", "#b3cde3", "#ccebc5"),
    name = "",
    labels = c("Com Dev Block Grant", "Grants", "Corporate Fund")
    ) +
  labs(title = "CDPH Budget by Fund Type",
       subtitle = "Millions of $",
       caption = "data.cityofchicago.gov") +
  theme_fivethirtyeight()

# Titles & Salaries -------------------------------------------------------

API_list_salaries <- c("7zkb-yr4j", # 2019
                       "txys-725h", # 2020
                       "gcwx-xm5a", # 2021
                       "kwap-s85k") # 2022 (recommended budget)

year = 2019

titles_salaries <- data.frame()

for (i in API_list_salaries){
  x <- paste0("https://data.cityofchicago.org/resource/", i, ".json")
  y <- read.socrata(x)
  y$year = year
  year = year + 1
  titles_salaries <- bind_rows(titles_salaries, y)
}

# we can look at this later, just wana see growth in epis over time
titles_salaries_clean <- titles_salaries %>% 
  mutate(
    total_budgeted_unit = as.numeric(total_budgeted_unit),
    budgeted_pay_rate = as.numeric(budgeted_pay_rate),
    total_budgeted_amount = as.numeric(total_budgeted_amount)
    )

# Covid Population Stats --------------------------------------------------

# chicago covid stats (https://data.cityofchicago.org/Health-Human-Services/COVID-19-Daily-Rolling-Average-Case-Death-and-Hosp/e68t-c7fv)

# read in data from chi portal
chi_covid_stats <- read.socrata("https://data.cityofchicago.org/resource/e68t-c7fv.json")

# convert variables to useable type
covid_totals <- chi_covid_stats %>% 
  mutate(date = as.Date(date)) %>% 
  mutate_if(is.character, as.numeric) %>% 
  select(date, contains("total"))

# visualization
covid_totals %>%
  
  # create trend type variable for color grouping
  pivot_longer(
    cols = contains("total"), 
    names_to = "type",
    values_to = "value"
    ) %>%
  filter(
    str_detect(type, "rate")
    ) %>% 
  ggplot() +
  geom_line(
    aes(date, value, color = type)
    ) +
  labs(
    title = "Covid in Chicago 2020-2021",
    subtitle = "Per 100,000 residents, 7-day moving average",
    caption = "data.cityofchicago.org"
    ) +
  scale_x_date(
    date_labels = "%b", date_breaks = "1 month"
    ) +
  scale_color_fivethirtyeight(
    name = "",
    labels = c("Cases", "Deaths", "Hospitalizations")
    ) +
  theme_fivethirtyeight()

# death rate by race
chi_covid_stats %>% 
  mutate_if(
    is.character, as.numeric
  ) %>% 
  pivot_longer(
    cols = "deaths_rate_latinx":"deaths_rate_other_race_non",
    names_to = "race",
    values_to = "death_rate_by_race"
  ) %>% 
  mutate(
    week_ends = as.Date(floor_date(date, "week"))
  ) %>% 
  group_by(
    week_ends, race
  ) %>% 
  summarize(week_sums = sum(death_rate_by_race)
  ) %>% 
  ggplot() +
  geom_line(
    aes(week_ends, week_sums, color = race)
  ) +
  labs(title = "Covid Deaths by Race in Chicago",
       subtitle = "Per 100,000 residents, 7-day moving average",
       caption = "data.cityofchicago.org"
  ) +
  scale_x_date(
    date_labels = "%b", date_breaks = "1 month"
  ) +
  scale_color_manual(
    values = c("#b2e2e2", "#e7298a", "#d95f02", "#66c2a4", "#2ca25f"),
    name = "",
    labels = c("Asian", "Black", "Latinx", "Other", "White"),
  ) +
  theme_fivethirtyeight()

# death rate by age
chi_covid_stats %>% 
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
  ) %>% 
  ggplot() +
  geom_line(
    aes(week_ends, week_sums, color = age)
  ) +
  labs(
    title = "Covid in Chicago 2020-2021",
    subtitle = "Per 100,000 residents, 7-day moving average",
    caption = "data.cityofchicago.org"
  ) +
  scale_x_date(
    date_labels = "%b", date_breaks = "1 month"
  ) +
  scale_color_fivethirtyeight(
    name = "",
    labels = c("60 and over", "Under 60"),
  ) +
  theme_fivethirtyeight()
