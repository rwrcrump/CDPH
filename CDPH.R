# overview of recent CDPH budgets and covid stats

# Setup -------------------------------------------------------------------

library(tidyverse)
library(RSocrata)
library(scales)
library(ggthemes)

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
  
  # add year column seeding at 2011
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
      year == 2019 ~ as.numeric(X_ordinance_amount_),
      year == 2020 ~ as.numeric(X_ordinance_amount_),
      year == 2021 ~ as.numeric(X_ordinance_amount_),
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


# Covid population stats --------------------------------------------------

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
  scale_color_manual(
    values = c("#1b9e77", "#d95f02", "#7570b3"),
    name = "",
    labels = c("Cases", "Deaths", "Hospitalizations")
    ) +
  theme_fivethirtyeight()

# but we know the pandemic has big asymmetric impacts, we can come back to this