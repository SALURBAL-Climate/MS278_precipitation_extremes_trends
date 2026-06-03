##################################################################
# MS278: Descriptive precitation extremes
#
# This script calculate the population exposed to prec extremes
# Population exposed - person-days and person-years
#
##################################################################

library(tidyverse); library(dplyr); library(arrow)
library(tidyr); library(writexl)

rm(list= ls())

data <- read_parquet("Data/data_prec_final.parquet")

#-----------------------------------------------------------
# Annual population exposed to extreme precipitation
#-----------------------------------------------------------
# Calculation of pop exposed

# 1. Population exposed - pop 2000 data as constant and pop 2000-2024
data <- data %>% 
  select(Country, CLZ, SALID1, YEAR, total_pop, pop_2000, n_extreme_days) %>% 
  mutate(pop_exp_2000 = n_extreme_days * pop_2000,
           pop_exp_2000_24 = n_extreme_days * total_pop)


#---------------------------------------------------------------
# Calculate annual pop and exposed pop - country - clz - total
#---------------------------------------------------------------
# 1. By year and country
pop_annual_country <- data %>% 
  group_by(Country, YEAR) %>%
  summarise(t_pop_2000    = sum(pop_2000, na.rm = TRUE),
            t_pop_2000_24 = sum(total_pop, na.rm = TRUE),
            pop_exposed_2000    = sum(pop_exp_2000, na.rm = TRUE),
            pop_exposed_2000_24 = sum(pop_exp_2000_24, na.rm = TRUE),.groups = "drop") %>%
  mutate(py_exposure_2000    = round(pop_exposed_2000 / 365.25, 0),
         py_exposure_2000_24 = round(pop_exposed_2000_24 / 365.25, 0))

# 2. pct of the total py by country
country_exposure <- pop_annual_country %>%
  group_by(Country) %>%
  summarise(
    py_exposed = sum(py_exposure_2000_24, na.rm = TRUE),
    py_total = sum(t_pop_2000_24, na.rm = TRUE),
    pct_exposed = round(100 * py_exposed / py_total, 1),.groups = "drop") %>%
  arrange(desc(pct_exposed))
print(country_exposure)

# 3. By year city
pop_annual_city <- data %>% 
  group_by(SALID1, YEAR) %>%
  summarise(t_pop_2000    = sum(pop_2000, na.rm = TRUE),
            t_pop_2000_24 = sum(total_pop, na.rm = TRUE),
            pop_exposed_2000    = sum(pop_exp_2000, na.rm = TRUE),
            pop_exposed_2000_24 = sum(pop_exp_2000_24, na.rm = TRUE),.groups = "drop") %>%
  mutate(py_exposure_2000    = round(pop_exposed_2000 / 365.25, 0),
         py_exposure_2000_24 = round(pop_exposed_2000_24 / 365.25, 0))

# 4. total by year
pop_total_year <- data %>%
  group_by(YEAR) %>%
  summarise(t_pop_2000    = sum(pop_2000, na.rm = TRUE),
            t_pop_2000_24 = sum(total_pop, na.rm = TRUE),
            pop_exposed_2000    = sum(pop_exp_2000, na.rm = TRUE),
            pop_exposed_2000_24 = sum(pop_exp_2000_24, na.rm = TRUE),.groups = "drop") %>%
  mutate(py_exposure_2000    = round(pop_exposed_2000 / 365.25, 0),
         py_exposure_2000_24 = round(pop_exposed_2000_24 / 365.25, 0))

max(pop_total_year$py_exposure_2000_24)

# Export
write_xlsx(pop_annual_country, "pop_annual_country.xlsx")

#---------------------------------------------------------------
# Calculate the overall % and by country from the total
#---------------------------------------------------------------
total_country <- pop_annual_country %>%
  summarise(pop_exposed_2000_total    = sum(pop_exposed_2000, na.rm = TRUE),
            pop_exposed_2000_24_total = sum(pop_exposed_2000_24, na.rm = TRUE),
            py_exposure_2000_total    = sum(py_exposure_2000, na.rm = TRUE),
            py_exposure_2000_24_total = sum(py_exposure_2000_24, na.rm = TRUE),.by = Country)

sum(total_country$py_exposure_2000_24_total)

total_country_table <- total_country %>%
  pivot_longer(cols = -Country,
               names_to = "metric",
               values_to = "value") %>%
  pivot_wider(names_from = Country,
              values_from = value) %>%
  mutate(Total = rowSums(across(-metric), na.rm = TRUE))

table_country_perc <- total_country_table %>%
  mutate(across(-c(metric, Total), ~ (.x / Total) * 100)) %>%
  mutate(across(-c(metric, Total), ~ round(.x, 1)), Total = 100)


# by country and year
total_country_year <- pop_annual_country %>%
  summarise(pop_exposed_2000    = sum(pop_exposed_2000, na.rm = TRUE),
            pop_exposed_2000_24 = sum(pop_exposed_2000_24, na.rm = TRUE),
            py_exposure_2000    = sum(py_exposure_2000, na.rm = TRUE),
            py_exposure_2000_24 = sum(py_exposure_2000_24, na.rm = TRUE),.by = c(Country, YEAR))

table_country_year <- total_country_year %>%
  pivot_longer(cols = -c(Country, YEAR),
               names_to = "metric",
               values_to = "value") %>%
  pivot_wider(names_from = Country,
              values_from = value) %>% 
  mutate(Total = rowSums(across(-c(metric, YEAR)), na.rm = TRUE))

table_country_year_perc <- table_country_year %>%
  mutate(across(-c(metric, YEAR, Total), ~ (.x / Total) * 100)) %>%
  mutate(across(-c(metric, YEAR, Total), ~ round(.x, 1)), Total = 100)
