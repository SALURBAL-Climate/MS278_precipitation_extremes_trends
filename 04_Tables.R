##################################################################
# MS278: Precipitation descriptive - city center point
#
# Manuscript tables
##################################################################

library(readr); library(dplyr); library(tidyverse)
library(purrr); library(writexl); library(tidyr)
library(arrow); library(glue)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS278")

#--------------------------------------------------
# Table 1
#--------------------------------------------------
data <- read_parquet("Data/data_prec_final.parquet")

str(data)

# 1. prepare to add total and country as columns

add_total <- function(data, summary_expr, value_name) {
  
  by_country <- data %>%
    summarize(!!value_name := {{ summary_expr }}, .by = Country)
  
  total <- data %>%
    summarize(!!value_name := {{ summary_expr }}) %>%
    mutate(Country = "Total")
  bind_rows(by_country, total) %>%
    pivot_wider(
      names_from = Country,
      values_from = !!sym(value_name))
}

to_char <- function(df) {
  df %>% mutate(across(-variables, as.character))}

# 2. Number of cities
table_1a <- add_total(data, n_distinct(SALID1),"n_cities") %>%
  mutate(variables = "n_cities", .before = 1) %>%
  to_char()

# 3. City population
base_pop <- data %>% 
  summarize(pop = first(total_pop), .by = c(Country, SALID1, YEAR)) %>% 
  summarize(pop = pop/1000, .by = c(Country, SALID1, YEAR)) %>% 
  summarize(pop = mean(pop), .by = c(Country, SALID1))

table_1b <- add_total(base_pop,
                      glue("{round(median(pop))} ({round(quantile(pop,.1))}, {round(quantile(pop,.9))})"),
                      "city_pop") %>%
  mutate(variables = "city_pop", .before = 1) %>%
  to_char()

# 4. Climate zones
table_1d <- data %>%
  distinct(Country, CLZ, SALID1) %>% 
  count(Country, CLZ, name = "n_cities") %>%
  bind_rows(data %>%
              distinct(Country, CLZ, SALID1) %>% 
              count(CLZ, name = "n_cities") %>%
              mutate(Country = "Total")) %>%
  group_by(Country) %>%
  mutate(total = sum(n_cities), pct_clz = 100 * n_cities / total) %>%
  ungroup() %>%
  select(Country, CLZ, pct_clz) %>%
  pivot_wider(names_from = Country,
              values_from = pct_clz,
              values_fill = 0) %>%
  mutate(across(-CLZ, ~ format(round(.x, 1), nsmall = 1))) %>%
  rename(variables = CLZ) %>%
  to_char()

# 5. R95P, Rx1day, and Rx5day
table_1e <- bind_rows(
  add_total(data,
            glue("{round(median(R95P),0)} ({round(quantile(R95P,.1),0)}, {round(quantile(R95P,.9),1)})"),"R95P") %>%
    mutate(variables = "R95P", .before = 1),
  add_total(data,
            glue("{round(median(Rx1day),0)} ({round(quantile(Rx1day,.1),0)}, {round(quantile(Rx1day,.9),1)})"),"Rx1day") %>%
    mutate(variables = "Rx1day", .before = 1),
  add_total(data,
            glue("{round(median(Rx5day),0)} ({round(quantile(Rx5day,.1),0)}, {round(quantile(Rx5day,.9),1)})"),"Rx5day") %>%
    mutate(variables = "Rx5day", .before = 1)) %>% 
  to_char()


# 6. Cities variables
data <- data %>%
  mutate(GDP = GDP / 1000,
         pop_density_guf = pop_density_guf / 1000,
         pop_over65 = pop_over65 * 100)

var <- c("pop_over65",
         "pop_density_guf",
         "GDP",
         "education",
         "NDVI",
         "median_elevation",
         "slope")

result_list <- lapply(var, function(v) {
  
  decimals <- ifelse(v %in% c("NDVI"), 2, 1)
  by_country <- data %>%
    group_by(Country) %>%
    summarise(
      med = median(.data[[v]], na.rm = TRUE),
      p10 = quantile(.data[[v]], 0.1, na.rm = TRUE),
      p90 = quantile(.data[[v]], 0.9, na.rm = TRUE),
      .groups = "drop")
  
  total <- data %>%
    summarise(
      Country = "Total",
      med = median(.data[[v]], na.rm = TRUE),
      p10 = quantile(.data[[v]], 0.1, na.rm = TRUE),
      p90 = quantile(.data[[v]], 0.9, na.rm = TRUE))
  
  bind_rows(by_country, total) %>%
    mutate(
      formatted = glue("{format(round(med, decimals), nsmall = decimals)} ",
                       "({format(round(p10, decimals), nsmall = decimals)}, ",
                       "{format(round(p90, decimals), nsmall = decimals)})")) %>%
    select(Country, formatted) %>%
    pivot_wider(names_from = Country, values_from = formatted) %>%
    mutate(variables = v, .before = 1)
})

table_1g <- bind_rows(result_list)

# 7. coastal n cities
table_1h <- data %>%
  distinct(Country, SALID1, coastal) %>%
  count(Country, coastal, name = "n_cities") %>%
  bind_rows(data %>%
      distinct(Country, SALID1, coastal) %>%
      count(coastal, name = "n_cities") %>%
      mutate(Country = "Total")) %>%
  filter(coastal == 1) %>%
  select(Country, n_cities) %>%
  pivot_wider(names_from = Country,
              values_from = n_cities,
              values_fill = 0) %>%
  mutate(variables = "coastal", .before = 1) %>%
  to_char()

# 8. Join tables
table1 <- bind_rows(table_1a, table_1e, table_1b, table_1g, table_1h, table_1d)

# 9. Prepare to final table 1
var_labels <- c(
  n_cities        = "Number of cities",
  R95P         = "Annual R95p*",
  Rx1day         = "Annual RX1day*",
  Rx5day         = "Annual RX5day*",
  city_pop        = "City population (thousands)",
  pop_density_guf = "Population density (1000 people per km2)",
  GDP             = "GDP per capita (US$ thousands)",
  pop_over65      = "Population ≥65 years (%)",
  education       = "Completed primary education (%)",
  NDVI            = "Greenness (NDVI)",
  median_elevation = "Altitude (m)",
  slope = "Slope (°)",
  coastal = "Coastal cities (n of cities)")

table1_names <- table1 %>%
  mutate(variables = dplyr::recode(variables, !!!as.list(var_labels)))

# add lines
table1_names <- table1_names %>%
  add_row(variables = "City social and environmental characteristics*",
          .before = which(table1_names$variables == "City population (thousands)"))

table1_names <- table1_names %>%
  add_row(variables = "Climate zones (%n of cities)",
          .before = which(table1_names$variables %in% unique(table_1d$variables))[1])
# note
col_1 <- names(table1_names)[1]

table1_names <- table1_names %>%
  add_row(!!col_1 := "* Values represent the median (10th, 90th percentiles) across cities within each country")

# organize colunms and rename
table1_names <- table1_names %>%
  select("variables", "Total", "Argentina", "Brazil", "Central America", 
         "Chile", "Colombia", "Mexico", "Peru") %>% 
  rename("  " = variables)

# 9. export as excel
write_xlsx(table1_names, "Tables/Table_1.xlsx")

#--------------------------------------------------
# Table 2 - univariable
#--------------------------------------------------
# 1. No categorical variables
univariate_R95P <- read_csv("Model_results/L1AD/R95P/R95P_univariate.csv")
univariate_Rx1day <- read_csv("Model_results/L1AD/Rx1day/Rx1day_univariate.csv")
univariate_Rx5day <- read_csv("Model_results/L1AD/Rx5day/Rx5day_univariate.csv")

univariate_R95P <- univariate_R95P %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         R95P = sprintf("%.1f (%.1f, %.1f)%s",
                        estimate, min95, max95, sig)) %>%
  select(variable, R95P)

univariate_Rx1day <- univariate_Rx1day %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         Rx1day = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  select(variable, Rx1day)

univariate_Rx5day <- univariate_Rx5day %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         Rx5day = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  select(variable, Rx5day)


univariate_join_v <- list(univariate_R95P,
                          univariate_Rx1day,
                          univariate_Rx5day) %>%
  reduce(full_join, by = "variable") %>% 
  mutate(variable = case_when(
    variable == "total_pop_z" ~ "Population size",
    variable == "pop_density_guf_z" ~ "Population density",
    variable == "GDP_z" ~ "GDP per capita",
    variable == "pop_over65_z" ~ "Population ≥65 years (%)",
    variable == "slope_z" ~ "Slope (º)", 
    variable == "education_z" ~ "Completed primary education (%)",
    variable == "elevation_z" ~ "Altitude (m)",
    variable == "NDVI_z" ~ "NDVI",
    variable == "coastal" ~ "Coastal cities",
    TRUE ~ variable)) 
univariate_join_v

# 2. Categorical variables - Climate zones
clz_R95P <- read_csv("Model_results/L1AD/R95P/R95P_CLZ.csv")
clz_Rx1day <- read_csv("Model_results/L1AD/Rx1day/Rx1day_CLZ.csv")
clz_Rx5day <- read_csv("Model_results/L1AD/Rx5day/Rx5day_CLZ.csv")

clz_R95P <- clz_R95P %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         R95P = sprintf("%.1f (%.1f, %.1f)%s",
                        estimate, min95, max95, sig)) %>%
  select(parm, R95P) %>% 
  rename(variable = parm) %>% 
  filter(!variable %in% c("YEAR_dec", "_cons"))

clz_Rx1day <- clz_Rx1day %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         Rx1day = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  select(parm, Rx1day) %>% 
  rename(variable = parm) %>% 
  filter(!variable %in% c("YEAR_dec", "_cons"))

clz_Rx5day <- clz_Rx5day %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         Rx5day = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  select(parm, Rx5day) %>% 
  rename(variable = parm) %>% 
  filter(!variable %in% c("YEAR_dec", "_cons"))

str(clz_R95P)

# join
univariate_join_clz <- list(clz_R95P,
                            clz_Rx1day,
                            clz_Rx5day) %>%
  reduce(full_join, by = "variable") %>% 
  mutate(variable = case_when(
    variable == "1.CLZ_num" ~ "Arid",
    variable == "2.CLZ_num" ~ "Polar",
    variable == "3.CLZ_num" ~ "Temperate",
    variable == "4b.CLZ_num" ~ "Tropical", 
    TRUE ~ variable)) %>% 
  mutate(across(-variable, ~ ifelse(variable == "Tropical", "Reference", .))) %>% 
  mutate(variable = factor(variable,
                           levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(variable)

# 3. Final join - table 2
table2 <- rbind(univariate_join_v, univariate_join_clz)

str(table2)

# arrange
table2 <- table2 %>%
  mutate(variable = factor(variable,
                           levels = c("Population size",
                                      "Population density",
                                      "Population ≥65 years (%)",
                                      "Completed primary education (%)",
                                      "GDP per capita",
                                      "NDVI",
                                      "Altitude (m)",
                                      "Slope (º)",
                                      "Coastal cities",
                                      "Tropical",
                                      "Arid",
                                      "Polar",
                                      "Temperate"))) %>%
  arrange(variable) %>% 
  mutate(variable = as.character(variable))

# line for climate zone
climate_row <- table2[1, ] %>% 
  mutate(across(everything(), ~ "")) %>%
  mutate(variable = "Climate Zone")
pos <- which(table2$variable == "Tropical")[1]
table2 <- bind_rows(table2[1:(pos - 1), ],
                    climate_row, table2[pos:nrow(table2), ])

# note an export
note_row <- tibble(variable = "Note:* statistically significant (p < 0.05)")

table2_final <- bind_rows(table2, note_row) %>% 
  rename("  " = "variable")

write_xlsx(table2_final, "Tables/Table_2.xlsx")

#--------------------------------------------------
# Table 3 - climate zones baseline and interaction
#--------------------------------------------------
# 1. baseline and interaction - no control
clz_R95P_int <- read_csv("Model_results/L1AD/R95P/R95P_CLZ_interaction.csv")
clz_Rx1day_int <- read_csv("Model_results/L1AD/Rx1day/Rx1day_CLZ_interaction.csv")
clz_Rx5day_int <- read_csv("Model_results/L1AD/Rx5day/Rx5day_CLZ_interaction.csv")

clz_R95P_int <- clz_R95P_int %>%
  filter(parm != "4b.CLZ_num#co.YEAR_dec") %>%
  mutate(parm = ifelse(parm == "YEAR_dec",
                       "4b.CLZ_num#co.YEAR_dec", parm)) %>%
  mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", ""),
         R95P = sprintf("%.1f (%.1f, %.1f)%s",
                        estimate, min95, max95, sig)) %>%
  mutate(clz = case_when(grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
                         grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
                         grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
                         grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num"),
         term = case_when(grepl("#.*YEAR_dec", parm) ~ "Mean changes over time (95% CI) - Unadjusted model",
                          TRUE ~ "Mean differences at baseline (95% CI) - Unadjusted model")) %>%
  filter(!parm %in% c("_cons")) %>%
  select(clz, term, R95P) %>%
  distinct(clz, term, .keep_all = TRUE) %>%
  mutate(R95P = ifelse(clz == "4b.CLZ_num" &
                         term == "Mean differences at baseline (95% CI) - Unadjusted model", 
                       "Reference", R95P)) %>%
  pivot_wider(names_from  = term, values_from = R95P) %>%
  mutate(clz = recode(clz, 
                      "1.CLZ_num"  = "Arid",
                      "2.CLZ_num"  = "Polar",
                      "3.CLZ_num"  = "Temperate",
                      "4b.CLZ_num" = "Tropical")) %>%
  mutate(clz = factor(clz, levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(clz) %>%
  mutate(clz = as.character(clz))

clz_Rx1day_int <- clz_Rx1day_int %>%
  filter(parm != "4b.CLZ_num#co.YEAR_dec") %>%
  mutate(parm = ifelse(parm == "YEAR_dec",
                       "4b.CLZ_num#co.YEAR_dec", parm)) %>%
  mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", ""),
         Rx1day = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  mutate(clz = case_when(grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
                         grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
                         grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
                         grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num"),
         term = case_when(grepl("#.*YEAR_dec", parm) ~ "Mean changes over time (95% CI) - Unadjusted model",
                          TRUE ~ "Mean differences at baseline (95% CI) - Unadjusted model")) %>%
  filter(!parm %in% c("_cons")) %>%
  select(clz, term, Rx1day) %>%
  distinct(clz, term, .keep_all = TRUE) %>%
  mutate(Rx1day = ifelse(clz == "4b.CLZ_num" &
                           term == "Mean differences at baseline (95% CI) - Unadjusted model", 
                         "Reference", Rx1day)) %>%
  pivot_wider(names_from  = term, values_from = Rx1day) %>%
  mutate(clz = recode(clz, 
                      "1.CLZ_num"  = "Arid",
                      "2.CLZ_num"  = "Polar",
                      "3.CLZ_num"  = "Temperate",
                      "4b.CLZ_num" = "Tropical")) %>%
  mutate(clz = factor(clz, levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(clz) %>%
  mutate(clz = as.character(clz))

clz_Rx5day_int <- clz_Rx5day_int %>%
  filter(parm != "4b.CLZ_num#co.YEAR_dec") %>%
  mutate(parm = ifelse(parm == "YEAR_dec",
                       "4b.CLZ_num#co.YEAR_dec", parm)) %>%
  mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", ""),
         Rx5day = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  mutate(clz = case_when(grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
                         grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
                         grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
                         grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num"),
         term = case_when(grepl("#.*YEAR_dec", parm) ~ "Mean changes over time (95% CI) - Unadjusted model",
                          TRUE ~ "Mean differences at baseline (95% CI) - Unadjusted model")) %>%
  filter(!parm %in% c("_cons")) %>%
  select(clz, term, Rx5day) %>%
  distinct(clz, term, .keep_all = TRUE) %>%
  mutate(Rx5day = ifelse(clz == "4b.CLZ_num" &
                           term == "Mean differences at baseline (95% CI) - Unadjusted model", 
                         "Reference", Rx5day)) %>%
  pivot_wider(names_from  = term, values_from = Rx5day) %>%
  mutate(clz = recode(clz, 
                      "1.CLZ_num"  = "Arid",
                      "2.CLZ_num"  = "Polar",
                      "3.CLZ_num"  = "Temperate",
                      "4b.CLZ_num" = "Tropical")) %>%
  mutate(clz = factor(clz, levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(clz) %>%
  mutate(clz = as.character(clz))

# 2. baseline and interaction - with control
clz_R95P_cont <- read_csv("Model_results/L1AD/R95P/R95P_CLZ_interaction_controlled.csv")
clz_Rx1day_cont <- read_csv("Model_results/L1AD/Rx1day/Rx1day_CLZ_interaction_controlled.csv")
clz_Rx5day_cont <- read_csv("Model_results/L1AD/Rx5day/Rx5day_CLZ_interaction_controlled.csv")

clz_R95P_cont <- clz_R95P_cont %>%
  filter(parm != "4b.CLZ_num#co.YEAR_dec") %>%
  mutate(parm = ifelse(parm == "YEAR_dec",
                       "4b.CLZ_num#co.YEAR_dec", parm)) %>%
  mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", ""),
         R95P = sprintf("%.1f (%.1f, %.1f)%s",
                        estimate, min95, max95, sig)) %>%
  mutate(clz = case_when(grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
                         grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
                         grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
                         grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num"),
         term = case_when(grepl("#.*YEAR_dec", parm) ~ "Mean changes over time (95% CI) - Adjusted model",
                          TRUE ~ "Mean differences at baseline (95% CI) - Adjusted model")) %>%
  filter(!parm %in% c("_cons")) %>%
  select(clz, term, R95P) %>%
  distinct(clz, term, .keep_all = TRUE) %>%
  mutate(R95P = ifelse(clz == "4b.CLZ_num" &
                         term == "Mean differences at baseline (95% CI) - Adjusted model", 
                       "Reference", R95P)) %>%
  pivot_wider(names_from  = term, values_from = R95P) %>%
  mutate(clz = recode(clz, 
                      "1.CLZ_num"  = "Arid",
                      "2.CLZ_num"  = "Polar",
                      "3.CLZ_num"  = "Temperate",
                      "4b.CLZ_num" = "Tropical")) %>%
  mutate(clz = factor(clz, levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(clz) %>%
  mutate(clz = as.character(clz))

clz_Rx1day_cont <- clz_Rx1day_cont %>%
  filter(parm != "4b.CLZ_num#co.YEAR_dec") %>%
  mutate(parm = ifelse(parm == "YEAR_dec",
                       "4b.CLZ_num#co.YEAR_dec", parm)) %>%
  mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", ""),
         Rx1day = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  mutate(clz = case_when(grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
                         grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
                         grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
                         grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num"),
         term = case_when(grepl("#.*YEAR_dec", parm) ~ "Mean changes over time (95% CI) - Adjusted model",
                          TRUE ~ "Mean differences at baseline (95% CI) - Adjusted model")) %>%
  filter(!parm %in% c("_cons")) %>%
  select(clz, term, Rx1day) %>%
  distinct(clz, term, .keep_all = TRUE) %>%
  mutate(Rx1day = ifelse(clz == "4b.CLZ_num" &
                           term == "Mean differences at baseline (95% CI) - Adjusted model", 
                         "Reference", Rx1day)) %>%
  pivot_wider(names_from  = term, values_from = Rx1day) %>%
  mutate(clz = recode(clz, 
                      "1.CLZ_num"  = "Arid",
                      "2.CLZ_num"  = "Polar",
                      "3.CLZ_num"  = "Temperate",
                      "4b.CLZ_num" = "Tropical")) %>%
  mutate(clz = factor(clz, levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(clz) %>%
  mutate(clz = as.character(clz))

clz_Rx5day_cont <- clz_Rx5day_cont %>%
  filter(parm != "4b.CLZ_num#co.YEAR_dec") %>%
  mutate(parm = ifelse(parm == "YEAR_dec",
                       "4b.CLZ_num#co.YEAR_dec", parm)) %>%
  mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", ""),
         Rx5day = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  mutate(clz = case_when(grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
                         grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
                         grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
                         grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num"),
         term = case_when(grepl("#.*YEAR_dec", parm) ~ "Mean changes over time (95% CI) - Adjusted model",
                          TRUE ~ "Mean differences at baseline (95% CI) - Adjusted model")) %>%
  filter(!parm %in% c("_cons")) %>%
  select(clz, term, Rx5day) %>%
  distinct(clz, term, .keep_all = TRUE) %>%
  mutate(Rx5day = ifelse(clz == "4b.CLZ_num" &
                           term == "Mean differences at baseline (95% CI) - Adjusted model", 
                         "Reference", Rx5day)) %>%
  pivot_wider(names_from  = term, values_from = Rx5day) %>%
  mutate(clz = recode(clz, 
                      "1.CLZ_num"  = "Arid",
                      "2.CLZ_num"  = "Polar",
                      "3.CLZ_num"  = "Temperate",
                      "4b.CLZ_num" = "Tropical")) %>%
  mutate(clz = factor(clz, levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(clz) %>%
  mutate(clz = as.character(clz))

# 3. join tables
table_R95P <- clz_R95P_int %>%
  left_join(clz_R95P_cont, by = "clz")
table_R95P$prec <- "R95p"

table_Rx1day <- clz_Rx1day_int %>%
  left_join(clz_Rx1day_cont, by = "clz")
table_Rx1day$prec <- "RX1day"

table_Rx5day <- clz_Rx5day_int %>%
  left_join(clz_Rx5day_cont, by = "clz")
table_Rx5day$prec <- "RX5day"

table_3 <- rbind(table_R95P, table_Rx1day, table_Rx5day) %>%
  select(prec,
         clz,
         "Mean differences at baseline (95% CI) - Unadjusted model",
         "Mean changes over time (95% CI) - Unadjusted model",
         "Mean differences at baseline (95% CI) - Adjusted model",
         "Mean changes over time (95% CI) - Adjusted model") %>%
  group_by(prec) %>%
  mutate(prec = ifelse(row_number() == 1, prec, "")) %>%
  ungroup()

note_row_sup <- tibble(prec = "Note:* statistically significant (p < 0.05)")

table_3_final <- bind_rows(table_3, note_row_sup) %>%
  rename("   " = prec,"  " = clz)

write_xlsx(table_3_final, "Tables/Table_3.xlsx")

# 3. join tables
# table3 <- clz_R95P_int %>% 
#   left_join(clz_R95P_cont, by = "clz") 
# 
# note_row3 <- tibble(clz = "Note:* statistically significant (p < 0.05)")
# table3_final <- bind_rows(table3, note_row3) %>% 
#   rename("  " = clz)
# 
# write_xlsx(table3_final, "Tables/Table_3.xlsx")

# Sup table
# sup_table_Rx1day <- clz_Rx1day_int %>% 
#   left_join(clz_Rx1day_cont, by = "clz")
# sup_table_Rx1day$prec <- "Rx1day"
# 
# sup_table_Rx5day <- clz_Rx5day_int %>% 
#   left_join(clz_Rx5day_cont, by = "clz")
# sup_table_Rx5day$prec <- "Rx5day"
# 
# sup_table <- rbind(sup_table_Rx1day, sup_table_Rx5day) %>% 
#   select(prec,
#          clz,
#          "Mean differences at baseline (95% CI) - Unadjusted model",
#          "Mean changes over time (95% CI) - Unadjusted model",
#          "Mean differences at baseline (95% CI) - Adjusted model",
#          "Mean changes over time (95% CI) - Adjusted model") %>% 
#   group_by(prec) %>%
#   mutate(prec = ifelse(row_number() == 1, prec, "")) %>%
#   ungroup()

# note_row_sup <- tibble(prec = "Note:* statistically significant (p < 0.05)")
# 
# sup_table_final <- bind_rows(sup_table, note_row_sup) %>% 
#   rename("   " = prec,"  " = clz)
# 
# write_xlsx(sup_table_final, "Tables/Sup_table.xlsx")

#--------------------------------------------------
# Table 4 - hybrid
#--------------------------------------------------
# 1. Hybrids - no control
hybrid_R95P <- read_csv("Model_results/L1AD/R95P/R95P_hybrid_models_with_slope.csv")
hybrid_Rx1day <- read_csv("Model_results/L1AD/Rx1day/Rx1day_hybrid_models_with_slope.csv")
hybrid_Rx5day <- read_csv("Model_results/L1AD/Rx5day/Rx5day_hybrid_models_with_slope.csv")

R95P_btw <- hybrid_R95P %>% 
  filter(variable_type == "between") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Difference in mean (95% CI) - Unadjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                 estimate, min95, max95, sig)) %>%
  select(variable, "Difference in mean (95% CI) - Unadjusted model")

R95P_wht <- hybrid_R95P %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - Unadjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                               estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - Unadjusted model")

Rx1day_btw <- hybrid_Rx1day %>% 
  filter(variable_type == "between") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Difference in mean (95% CI) - Unadjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                 estimate, min95, max95, sig)) %>%
  select(variable, "Difference in mean (95% CI) - Unadjusted model")

Rx1day_wht <- hybrid_Rx1day %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - Unadjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                               estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - Unadjusted model")

Rx5day_btw <- hybrid_Rx5day %>% 
  filter(variable_type == "between") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Difference in mean (95% CI) - Unadjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                 estimate, min95, max95, sig)) %>%
  select(variable, "Difference in mean (95% CI) - Unadjusted model")

Rx5day_wht <- hybrid_Rx5day %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - Unadjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                               estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - Unadjusted model")

R95P_h <- list(R95P_btw, R95P_wht) %>%
  reduce(left_join, by = "variable") %>% 
  mutate(variable = case_when(
    variable == "total_pop" ~ "Population size",
    variable == "pop_density_guf" ~ "Population density",
    variable == "GDP" ~ "GDP per capita",
    variable == "pop_over65" ~ "Population ≥65 years (%)",
    variable == "education" ~ "Completed primary education (%)",
    variable == "NDVI" ~ "NDVI",
    TRUE ~ variable)) %>% 
  mutate(variable = factor(variable,
                           levels = c("Population size",
                                      "Population density",
                                      "Population ≥65 years (%)",
                                      "Completed primary education (%)",
                                      "GDP per capita",
                                      "NDVI"))) %>%
  arrange(variable) %>% 
  mutate(variable = as.character(variable))

# 2. models adjusted
hybrid_R95P_c <- read_csv("Model_results/L1AD/R95P/R95P_hybrid_models_with_slope_controlled.csv")
hybrid_Rx1day_c <- read_csv("Model_results/L1AD/Rx1day/Rx1day_hybrid_models_with_slope_controlled.csv")
hybrid_Rx5day_c <- read_csv("Model_results/L1AD/Rx5day/Rx5day_hybrid_models_with_slope_controlled.csv")

R95P_btw_c <- hybrid_R95P_c %>% 
  filter(variable_type == "between") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Difference in mean (95% CI) - Adjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                            estimate, min95, max95, sig)) %>%
  select(variable, "Difference in mean (95% CI) - Adjusted model")

R95P_wht_c <- hybrid_R95P_c %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - Adjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                          estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - Adjusted model")

Rx1day_btw_c <- hybrid_Rx1day_c %>% 
  filter(variable_type == "between") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Difference in mean (95% CI) - Adjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                            estimate, min95, max95, sig)) %>%
  select(variable, "Difference in mean (95% CI) - Adjusted model")

Rx1day_wht_c <- hybrid_Rx1day_c %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - Adjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                          estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - Adjusted model")

Rx5day_btw_c <- hybrid_Rx5day_c %>% 
  filter(variable_type == "between") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Difference in mean (95% CI) - Adjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                            estimate, min95, max95, sig)) %>%
  select(variable, "Difference in mean (95% CI) - Adjusted model")

Rx5day_wht_c <- hybrid_Rx5day_c %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - Adjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                          estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - Adjusted model")

R95P_h_c <- list(R95P_btw_c, R95P_wht_c) %>%
  reduce(left_join, by = "variable") %>% 
  mutate(variable = case_when(
    variable == "total_pop" ~ "Population size",
    variable == "pop_density_guf" ~ "Population density",
    variable == "GDP" ~ "GDP per capita",
    variable == "pop_over65" ~ "Population ≥65 years (%)",
    variable == "education" ~ "Completed primary education (%)",
    variable == "NDVI" ~ "NDVI",
    TRUE ~ variable)) %>% 
  mutate(variable = factor(variable,
                           levels = c("Population size",
                                      "Population density",
                                      "Population ≥65 years (%)",
                                      "Completed primary education (%)",
                                      "GDP per capita",
                                      "NDVI"))) %>%
  arrange(variable) %>% 
  mutate(variable = as.character(variable))

R95P_h_final <- R95P_h %>% left_join(R95P_h_c, by = "variable")
R95P_h_final$prec <- "R95p"

# # Table 4 
# table4 <- R95P_h %>% left_join(R95P_h_c, by = "variable")
# 
# note_row4 <- tibble(variable = "Note:* statistically significant (p < 0.05)")
# table4_final <- bind_rows(table4, note_row4) %>% 
#   rename("  " = variable)
#
#write_xlsx(table4_final, "Tables/Table_4.xlsx")

# sup_table 2
Rx1day_sup_h <- list(Rx1day_btw, Rx1day_wht,
                     Rx1day_btw_c, Rx1day_wht_c) %>%
  reduce(left_join, by = "variable") %>% 
  mutate(variable = case_when(
    variable == "total_pop" ~ "Population size",
    variable == "pop_density_guf" ~ "Population density",
    variable == "GDP" ~ "GDP per capita",
    variable == "pop_over65" ~ "Population ≥65 years (%)",
    variable == "education" ~ "Completed primary education (%)",
    variable == "NDVI" ~ "NDVI",
    TRUE ~ variable)) %>% 
  mutate(variable = factor(variable,
                           levels = c("Population size",
                                      "Population density",
                                      "Population ≥65 years (%)",
                                      "Completed primary education (%)",
                                      "GDP per capita",
                                      "NDVI"))) %>%
  arrange(variable) %>% 
  mutate(variable = as.character(variable))
Rx1day_sup_h$prec <- "RX1day"

Rx5day_sup_h <- list(Rx5day_btw, Rx5day_wht,
                     Rx5day_btw_c, Rx5day_wht_c) %>%
  reduce(left_join, by = "variable") %>% 
  mutate(variable = case_when(
    variable == "total_pop" ~ "Population size",
    variable == "pop_density_guf" ~ "Population density",
    variable == "GDP" ~ "GDP per capita",
    variable == "pop_over65" ~ "Population ≥65 years (%)",
    variable == "education" ~ "Completed primary education (%)",
    variable == "NDVI" ~ "NDVI",
    TRUE ~ variable)) %>% 
  mutate(variable = factor(variable,
                           levels = c("Population size",
                                      "Population density",
                                      "Population ≥65 years (%)",
                                      "Completed primary education (%)",
                                      "GDP per capita",
                                      "NDVI"))) %>%
  arrange(variable) %>% 
  mutate(variable = as.character(variable))
Rx5day_sup_h$prec <- "RX5day"


# join for table 4
table_4_all <- rbind(R95P_h_final, Rx1day_sup_h, Rx5day_sup_h) %>% 
  select(prec,
         variable,
         "Difference in mean (95% CI) - Unadjusted model",
         "Difference in mean (95% CI) - Adjusted model",
         "Within-city difference over time (95% CI) - Unadjusted model",
         "Within-city difference over time (95% CI) - Adjusted model") %>%
  group_by(prec) %>%
  mutate(prec = ifelse(row_number() == 1, prec, "")) %>%
  ungroup()

note_row4 <- tibble(prec = "Note:* statistically significant (p < 0.05)")
sup_table4 <- bind_rows(sup_table4_all, note_row4) %>% 
  rename("  " = prec)

note_row4 <- tibble(prec = "Note:* statistically significant (p < 0.05)")
table_4 <- bind_rows(table_4_all, note_row4) %>% 
  rename("  " = prec)
 
write_xlsx(table_4, "Tables/Table_4.xlsx")

