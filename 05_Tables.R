##################################################################
# MS278: Precipitation descriptive
#
# Manuscript tables
##################################################################

library(readr); library(dplyr); library(tidyverse)
library(purrr); library(writexl); library(tidyr)
library(arrow); library(glue)

dir.create("Tables", recursive = TRUE, showWarnings = FALSE)

#--------------------------------------------------
# Table 1
#--------------------------------------------------
data <- read_parquet("Data/data_prec_final_wht_polar.parquet")

str(data)

# 2. Prepare variables
data <- data %>%
  mutate(
    GDP = GDP / 1000,
    pop_density_guf = pop_density_guf / 1000,
    pop_over65 = pop_over65 * 100)

# 3. Function to add total
add_total <- function(data, summary_expr, value_name) {
  
  by_country <- data %>%
    summarize(
      !!value_name := {{ summary_expr }},.by = Country)
  
  total <- data %>%
    summarize(
      !!value_name := {{ summary_expr }}) %>%
    mutate(Country = "Total")
  
  bind_rows(by_country, total) %>%
    pivot_wider(
      names_from = Country,
      values_from = !!sym(value_name))
}

# 4. Convert columns to character
to_char <- function(df) {
  df %>% mutate(across(everything(), as.character))
}

# 5. Number of cities
table_1a <- add_total(
  data,
  n_distinct(SALID1),
  "n_cities") %>%
  mutate(variables = "n_cities", .before = 1) %>%
  to_char()

# 6. Create city-level averages
city_level <- data %>%
  group_by(Country, SALID1) %>%
  summarise(
    R95P = mean(R95P, na.rm = TRUE),
    Rx1day = mean(Rx1day, na.rm = TRUE),
    Rx5day = mean(Rx5day, na.rm = TRUE),
    total_pop = mean(total_pop, na.rm = TRUE) / 1000,
    pop_over65 = mean(pop_over65, na.rm = TRUE),
    pop_density_guf = mean(pop_density_guf, na.rm = TRUE),
    GDP = mean(GDP, na.rm = TRUE),
    education = mean(education, na.rm = TRUE),
    NDVI = mean(NDVI, na.rm = TRUE),
    median_elevation = mean(median_elevation),
    slope = mean(slope),
    coastal = first(coastal),
    CLZ = first(CLZ),.groups = "drop")

# 7. Annual precipitation indices
table_1e <- bind_rows(
  add_total(
    city_level,
    glue("{round(median(R95P),0)} ({round(quantile(R95P,.1),0)}, {round(quantile(R95P,.9),1)})"),
    "R95P") %>%
    mutate(variables = "R95P", .before = 1),
  add_total(
    city_level,
    glue("{round(median(Rx1day),0)} ({round(quantile(Rx1day,.1),0)}, {round(quantile(Rx1day,.9),1)})"),
    "Rx1day") %>%
    mutate(variables = "Rx1day", .before = 1),
  add_total(
    city_level,
    glue("{round(median(Rx5day),0)} ({round(quantile(Rx5day,.1),0)}, {round(quantile(Rx5day,.9),1)})"),
    "Rx5day") %>%
    mutate(variables = "Rx5day", .before = 1)) %>%
  to_char()

# 8. City population
table_1b <- add_total(
  city_level,
  glue("{round(median(total_pop))} ({round(quantile(total_pop,.1))}, {round(quantile(total_pop,.9))})"),
  "city_pop") %>%
  mutate(variables = "city_pop", .before = 1) %>%
  to_char()

# 9. City-level variables
var <- c(
  "pop_over65",
  "pop_density_guf",
  "GDP",
  "education",
  "NDVI",
  "median_elevation",
  "slope")

result_list <- lapply(var, function(v) {
  
  decimals <- ifelse(v %in% c("NDVI"), 2, 1)
  
  by_country <- city_level %>%
    group_by(Country) %>%
    summarise(
      med = median(.data[[v]], na.rm = TRUE),
      p10 = quantile(.data[[v]], 0.1, na.rm = TRUE),
      p90 = quantile(.data[[v]], 0.9, na.rm = TRUE),.groups = "drop")
  
  total <- city_level %>%
    summarise(
      Country = "Total",
      med = median(.data[[v]], na.rm = TRUE),
      p10 = quantile(.data[[v]], 0.1, na.rm = TRUE),
      p90 = quantile(.data[[v]], 0.9, na.rm = TRUE))
  
  bind_rows(by_country, total) %>%
    mutate(
      formatted = glue(
        "{format(round(med, decimals), nsmall = decimals)} ",
        "({format(round(p10, decimals), nsmall = decimals)}, ",
        "{format(round(p90, decimals), nsmall = decimals)})")) %>%
    select(Country, formatted) %>%
    pivot_wider(
      names_from = Country,
      values_from = formatted) %>%
    mutate(variables = v, .before = 1)
})

table_1g <- bind_rows(result_list)

# 10. Coastal cities
table_1h <- city_level %>%
  count(Country, coastal, name = "n_cities") %>%
  bind_rows(
    city_level %>%
      count(coastal, name = "n_cities") %>%
      mutate(Country = "Total")) %>%
  filter(coastal == 1) %>%
  select(Country, n_cities) %>%
  pivot_wider(
    names_from = Country,
    values_from = n_cities,
    values_fill = 0) %>%
  mutate(variables = "coastal", .before = 1) %>%
  to_char()

# 11. Climate zones
table_1d <- city_level %>%
  distinct(Country, CLZ, SALID1) %>%
  count(Country, CLZ, name = "n_cities") %>%
  bind_rows(
    city_level %>%
      distinct(CLZ, SALID1) %>%
      count(CLZ, name = "n_cities") %>%
      mutate(Country = "Total")) %>%
  group_by(Country) %>%
  mutate(
    total = sum(n_cities),
    pct_clz = 100 * n_cities / total) %>%
  ungroup() %>%
  select(Country, CLZ, pct_clz) %>%
  pivot_wider(
    names_from = Country,
    values_from = pct_clz,
    values_fill = 0) %>%
  mutate(
    across(-CLZ, ~ format(round(.x, 1), nsmall = 1))) %>%
  rename(variables = CLZ) %>%
  to_char()

# 12. Join all tables
table1 <- bind_rows(
  table_1a,
  table_1e,
  table_1b,
  table_1g,
  table_1h,
  table_1d)

# 13. Variable labels
var_labels <- c(
  n_cities = "Number of cities",
  R95P = "Annual R95p (mm)*",
  Rx1day = "Annual RX1day (mm)*",
  Rx5day = "Annual RX5day (mm)*",
  city_pop = "City population (thousands)",
  pop_density_guf = "Population density (1000 people per km2)",
  GDP = "GDP per capita (US$ thousands)",
  pop_over65 = "Population ≥65 years (%)",
  education = "Completed primary education (%)",
  NDVI = "Greenness - NDVI",
  median_elevation = "Elevation (m)",
  slope = "Slope (°)",
  coastal = "Coastal cities (n of cities)")

table1_names <- table1 %>%
  mutate(
    variables = dplyr::recode(variables, !!!as.list(var_labels)))

# 14. Add section headers
table1_names <- table1_names %>%
  add_row(
    variables = "City social and environmental characteristics*",
    .before = which(table1_names$variables == "City population (thousands)"))

table1_names <- table1_names %>%
  add_row(
    variables = "Climate zones (% of cities)",
    .before = which(
      table1_names$variables %in% unique(table_1d$variables)
    )[1])

# 15. Add footnote
col_1 <- names(table1_names)[1]

table1_names <- table1_names %>%
  add_row(
    !!col_1 := "* Values represent the median (10th, 90th percentiles) of city-specific averages across the study period (2000–2024)")

# 16. Organize columns
table1_names <- table1_names %>%
  select(
    "variables",
    "Total",
    "Argentina",
    "Brazil",
    "Central America",
    "Chile",
    "Colombia",
    "Mexico",
    "Peru") %>%
  rename(" " = variables)

# 17. Export table
write_xlsx(table1_names,"Tables/Table_1.xlsx")

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
    variable == "total_pop_z" ~ "Population sizea",
    variable == "pop_density_guf_z" ~ "Population densitya",
    variable == "GDP_z" ~ "GDP per capitac",
    variable == "pop_over65_z" ~ "Population ≥65 yearsa",
    variable == "slope_z" ~ "Sloped", 
    variable == "education_z" ~ "Completed primary educationb",
    variable == "elevation_z" ~ "Elevationd",
    variable == "NDVI_z" ~ "Greeness - NDVIc",
    variable == "coastal" ~ "Coastal cities vs non coastal citiesd",
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
    variable == "2.CLZ_num" ~ "Temperate",
    variable == "3b.CLZ_num" ~ "Tropical", 
    TRUE ~ variable)) %>% 
  mutate(across(-variable, ~ ifelse(variable == "Tropical", "Reference", .))) %>% 
  mutate(variable = factor(variable,
                           levels = c("Tropical", "Arid", "Temperate"))) %>%
  arrange(variable)

# 3. Final join - table 2
table2 <- rbind(univariate_join_v, univariate_join_clz)

str(table2)

# arrange and change reference 
table2 <- table2 %>%
  mutate(variable = factor(variable,
                           levels = c("Population sizea",
                                      "Population densitya",
                                      "Population ≥65 yearsa",
                                      "Completed primary educationb",
                                      "GDP per capitac",
                                      "Greeness - NDVIc",
                                      "Elevationd",
                                      "Sloped",
                                      "Coastal cities vs non coastal citiesd",
                                      "Tropical",
                                      "Arid",
                                      "Temperate"))) %>%
  arrange(variable) %>% 
  mutate(variable = as.character(variable))

table2 <- table2 %>% 
  mutate(R95P = ifelse(R95P == "Reference", "Reference category",R95P))

# line for climate zone
climate_row <- table2[1, ]
climate_row[] <- ""
climate_row$variable <- "Climate Zoned"

pos <- which(table2$variable == "Tropical")[1]

table2 <- rbind(
  table2[1:(pos - 1), , drop = FALSE],
  climate_row,
  table2[pos:nrow(table2), , drop = FALSE])

# note an export
note_row <- tibble(
  variable = paste0(
    "Note: * statistically significant (p < 0.05).\n",
    "a time-varying variable with interpolated/projected values;\n",
    "b time-varying variable with interpolation between census years and last observation carried forward;\n",
    "c time-varying variable with last observation carried forward for years without data availability;\n",
    "d time-invariant variable;\n",
    "Mean differences estimates are expressed per 1 SD increase in the pooled distribution of the city-level predictor across all city-year observations.\n"))

table2_final <- bind_rows(table2, note_row) %>% 
  rename("  " = "variable")

# rename columns
table2_final <- table2_final %>% 
  rename("Mean difference (95% CI) - R95p" = R95P,
         "Mean difference (95% CI) - RX1day" = Rx1day,
         "Mean difference (95% CI) - RX5day" = Rx5day)

write_xlsx(table2_final, "Tables/Table_2.xlsx")

#--------------------------------------------------
# Table 3 - climate zones baseline and trend
#--------------------------------------------------
# 1. baseline and trend
clz_R95P_int <- read_csv("Model_results/L1AD/R95P/R95P_CLZ_results.csv")
clz_Rx1day_int <- read_csv("Model_results/L1AD/Rx1day/Rx1day_CLZ_results.csv")
clz_Rx5day_int <- read_csv("Model_results/L1AD/Rx5day/Rx5day_CLZ_results.csv")

format_clz_interaction <- function(data, value_name){
  
  data %>%
    mutate(
      sig = ifelse(!is.na(Trend_p) & Trend_p < 0.05, "*", ""),
      value = sprintf("%.1f (%.1f, %.1f)%s",
                      Trend,
                      Trend_LCI,
                      Trend_UCI,
                      sig)) %>%
    mutate(Climate_zone = factor(Climate_zone,
                                 levels = c("Tropical", "Arid", "Temperate"))) %>%
    arrange(Climate_zone) %>%
    mutate(Climate_zone = as.character(Climate_zone)) %>%
    select(Climate_zone, value) %>%
    rename(clz = Climate_zone,
      !!value_name := value)
}

# table for each
clz_R95P_int_trend  <- format_clz_interaction(clz_R95P_int, "R95P")
clz_Rx1day_int_trend <- format_clz_interaction(clz_Rx1day_int, "Rx1day")
clz_Rx5day_int_trend <- format_clz_interaction(clz_Rx5day_int, "Rx5day")

# join
clz_trend <- list(clz_R95P_int_trend, clz_Rx1day_int_trend, clz_Rx5day_int_trend) %>%
  reduce(left_join, by = "clz")

# 2.  p values of interaction
clz_R95P_p <- read_csv("Model_results/L1AD/R95P/R95P_CLZ_interaction_test.csv")
clz_Rx1day_p <- read_csv("Model_results/L1AD/Rx1day/Rx1day_CLZ_interaction_test.csv")
clz_Rx5day_p <- read_csv("Model_results/L1AD/Rx5day/Rx5day_CLZ_interaction_test.csv")

clz_R95P_p <- clz_R95P_p %>%
  rename(R95P = p) %>%
  mutate(clz = "p-value for interaction") %>%
  select(clz, R95P)

clz_Rx1day_p <- clz_Rx1day_p %>%
  rename(Rx1day = p) %>%
  mutate(clz = "p-value for interaction") %>%
  select(clz, Rx1day)

clz_Rx5day_p <- clz_Rx5day_p %>%
  rename(Rx5day = p) %>%
  mutate(clz = "p-value for interaction") %>%
  select(clz, Rx5day)

# join p test
p_test <- clz_R95P_p %>% 
  full_join(clz_Rx1day_p, by = "clz") %>%
  full_join(clz_Rx5day_p, by = "clz") %>%
  mutate(across(c(R95P, Rx1day, Rx5day),
                ~ case_when(. < 0.0001 ~ "<0.0001",
                            . < 0.001  ~ "<0.001",
                            . < 0.01   ~ "<0.01",
                            . < 0.05   ~ "<0.05",
                            TRUE ~ sprintf("%.3f", .))))
# 3. join tables and organize
table_3 <- bind_rows(clz_trend, p_test)

table_3 <- table_3 %>%
  rename("Mean changes over time (95% CI) - R95p" = R95P,
         "Mean changes over time (95% CI) - RX1day" = Rx1day,
         "Mean changes over time (95% CI) - RX5day" = Rx5day)

note_row3 <- tibble(clz = paste0("Note: * statistically significant (p < 0.05).\n",
                      "For the reference category (Tropical), the values corresponds to the main effect of time only."))

table_3_final <- bind_rows(table_3, note_row3) %>%
  rename("  " = clz)

write_xlsx(table_3_final, "Tables/Table_3.xlsx")

#--------------------------------------------------
# Table 4 - hybrid
#--------------------------------------------------
# 1. Hybrids
hybrid_R95P <- read_csv("Model_results/L1AD/R95P/R95P_hybrid_models_with_slope.csv")
hybrid_Rx1day <- read_csv("Model_results/L1AD/Rx1day/Rx1day_hybrid_models_with_slope.csv")
hybrid_Rx5day <- read_csv("Model_results/L1AD/Rx5day/Rx5day_hybrid_models_with_slope.csv")

R95P_wht <- hybrid_R95P %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - R95p" = sprintf("%.1f (%.1f, %.1f)%s",
                                                               estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - R95p")

Rx1day_wht <- hybrid_Rx1day %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - RX1day" = sprintf("%.1f (%.1f, %.1f)%s",
                                                               estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - RX1day")

Rx5day_wht <- hybrid_Rx5day %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - RX5day" = sprintf("%.1f (%.1f, %.1f)%s",
                                                               estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - RX5day")

# join
table_4 <- list(R95P_wht, Rx1day_wht, Rx5day_wht) %>%
  reduce(left_join, by = "variable")

table_4 <- table_4 %>%
  mutate(variable = case_when(
    variable == "total_pop" ~ "Population size",
    variable == "pop_density_guf" ~ "Population density",
    variable == "GDP" ~ "GDP per capita",
    variable == "pop_over65" ~ "Population ≥65 years",
    variable == "education" ~ "Completed primary education",
    variable == "NDVI" ~ "Greeness - NDVI",
    TRUE ~ variable)) %>% 
  mutate(variable = factor(variable,
                           levels = c("Population size",
                                      "Population density",
                                      "Population ≥65 years",
                                      "Completed primary education",
                                      "GDP per capita",
                                      "Greeness - NDVI"))) %>%
  arrange(variable) %>% 
  mutate(variable = as.character(variable))

note_row4 <- tibble(variable = "Note:* statistically significant (p < 0.05);\n",
                    "Estimates are expressed per 1 SD increase in the within-city deviation of the predictor, calculated as the difference between each city-year value and the city-specific mean")

table_4_final <- bind_rows(table_4, note_row4) %>% 
  rename("  " = variable)

write_xlsx(table_4_final, "Tables/Table_4.xlsx")
