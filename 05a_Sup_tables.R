##################################################################
# MS278: Precipitation descriptive - city center point
#
# Manuscript supplementary tables
##################################################################

library(readr); library(dplyr); library(tidyverse)
library(purrr); library(writexl); library(tidyr)
library(glue); library(foreign); library(readxl)
library(dplyr)

#--------------------------------------------------
# Supplementary table 1
#--------------------------------------------------
# 1. No categorical variables
univariate_R95PCC <- read_csv("Model_results/City_center/R95PCC/R95PCC_univariate.csv")
univariate_RX1dayCC <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_univariate.csv")
univariate_RX5dayCC <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_univariate.csv")

univariate_R95PCC <- univariate_R95PCC %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         R95PCC = sprintf("%.1f (%.1f, %.1f)%s",
                        estimate, min95, max95, sig)) %>%
  select(variable, R95PCC)

univariate_RX1dayCC <- univariate_RX1dayCC %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         RX1dayCC = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  select(variable, RX1dayCC)

univariate_RX5dayCC <- univariate_RX5dayCC %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         RX5dayCC = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  select(variable, RX5dayCC)


univariate_join_v <- list(univariate_R95PCC,
                          univariate_RX1dayCC,
                          univariate_RX5dayCC) %>%
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
clz_R95PCC <- read_csv("Model_results/City_center/R95PCC/R95PCC_CLZ.csv")
clz_RX1dayCC <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_CLZ.csv")
clz_RX5dayCC <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_CLZ.csv")

clz_R95PCC <- clz_R95PCC %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         R95PCC = sprintf("%.1f (%.1f, %.1f)%s",
                        estimate, min95, max95, sig)) %>%
  select(parm, R95PCC) %>% 
  rename(variable = parm) %>% 
  filter(!variable %in% c("YEAR_dec", "_cons"))

clz_RX1dayCC <- clz_RX1dayCC %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         RX1dayCC = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  select(parm, RX1dayCC) %>% 
  rename(variable = parm) %>% 
  filter(!variable %in% c("YEAR_dec", "_cons"))

clz_RX5dayCC <- clz_RX5dayCC %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         RX5dayCC = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  select(parm, RX5dayCC) %>% 
  rename(variable = parm) %>% 
  filter(!variable %in% c("YEAR_dec", "_cons"))

str(clz_R95PCC)

# join
univariate_join_clz <- list(clz_R95PCC,
                            clz_RX1dayCC,
                            clz_RX5dayCC) %>%
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
sup_table_1 <- rbind(univariate_join_v, univariate_join_clz)

str(sup_table_1)

# arrange and change reference 
sup_table_1 <- sup_table_1 %>%
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

sup_table_1 <- sup_table_1 %>% 
  mutate(R95PCC = ifelse(R95PCC == "Reference", "Reference category",R95PCC))

# line for climate zone
climate_row <- sup_table_1[1, ]
climate_row[] <- ""
climate_row$variable <- "Climate Zoned"

pos <- which(sup_table_1$variable == "Tropical")[1]

sup_table_1 <- rbind(
  sup_table_1[1:(pos - 1), , drop = FALSE],
  climate_row,
  sup_table_1[pos:nrow(sup_table_1), , drop = FALSE])

# note an export
note_row <- tibble(
  variable = paste0(
    "Note: * statistically significant (p < 0.05).\n",
    "a time-varying variable with interpolated/projected values;\n",
    "b time-varying variable with interpolation between census years and last observation carried forward;\n",
    "c time-varying variable with last observation carried forward for years without data availability;\n",
    "d time-invariant variable;\n",
    "Mean differences estimates are expressed per 1 SD increase in the within-city deviation of the predictor, calculated as the difference between each city-year value and the city-specific mean.\n"))

sup_table_1_final <- bind_rows(sup_table_1, note_row) %>% 
  rename("  " = "variable")

# rename columns
sup_table_1_final <- sup_table_1_final %>% 
  rename("Mean difference (95% CI) - R95P" = R95PCC,
         "Mean difference (95% CI) - RX1day" = RX1dayCC,
         "Mean difference (95% CI) - RX5day" = RX5dayCC)

write_xlsx(sup_table_1_final, "Tables/Sup_table_1.xlsx")

#--------------------------------------------------
# Supplementary Table 2 
#--------------------------------------------------
# 1. baseline and trend
clz_R95P_int <- read_csv("Model_results/City_center/R95PCC/R95PCC_CLZ_results.csv")
clz_Rx1day_int <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_CLZ_results.csv")
clz_Rx5day_int <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_CLZ_results.csv")

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
clz_R95P_p <- read_csv("Model_results/City_center/R95PCC/R95PCC_CLZ_interaction_test.csv")
clz_Rx1day_p <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_CLZ_interaction_test.csv")
clz_Rx5day_p <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_CLZ_interaction_test.csv")

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
sup_table_2 <- bind_rows(clz_trend, p_test)

sup_table_2 <- sup_table_2 %>%
  rename("Mean changes over time (95% CI) - R95p" = R95P,
         "Mean changes over time (95% CI) - RX1day" = Rx1day,
         "Mean changes over time (95% CI) - RX5day" = Rx5day)

note_row2 <- tibble(clz = paste0("Note: * statistically significant (p < 0.05).\n",
                                 "For the reference category (Tropical), the values corresponds to the main effect of time only."))

sup_table_2_final <- bind_rows(sup_table_2, note_row2) %>%
  rename("  " = clz)

write_xlsx(sup_table_2_final, "Tables/Sup_table_2.xlsx")

#--------------------------------------------------
# Supplementary Table 3 
#--------------------------------------------------
# 1. Hybrids 
hybrid_R95PCC <- read_csv("Model_results/City_center/R95PCC/R95PCC_hybrid_models_with_slope.csv")
hybrid_Rx1dayCC <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_hybrid_models_with_slope.csv")
hybrid_Rx5dayCC <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_hybrid_models_with_slope.csv")

R95P_wht <- hybrid_R95PCC %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - R95p" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                      estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - R95p")

Rx1day_wht <- hybrid_Rx1dayCC  %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - RX1day" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                        estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - RX1day")

Rx5day_wht <- hybrid_Rx5dayCC  %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - RX5day" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                        estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - RX5day")

# join
sup_table_3 <- list(R95P_wht, Rx1day_wht, Rx5day_wht) %>%
  reduce(left_join, by = "variable")

sup_table_3 <- sup_table_3 %>%
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

note_row3 <- tibble(variable = "Note:* statistically significant (p < 0.05); \n",
                    "Estimates are expressed per 1 SD increase in the within-city deviation of the predictor, calculated as the difference between each city-year value and the city-specific mean.")

sup_table_3_final <- bind_rows(sup_table_3, note_row3) %>% 
  rename("  " = variable)

write_xlsx(sup_table_3_final, "Tables/sup_table_3.xlsx")

#--------------------------------------------------
# Supplementary Table 4
#--------------------------------------------------
null_R95P <- read_xlsx("Model_results/L1AD/R95P/R95P_null_variance.xlsx")
null_Rx1day <- read_xlsx("Model_results/L1AD/Rx1day/Rx1day_null_variance.xlsx")
null_Rx5day <- read_xlsx("Model_results/L1AD/Rx5day/Rx5day_null_variance.xlsx")

sup_table_4 <-  bind_rows(null_R95P, null_Rx1day, null_Rx5day)

sup_table_4_final <- sup_table_4 %>%  
  rename("Between-city variance" = Between,
         "Within-city variance" = Within,
         "ICC (%)" = ICC_pct) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 1)))

write_xlsx(sup_table_4_final, "Tables/sup_table_4.xlsx")

#--------------------------------------------------
# Supplementary Table 5
#--------------------------------------------------
slopes_R95P <- read_csv("Model_results/L1AD/R95P/R95P_city_random_slopes.csv")
slopes_Rx1day <- read_csv("Model_results/L1AD/Rx1day/Rx1day_city_random_slopes.csv")
slopes_Rx5day <- read_csv("Model_results/L1AD/Rx5day/Rx5day_city_random_slopes.csv")
L1AD_name <- read.dbf("Data/SHP/L1AD_centroid.dbf")

# Calculate city-slopes summary and pct
# Function to calculate slope summary
get_slope_summary <- function(data, index_name) {
  
  q <- quantile(
    data$slope_total,
    probs = c(0.025, 0.25, 0.75, 0.975),
    na.rm = TRUE)
  
  trend <- data %>%
    mutate(slope_rounded = round(slope_total, 0),
           trend = case_when(slope_rounded > 0 ~ "Increasing", 
                             slope_rounded < 0 ~ "Decreasing", TRUE ~ "Stable")) %>%
    count(trend) %>%
    mutate(percent = round(100 * n / sum(n), 0))
  
  data.frame(Index = index_name,
             P2.5 = q[1],
             P25 = q[2],
             P75 = q[3],
             P97.5 = q[4],
             Increasing_pct = trend$percent[match("Increasing", trend$trend)],
             Stable_pct = trend$percent[match("Stable", trend$trend)],
             Decreasing_pct = trend$percent[match("Decreasing", trend$trend)])
}

# Create final table
slope_summary <- bind_rows(
  get_slope_summary(slopes_R95P, "R95P"),
  get_slope_summary(slopes_Rx1day, "RX1day"),
  get_slope_summary(slopes_Rx5day, "RX5day"))

slope_summary <- slope_summary %>%
  mutate(across(c(P2.5, P25, P75, P97.5), ~ round(.x, 2)))

write_xlsx(slope_summary, "Tables/city_slope_summary.xlsx")

# Prepare data for sup table 5
slopes_R95P <- slopes_R95P %>% 
  rename(R95P = slope_total) %>% 
  mutate(R95P = round(R95P, 0)) %>% 
  select(SALID1, R95P)

slopes_Rx1day <- slopes_Rx1day %>% 
  rename(Rx1day = slope_total) %>% 
  mutate(Rx1day = round(Rx1day, 0)) %>% 
  select(SALID1, Rx1day)

slopes_Rx5day <- slopes_Rx5day %>% 
  rename(Rx5day = slope_total) %>% 
  mutate(Rx5day = round(Rx5day, 0)) %>% 
  select(SALID1, Rx5day)

L1AD_name <- L1AD_name %>% 
  select(L1Name, Country, SALID1) %>% 
  rename("City name" = L1Name)

sup_table_5 <- slopes_R95P %>% left_join(slopes_Rx1day, by = "SALID1") %>% 
  left_join(slopes_Rx5day, by = "SALID1") %>% 
  left_join(L1AD_name, by = "SALID1")

sup_table5_final <- sup_table_5 %>% 
  select("City name", Country, R95P, Rx1day, Rx5day) %>% 
  rename("R95P (mm per decade)" = R95P,
         "Rx1day (mm per decade)" = Rx1day, 
         "Rx5day (mm per decade)" = Rx5day) %>% 
  arrange(Country, `City name`)

write_xlsx(sup_table5_final, "Tables/sup_table_5.xlsx")

#-----------------------------------------------------
# Supplementary Table 6
#-----------------------------------------------------
data <- read_parquet("Data/data_prec_final_wht_polar.parquet")

vars <- c(
  "total_pop",
  "pop_density_guf",
  "pop_over65",
  "GDP",
  "education",
  "NDVI")

z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data <- data %>%
  group_by(SALID1) %>%
  mutate(across(all_of(vars), ~ .x - mean(.x, na.rm = TRUE),.names = "{.col}_wht")) %>%
  ungroup()

data <- data %>%
  mutate(across(all_of(vars), z_score,.names = "{.col}_z"))

data <- data %>%
  mutate(across(all_of(paste0(vars, "_wht")), z_score,.names = "{.col}_z"))

data %>%
  select(SALID1,
         YEAR,
         all_of(vars),
         all_of(paste0(vars, "_z")),
         all_of(paste0(vars, "_wht")),
         all_of(paste0(vars, "_wht_z")))

variable_labels <- c(
  total_pop = "City population",
  pop_density_guf = "Population density",
  pop_over65 = "Population ≥65 years",
  GDP = "GDP per capita",
  education = "Completed primary education",
  NDVI = "NDVI")

sd_table <- tibble(`City characteristic` = unname(variable_labels),
                   `SD (Pooled)` = sapply(names(variable_labels),
                                          function(v) {
                                            sd(data[[v]], na.rm = TRUE)
                                          }),
                   `SD (Within-city)` = sapply(
                     names(variable_labels),
                     function(v) {
                       sd(data[[paste0(v, "_wht")]], na.rm = TRUE)
                     }))

sup_table_6 <- sd_table %>%
  mutate(`SD (Pooled)` = round(`SD (Pooled)`, 3),
         `SD (Within-city)` = round(`SD (Within-city)`, 3))


sup_table_6 <- tibble(
  `City characteristic` = unname(variable_labels),
  `SD (Pooled)` = sapply(
    names(variable_labels),
    function(v) sd(data[[v]], na.rm = TRUE)),
  `SD (Within-city)` = sapply(
    names(variable_labels),
    function(v) sd(data[[paste0(v, "_wht")]], na.rm = TRUE)))

sup_table_6 <- sup_table_6 %>%
  mutate(`SD (Pooled)` = round(`SD (Pooled)`, 4), 
         `SD (Within-city)` = round(`SD (Within-city)`, 4))


write_xlsx(sup_table_6, "Tables/Sup_table_6.xlsx")