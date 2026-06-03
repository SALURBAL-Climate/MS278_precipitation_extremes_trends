##################################################################
# MS278: Precipitation descriptive - city center point
#
# Manuscript supplementary tables
##################################################################

library(readr); library(dplyr); library(tidyverse)
library(purrr); library(writexl); library(tidyr)
library(glue); library(foreign)

#--------------------------------------------------
# Supplementary table 1
#--------------------------------------------------
# 1. No categorical variables
univariate_R95PCC <- read_csv("Model_results/City_center/R95PCC/R95PCC_univariate.csv")
univariate_Rx1dayCC <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_univariate.csv")
univariate_Rx5dayCC <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_univariate.csv")

univariate_R95PCC <- univariate_R95PCC %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         R95PCC = sprintf("%.1f (%.1f, %.1f)%s",
                        estimate, min95, max95, sig)) %>%
  select(variable, R95PCC)

univariate_Rx1dayCC <- univariate_Rx1dayCC %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         Rx1dayCC = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  select(variable, Rx1dayCC)

univariate_Rx5dayCC <- univariate_Rx5dayCC %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         Rx5dayCC = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  select(variable, Rx5dayCC)


univariate_join_v <- list(univariate_R95PCC,
                          univariate_Rx1dayCC,
                          univariate_Rx5dayCC) %>%
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
clz_Rx1dayCC <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_CLZ.csv")
clz_Rx5dayCC <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_CLZ.csv")

clz_R95PCC <- clz_R95PCC %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         R95PCC = sprintf("%.1f (%.1f, %.1f)%s",
                        estimate, min95, max95, sig)) %>%
  select(parm, R95PCC) %>% 
  rename(variable = parm) %>% 
  filter(!variable %in% c("YEAR_dec", "_cons"))

clz_Rx1dayCC <- clz_Rx1dayCC %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         Rx1dayCC = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  select(parm, Rx1dayCC) %>% 
  rename(variable = parm) %>% 
  filter(!variable %in% c("YEAR_dec", "_cons"))

clz_Rx5dayCC <- clz_Rx5dayCC %>%
  mutate(sig = ifelse(p < 0.05, "*", ""),
         Rx5dayCC = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  select(parm, Rx5dayCC) %>% 
  rename(variable = parm) %>% 
  filter(!variable %in% c("YEAR_dec", "_cons"))

# join
univariate_join_clz <- list(clz_R95PCC,
                            clz_Rx1dayCC,
                            clz_Rx5dayCC) %>%
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
sup_table1 <- rbind(univariate_join_v, univariate_join_clz)

str(sup_table1)

# arrange and change reference 
sup_table1 <- sup_table1 %>%
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
                                      "Polar",
                                      "Temperate"))) %>%
  arrange(variable) %>% 
  mutate(variable = as.character(variable))

sup_table1 <- sup_table1 %>% 
  mutate(R95PCC = ifelse(R95PCC == "Reference", "Reference category",R95PCC))

# line for climate zone
climate_row <- sup_table1[1, ] %>% 
  mutate(across(everything(), ~ "")) %>%
  mutate(variable = "Climate Zoned")
pos <- which(sup_table1$variable == "Tropical")[1]
sup_table1 <- bind_rows(sup_table1[1:(pos - 1), ],
                    climate_row, sup_table1[pos:nrow(sup_table1), ])
# note an export
note_row <- tibble(
  variable = paste0(
    "Note: * statistically significant (p < 0.05).\n",
    "a time-varying variable with interpolated/projected values;\n",
    "b time-varying variable with interpolation between census years and last observation carried forward;\n",
    "c time-varying variable with last observation carried forward for years without data availability;\n",
    "d time-invariant variable;\n",
    "Mean differences are per SD higher value of the city-level predictor unless otherwise noted.\n")
)

sup_table1_final <- bind_rows(sup_table1, note_row) %>% 
  rename("  " = "variable")

# rename columns
sup_table1_final <- sup_table1_final %>% 
  rename("Mean difference (95% CI) - R95P" = R95PCC,
         "Mean difference (95% CI) - Rx1day" = Rx1dayCC,
         "Mean difference (95% CI) - Rx5day" = Rx5dayCC)

write_xlsx(sup_table1_final, "Tables/Sup_Table_1.xlsx")

#--------------------------------------------------
# Supplementary Table 2 
#--------------------------------------------------
# 1. baseline and interaction
clz_R95PCC_int <- read_csv("Model_results/City_center/R95PCC/R95PCC_CLZ_interaction.csv")
clz_Rx1dayCC_int <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_CLZ_interaction.csv")
clz_Rx5dayCC_int <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_CLZ_interaction.csv")

format_clz_interaction <- function(data, value_name){
  data %>%
    filter(grepl("YEAR_dec", parm)) %>%
    mutate(parm = ifelse(parm == "YEAR_dec",
                         "4b.CLZ_num#c.YEAR_dec",
                         parm)) %>%
    mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", "")) %>%
    mutate(value = sprintf("%.1f (%.1f, %.1f)%s",
                           estimate, min95, max95, sig)) %>%
    mutate(clz = case_when(
      grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
      grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
      grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
      grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num")) %>%
    select(clz, value) %>%
    distinct(clz, .keep_all = TRUE) %>%
    mutate(clz = recode(clz,
                        "1.CLZ_num"  = "Arid",
                        "2.CLZ_num"  = "Polar",
                        "3.CLZ_num"  = "Temperate",
                        "4b.CLZ_num" = "Tropical")) %>%
    mutate(clz = factor(clz,
                        levels = c("Tropical",
                                   "Arid",
                                   "Temperate",
                                   "Polar"))) %>%
    arrange(clz) %>%
    mutate(clz = as.character(clz)) %>%
    rename(!!value_name := value)
}

# table for each
clz_R95PCC_int_p   <- format_clz_interaction(clz_R95PCC_int, "R95P")
clz_Rx1dayCC_int_p <- format_clz_interaction(clz_Rx1dayCC_int, "Rx1day")
clz_Rx5dayCC_int_p <- format_clz_interaction(clz_Rx5dayCC_int, "Rx5day")

# 2.  p values of interaction (results from stata)
clz_R95PCC_int_p <- clz_R95PCC_int_p %>%
  add_row(clz = "p-value for interaction", R95P = "<0.001")

clz_Rx1dayCC_int_p <- clz_Rx1dayCC_int_p %>%
  add_row(clz = "p-value for interaction", Rx1day = "<0.001")

clz_Rx5dayCC_int_p <- clz_Rx5dayCC_int_p %>%
  add_row(clz = "p-value for interaction", Rx5day = "<0.001")

# 3. join tables and organize
sup_table_2 <- list(clz_R95PCC_int_p, clz_Rx1dayCC_int_p, clz_Rx5dayCC_int_p) %>%
  reduce(left_join, by = "clz")

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

note_row3 <- tibble(variable = "Note:* statistically significant (p < 0.05)")

sup_table_3_final <- bind_rows(sup_table_3, note_row3) %>% 
  rename("  " = variable)

write_xlsx(sup_table_3_final, "Tables/sup_table_3.xlsx")

#--------------------------------------------------
# Supplementary Table 4
#--------------------------------------------------
# Results from stata

#--------------------------------------------------
# Supplementary Table 5
#--------------------------------------------------
slopes_R95PCC <- read_csv("Model_results/L1AD/R95PCC/R95PCC_city_random_slopes.csv")
slopes_Rx1dayCC <- read_csv("Model_results/L1AD/Rx1dayCC/Rx1dayCC_city_random_slopes.csv")
slopes_Rx5dayCC <- read_csv("Model_results/L1AD/Rx5dayCC/Rx5dayCC_city_random_slopes.csv")
L1AD_name <- read.dbf("Data/SHP/L1AD_centroid.dbf")

# Prepare data
slopes_R95PCC <- slopes_R95PCC %>% 
  rename(R95PCC = slope_total) %>% 
  mutate(R95PCC = round(R95PCC, 0)) %>% 
  select(SALID1, R95PCC)

slopes_Rx1dayCC <- slopes_Rx1dayCC %>% 
  rename(Rx1dayCC = slope_total) %>% 
  mutate(Rx1dayCC = round(Rx1dayCC, 0)) %>% 
  select(SALID1, Rx1dayCC)

slopes_Rx5dayCC <- slopes_Rx5dayCC %>% 
  rename(Rx5dayCC = slope_total) %>% 
  mutate(Rx5dayCC = round(Rx5dayCC, 0)) %>% 
  select(SALID1, Rx5dayCC)

L1AD_name <- L1AD_name %>% 
  select(L1Name, Country, SALID1) %>% 
  rename("City name" = L1Name)

sup_table_5 <- slopes_R95PCC %>% left_join(slopes_Rx1dayCC, by = "SALID1") %>% 
  left_join(slopes_Rx5dayCC, by = "SALID1") %>% 
  left_join(L1AD_name, by = "SALID1")

sup_table5_final <- sup_table_5 %>% 
  select("City name", Country, R95PCC, Rx1dayCC, Rx5dayCC) %>% 
  rename("R95PCC (mm per decade)" = R95PCC,
         "Rx1dayCC (mm per decade)" = Rx1dayCC, 
         "Rx5dayCC (mm per decade)" = Rx5dayCC) %>% 
  arrange(Country, `City name`)

write_xlsx(sup_table5_final, "Tables/sup_table_5.xlsx")
