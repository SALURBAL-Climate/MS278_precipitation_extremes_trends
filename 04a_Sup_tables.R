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

str(clz_R95PCC)

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
  mutate(across(-variable, ~ ifelse(variable == "Tropical", "1 (Ref.)", .))) %>% 
  mutate(variable = factor(variable,
                           levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(variable)

# 3. Final join - table 2
sup_table1 <- rbind(univariate_join_v, univariate_join_clz)

str(table2)

# arrange
sup_table1 <- sup_table1 %>%
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
  rename(R95p = R95PCC,
         R1Xday = Rx1dayCC,
         R5Xday = Rx5dayCC) %>% 
  arrange(variable) %>% 
  mutate(variable = as.character(variable))

# line for climate zone
climate_row <- sup_table1[1, ] %>% 
  mutate(across(everything(), ~ "")) %>%
  mutate(variable = "Climate Zone")
pos <- which(sup_table1$variable == "Tropical")[1]
sup_table1 <- bind_rows(sup_table1[1:(pos - 1), ],
                    climate_row, sup_table1[pos:nrow(sup_table1), ])

# note an export
note_row <- tibble(variable = "Note:* statistically significant (p < 0.05)")

sup_table1_final <- bind_rows(sup_table1, note_row) %>% 
  rename("  " = "variable")

write_xlsx(sup_table1_final, "Tables/Sup_Table_1.xlsx")

#--------------------------------------------------
# Supplementary Table 2 
#--------------------------------------------------
# 1. baseline and interaction - no control
clz_R95PCC_int <- read_csv("Model_results/City_center/R95PCC/R95PCC_CLZ_interaction.csv")
clz_Rx1dayCC_int <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_CLZ_interaction.csv")
clz_Rx5dayCC_int <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_CLZ_interaction.csv")

clz_R95PCC_int <- clz_R95PCC_int %>%
  filter(parm != "4b.CLZ_num#co.YEAR_dec") %>%
  mutate(parm = ifelse(parm == "YEAR_dec",
                       "4b.CLZ_num#co.YEAR_dec", parm)) %>%
  mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", ""),
         R95PCC = sprintf("%.1f (%.1f, %.1f)%s",
                        estimate, min95, max95, sig)) %>%
  mutate(clz = case_when(grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
                         grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
                         grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
                         grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num"),
         term = case_when(grepl("#.*YEAR_dec", parm) ~ "Mean changes over time (95% CI) - Unadjusted model",
                          TRUE ~ "Mean differences at baseline (95% CI) - Unadjusted model")) %>%
  filter(!parm %in% c("_cons")) %>%
  select(clz, term, R95PCC) %>%
  distinct(clz, term, .keep_all = TRUE) %>%
  mutate(R95PCC = ifelse(clz == "4b.CLZ_num" &
                         term == "Mean differences at baseline (95% CI) - Unadjusted model", 
                       "1 (Ref.)", R95PCC)) %>%
  pivot_wider(names_from  = term, values_from = R95PCC) %>%
  mutate(clz = recode(clz, 
                      "1.CLZ_num"  = "Arid",
                      "2.CLZ_num"  = "Polar",
                      "3.CLZ_num"  = "Temperate",
                      "4b.CLZ_num" = "Tropical")) %>%
  mutate(clz = factor(clz, levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(clz) %>%
  mutate(clz = as.character(clz))

clz_Rx1dayCC_int <- clz_Rx1dayCC_int %>%
  filter(parm != "4b.CLZ_num#co.YEAR_dec") %>%
  mutate(parm = ifelse(parm == "YEAR_dec",
                       "4b.CLZ_num#co.YEAR_dec", parm)) %>%
  mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", ""),
         Rx1dayCC = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  mutate(clz = case_when(grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
                         grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
                         grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
                         grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num"),
         term = case_when(grepl("#.*YEAR_dec", parm) ~ "Mean changes over time (95% CI) - Unadjusted model",
                          TRUE ~ "Mean differences at baseline (95% CI) - Unadjusted model")) %>%
  filter(!parm %in% c("_cons")) %>%
  select(clz, term, Rx1dayCC) %>%
  distinct(clz, term, .keep_all = TRUE) %>%
  mutate(Rx1dayCC = ifelse(clz == "4b.CLZ_num" &
                           term == "Mean differences at baseline (95% CI) - Unadjusted model", 
                         "1 (Ref.)", Rx1dayCC)) %>%
  pivot_wider(names_from  = term, values_from = Rx1dayCC) %>%
  mutate(clz = recode(clz, 
                      "1.CLZ_num"  = "Arid",
                      "2.CLZ_num"  = "Polar",
                      "3.CLZ_num"  = "Temperate",
                      "4b.CLZ_num" = "Tropical")) %>%
  mutate(clz = factor(clz, levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(clz) %>%
  mutate(clz = as.character(clz))

clz_Rx5dayCC_int <- clz_Rx5dayCC_int %>%
  filter(parm != "4b.CLZ_num#co.YEAR_dec") %>%
  mutate(parm = ifelse(parm == "YEAR_dec",
                       "4b.CLZ_num#co.YEAR_dec", parm)) %>%
  mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", ""),
         Rx5dayCC = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  mutate(clz = case_when(grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
                         grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
                         grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
                         grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num"),
         term = case_when(grepl("#.*YEAR_dec", parm) ~ "Mean changes over time (95% CI) - Unadjusted model",
                          TRUE ~ "Mean differences at baseline (95% CI) - Unadjusted model")) %>%
  filter(!parm %in% c("_cons")) %>%
  select(clz, term, Rx5dayCC) %>%
  distinct(clz, term, .keep_all = TRUE) %>%
  mutate(Rx5dayCC = ifelse(clz == "4b.CLZ_num" &
                           term == "Mean differences at baseline (95% CI) - Unadjusted model", 
                         "1 (Ref.)", Rx5dayCC)) %>%
  pivot_wider(names_from  = term, values_from = Rx5dayCC) %>%
  mutate(clz = recode(clz, 
                      "1.CLZ_num"  = "Arid",
                      "2.CLZ_num"  = "Polar",
                      "3.CLZ_num"  = "Temperate",
                      "4b.CLZ_num" = "Tropical")) %>%
  mutate(clz = factor(clz, levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(clz) %>%
  mutate(clz = as.character(clz))

# 2. baseline and interaction - with control
clz_R95PCC_cont <- read_csv("Model_results/City_center/R95PCC/R95PCC_CLZ_interaction_controlled.csv")
clz_Rx1dayCC_cont <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_CLZ_interaction_controlled.csv")
clz_Rx5dayCC_cont <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_CLZ_interaction_controlled.csv")

clz_R95PCC_cont <- clz_R95PCC_cont %>%
  filter(parm != "4b.CLZ_num#co.YEAR_dec") %>%
  mutate(parm = ifelse(parm == "YEAR_dec",
                       "4b.CLZ_num#co.YEAR_dec", parm)) %>%
  mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", ""),
         R95PCC = sprintf("%.1f (%.1f, %.1f)%s",
                        estimate, min95, max95, sig)) %>%
  mutate(clz = case_when(grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
                         grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
                         grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
                         grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num"),
         term = case_when(grepl("#.*YEAR_dec", parm) ~ "Mean changes over time (95% CI) - Adjusted model",
                          TRUE ~ "Mean differences at baseline (95% CI) - Adjusted model")) %>%
  filter(!parm %in% c("_cons")) %>%
  select(clz, term, R95PCC) %>%
  distinct(clz, term, .keep_all = TRUE) %>%
  mutate(R95PCC = ifelse(clz == "4b.CLZ_num" &
                         term == "Mean differences at baseline (95% CI) - Adjusted model", 
                       "1 (Ref.)", R95PCC)) %>%
  pivot_wider(names_from  = term, values_from = R95PCC) %>%
  mutate(clz = recode(clz, 
                      "1.CLZ_num"  = "Arid",
                      "2.CLZ_num"  = "Polar",
                      "3.CLZ_num"  = "Temperate",
                      "4b.CLZ_num" = "Tropical")) %>%
  mutate(clz = factor(clz, levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(clz) %>%
  mutate(clz = as.character(clz))

clz_Rx1dayCC_cont <- clz_Rx1dayCC_cont %>%
  filter(parm != "4b.CLZ_num#co.YEAR_dec") %>%
  mutate(parm = ifelse(parm == "YEAR_dec",
                       "4b.CLZ_num#co.YEAR_dec", parm)) %>%
  mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", ""),
         Rx1dayCC = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  mutate(clz = case_when(grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
                         grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
                         grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
                         grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num"),
         term = case_when(grepl("#.*YEAR_dec", parm) ~ "Mean changes over time (95% CI) - Adjusted model",
                          TRUE ~ "Mean differences at baseline (95% CI) - Adjusted model")) %>%
  filter(!parm %in% c("_cons")) %>%
  select(clz, term, Rx1dayCC) %>%
  distinct(clz, term, .keep_all = TRUE) %>%
  mutate(Rx1dayCC = ifelse(clz == "4b.CLZ_num" &
                           term == "Mean differences at baseline (95% CI) - Adjusted model", 
                         "1 (Ref.)", Rx1dayCC)) %>%
  pivot_wider(names_from  = term, values_from = Rx1dayCC) %>%
  mutate(clz = recode(clz, 
                      "1.CLZ_num"  = "Arid",
                      "2.CLZ_num"  = "Polar",
                      "3.CLZ_num"  = "Temperate",
                      "4b.CLZ_num" = "Tropical")) %>%
  mutate(clz = factor(clz, levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(clz) %>%
  mutate(clz = as.character(clz))

clz_Rx5dayCC_cont <- clz_Rx5dayCC_cont %>%
  filter(parm != "4b.CLZ_num#co.YEAR_dec") %>%
  mutate(parm = ifelse(parm == "YEAR_dec",
                       "4b.CLZ_num#co.YEAR_dec", parm)) %>%
  mutate(sig = ifelse(!is.na(p) & p < 0.05, "*", ""),
         Rx5dayCC = sprintf("%.1f (%.1f, %.1f)%s",
                          estimate, min95, max95, sig)) %>%
  mutate(clz = case_when(grepl("^1\\.CLZ_num", parm)  ~ "1.CLZ_num",
                         grepl("^2\\.CLZ_num", parm)  ~ "2.CLZ_num",
                         grepl("^3\\.CLZ_num", parm)  ~ "3.CLZ_num",
                         grepl("^4b\\.CLZ_num", parm) ~ "4b.CLZ_num"),
         term = case_when(grepl("#.*YEAR_dec", parm) ~ "Mean changes over time (95% CI) - Adjusted model",
                          TRUE ~ "Mean differences at baseline (95% CI) - Adjusted model")) %>%
  filter(!parm %in% c("_cons")) %>%
  select(clz, term, Rx5dayCC) %>%
  distinct(clz, term, .keep_all = TRUE) %>%
  mutate(Rx5dayCC = ifelse(clz == "4b.CLZ_num" &
                           term == "Mean differences at baseline (95% CI) - Adjusted model", 
                         "1 (Ref.)", Rx5dayCC)) %>%
  pivot_wider(names_from  = term, values_from = Rx5dayCC) %>%
  mutate(clz = recode(clz, 
                      "1.CLZ_num"  = "Arid",
                      "2.CLZ_num"  = "Polar",
                      "3.CLZ_num"  = "Temperate",
                      "4b.CLZ_num" = "Tropical")) %>%
  mutate(clz = factor(clz, levels = c("Tropical", "Arid", "Temperate", "Polar"))) %>%
  arrange(clz) %>%
  mutate(clz = as.character(clz))

# 3. join tables
table_R95PCC <- clz_R95PCC_int %>%
  left_join(clz_R95PCC_cont, by = "clz")
table_R95PCC$prec <- "R95p"

table_Rx1dayCC <- clz_Rx1dayCC_int %>%
  left_join(clz_Rx1dayCC_cont, by = "clz")
table_Rx1dayCC$prec <- "RX1day"

table_Rx5dayCC <- clz_Rx5dayCC_int %>%
  left_join(clz_Rx5dayCC_cont, by = "clz")
table_Rx5dayCC$prec <- "RX5day"

sup_table_2 <- rbind(table_R95PCC, table_Rx1dayCC, table_Rx5dayCC) %>%
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

sup_table_2_final <- bind_rows(sup_table_2, note_row_sup) %>%
  rename("   " = prec,"  " = clz)

write_xlsx(sup_table_2_final, "Tables/Sup_table_2.xlsx")

#--------------------------------------------------
# Supplementary Table 3 
#--------------------------------------------------
# 1. Hybrids - no control
hybrid_R95PCC <- read_csv("Model_results/City_center/R95PCC/R95PCC_hybrid_models_with_slope.csv")
hybrid_Rx1dayCC <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_hybrid_models_with_slope.csv")
hybrid_Rx5dayCC <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_hybrid_models_with_slope.csv")

R95PCC_btw <- hybrid_R95PCC %>% 
  filter(variable_type == "between") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Difference in mean (95% CI) - Unadjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                    estimate, min95, max95, sig)) %>%
  select(variable, "Difference in mean (95% CI) - Unadjusted model")

R95PCC_wht <- hybrid_R95PCC %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - Unadjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                                  estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - Unadjusted model")

Rx1dayCC_btw <- hybrid_Rx1dayCC %>% 
  filter(variable_type == "between") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Difference in mean (95% CI) - Unadjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                    estimate, min95, max95, sig)) %>%
  select(variable, "Difference in mean (95% CI) - Unadjusted model")

Rx1dayCC_wht <- hybrid_Rx1dayCC %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - Unadjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                                  estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - Unadjusted model")

Rx5dayCC_btw <- hybrid_Rx5dayCC %>% 
  filter(variable_type == "between") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Difference in mean (95% CI) - Unadjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                    estimate, min95, max95, sig)) %>%
  select(variable, "Difference in mean (95% CI) - Unadjusted model")

Rx5dayCC_wht <- hybrid_Rx5dayCC %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - Unadjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                                  estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - Unadjusted model")

R95PCC_h <- list(R95PCC_btw, R95PCC_wht) %>%
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
hybrid_R95PCC_c <- read_csv("Model_results/City_center/R95PCC/R95PCC_hybrid_models_with_slope_controlled.csv")
hybrid_Rx1dayCC_c <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_hybrid_models_with_slope_controlled.csv")
hybrid_Rx5dayCC_c <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_hybrid_models_with_slope_controlled.csv")

R95PCC_btw_c <- hybrid_R95PCC_c %>% 
  filter(variable_type == "between") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Difference in mean (95% CI) - Adjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                  estimate, min95, max95, sig)) %>%
  select(variable, "Difference in mean (95% CI) - Adjusted model")

R95PCC_wht_c <- hybrid_R95PCC_c %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - Adjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                                estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - Adjusted model")

Rx1dayCC_btw_c <- hybrid_Rx1dayCC_c %>% 
  filter(variable_type == "between") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Difference in mean (95% CI) - Adjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                  estimate, min95, max95, sig)) %>%
  select(variable, "Difference in mean (95% CI) - Adjusted model")

Rx1dayCC_wht_c <- hybrid_Rx1dayCC_c %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - Adjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                                estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - Adjusted model")

Rx5dayCC_btw_c <- hybrid_Rx5dayCC_c %>% 
  filter(variable_type == "between") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Difference in mean (95% CI) - Adjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                  estimate, min95, max95, sig)) %>%
  select(variable, "Difference in mean (95% CI) - Adjusted model")

Rx5dayCC_wht_c <- hybrid_Rx5dayCC_c %>% 
  filter(variable_type == "within") %>% 
  mutate(sig = ifelse(p < 0.05, "*", ""),
         "Within-city difference over time (95% CI) - Adjusted model" = sprintf("%.1f (%.1f, %.1f)%s",
                                                                                estimate, min95, max95, sig)) %>%
  select(variable, "Within-city difference over time (95% CI) - Adjusted model")

R95PCC_h_c <- list(R95PCC_btw_c, R95PCC_wht_c) %>%
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

R95PCC_h_final <- R95PCC_h %>% left_join(R95PCC_h_c, by = "variable")
R95PCC_h_final$prec <- "R95p"

# sup_table 2
Rx1dayCC_sup_h <- list(Rx1dayCC_btw, Rx1dayCC_wht,
                     Rx1dayCC_btw_c, Rx1dayCC_wht_c) %>%
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
Rx1dayCC_sup_h$prec <- "RX1day"

Rx5dayCC_sup_h <- list(Rx5dayCC_btw, Rx5dayCC_wht,
                     Rx5dayCC_btw_c, Rx5dayCC_wht_c) %>%
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
Rx5dayCC_sup_h$prec <- "RX5day"

str(R95PCC_h_final)

# join for table 4
sup_table3_all <- rbind(R95PCC_h_final, Rx1dayCC_sup_h, Rx5dayCC_sup_h) %>% 
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
sup_table3 <- bind_rows(sup_table3_all, note_row4) %>% 
  rename("  " = prec)

write_xlsx(sup_table3, "Tables/sup_table3.xlsx")

#--------------------------------------------------
# Supplementary Table 4
#--------------------------------------------------
slopes_R95P <- read_csv("Model_results/L1AD/R95P/R95P_city_random_slopes.csv")
slopes_Rx1day <- read_csv("Model_results/L1AD/Rx1day/Rx1day_city_random_slopes.csv")
slopes_Rx5day <- read_csv("Model_results/L1AD/Rx5day/Rx5day_city_random_slopes.csv")
L1AD_name <- read.dbf("Data/SHP/L1AD_centroid.dbf")

# Prepare data
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

sup_table4 <- slopes_R95P %>% left_join(slopes_Rx1day, by = "SALID1") %>% 
  left_join(slopes_Rx5day, by = "SALID1") %>% 
  left_join(L1AD_name, by = "SALID1")

sup_table4_final <- sup_table4 %>% 
  select("City name", Country, R95P, Rx1day, Rx5day) %>% 
  mutate(Country = dplyr::recode(Country, "Brasil" = "Brazil")) %>%  
  rename(R95p = R95P,
         RX1day = Rx1day, 
         RX5day = Rx5day) %>% 
  arrange(Country, `City name`)

write_xlsx(sup_table4_final, "Tables/sup_table4.xlsx")
