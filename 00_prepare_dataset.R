##################################################################
# MS278: Descriptive Precipitation indices
#
# This script prepare the final yearly datasets of precipitation, 
# population, and other variables for descriptive and modelling
# 
##################################################################

library(dplyr); library(arrow); library(readxl)
library(writexl); library(lubridate)

rm(list= ls())

data_long <- read.csv("Data/2026_03_02/data/L1AD_LONG_20260302.csv")
city_center <- read.csv("Data/2026_03_02/data/PRCANNUAL_L1CC_20251105.CSV")

glimpse(data_long)

# 1. Select variables for the study - data df
data_long <- data_long %>%
  select(ISO2,
         SALID1, 
         YEAR,
         starts_with("PRCA"),
         BECMEDNDVINWL1AD,
         BECPOPDENGUFL1AD,
         BECCZL1L1UX,
         PRJL1ADPOP,
         PRJL1ADPOP_GE65,
         GDP_PCL1AD,
         CNSMINPR_L1AD,
         BECCOASTL1AD,
         BECELEVATIONMEDIANL1AD,
         BECSLOPEMEDIANL1AD,
         INCLUDE_NOUNITCHG_L1AD,
         INCLUDE_NOBDRYCHG_L1AD)

# 2.  Create CLZ varible as names, recode countries and rename some columns
data_long <- data_long %>%
    mutate(CLZ = case_when(
      BECCZL1L1UX  == "A" ~ "Tropical",
      BECCZL1L1UX == "B" ~ "Arid",
      BECCZL1L1UX == "C" ~ "Temperate",
      BECCZL1L1UX == "D" ~ "Continental",
      BECCZL1L1UX == "E" ~ "Polar",
      TRUE ~ "Other")) %>%
   mutate(Country = case_when(
      ISO2 == "AR" ~ "Argentina",
      ISO2 == "BR" ~ "Brazil",
      ISO2 == "CL" ~ "Chile",
      ISO2 %in% c("CR", "NI", "PA", "SV", "GT") ~ "Central America",
      ISO2 == "CO" ~ "Colombia",
      ISO2 == "MX" ~ "Mexico",
      ISO2 == "PE" ~ "Peru",
      TRUE ~ ISO2)) %>% 
  rename(total_prec = PRCAPTOTL1AD,
         R95P = PRCAR95PL1AD,
         Rx1day = PRCARX1DAYL1AD,
         Rx5day = PRCARX5DAYL1AD,
         SDII = PRCASDIIL1AD,
         NDVI = BECMEDNDVINWL1AD, 
         pop_density_guf = BECPOPDENGUFL1AD,
         total_pop = PRJL1ADPOP,
         pop_over65 = PRJL1ADPOP_GE65,
         GDP = GDP_PCL1AD,
         education =   CNSMINPR_L1AD,
         coastal = BECCOASTL1AD,
         median_elevation = BECELEVATIONMEDIANL1AD,
         slope = BECSLOPEMEDIANL1AD)

# 3. select and rename variables - city center df
city_center <- city_center %>% 
  select(SALID1, 
         YEAR,
         PRCAPTOTL1CC,
         PRCAR95PL1CC,
         PRCARX1DAYL1CC,
         PRCARX5DAYL1CC) %>% 
  rename(total_precCC = PRCAPTOTL1CC,
         R95PCC = PRCAR95PL1CC,
         Rx1dayCC = PRCARX1DAYL1CC,
         Rx5dayCC = PRCARX5DAYL1CC)

# join tables
data_prec <- city_center %>% left_join(data_long, by = c("SALID1", "YEAR"))

# 5. check NA values
colSums(is.na(data_prec)) > 0

unique(data_prec$Country[is.na(data_prec$pop_over65)])
unique(data_prec$SALID1[is.na(data_prec$pop_over65)])

# drop NI 

# 6. delete the Known boundary change indicator and NI cities
# INCLUDE_NOUNITCHG_L1AD
#all cities
data_prec_new <- data_prec %>%
  group_by(SALID1) %>%
  filter(!any(INCLUDE_NOUNITCHG_L1AD == 0)) %>% 
  ungroup() %>% 
  filter(ISO2 != "NI")

length(unique(data_prec_new$SALID1))
length(unique(data_prec_new$Country))
colSums(is.na(data_prec_new)) > 0

# 7. Create a column of decade (modelling step)
# Year by decade
# center the YEAR (2000 as reference)
data_prec_new <- data_prec_new %>%
  mutate(YEAR_dec = (YEAR - 2000) / 10)

#cat
data_prec_new <- data_prec_new %>%
  mutate(YEAR_cat = cut(YEAR,
                        breaks = c(2000, 2010, 2020, 2030),
                        labels = c("2000–2009", "2010–2019", "2020–2024"),
                        right = FALSE))

# 8. scale variables (modelling step)
data_prec_new <- data_prec_new %>% 
  mutate(
    total_pop_z       = as.numeric(scale(total_pop)),
    pop_density_guf_z = as.numeric(scale(pop_density_guf)),
    pop_over65_z      = as.numeric(scale(pop_over65)),
    GDP_z             = as.numeric(scale(GDP)),
    NDVI_z            = as.numeric(scale(NDVI)),
    education_z       = as.numeric(scale(education)),
    elevation_z       = as.numeric(scale(median_elevation)),
    slope_z           = as.numeric(scale(slope)))

  
data_prec_new <- data_prec_new %>%
  group_by(SALID1) %>%
  mutate(
    across(
      .cols = c(total_pop, pop_density_guf, pop_over65, GDP, 
                education, NDVI),
      .fns = list(
        between = ~mean(.x, na.rm = TRUE),
        within  = ~.x - mean(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}")) %>%
  ungroup() %>% 
  mutate(
    total_pop_btw_z = as.numeric(scale(total_pop_between)),
    total_pop_wht_z = as.numeric(scale(total_pop_within)),
    pop_density_guf_btw_z = as.numeric(scale(pop_density_guf_between)),
    pop_density_guf_wht_z = as.numeric(scale(pop_density_guf_within)),
    pop_over65_btw_z      = as.numeric(scale(pop_over65_between)),
    pop_over65_wht_z      = as.numeric(scale(pop_over65_within)),
    GDP_btw_z             = as.numeric(scale(GDP_between)),
    GDP_wht_z             = as.numeric(scale(GDP_within)),
    education_btw_z       = as.numeric(scale(education_between)),
    education_wht_z       = as.numeric(scale(education_within)),
    NDVI_btw_z            = as.numeric(scale(NDVI_between)),
    NDVI_wht_z            = as.numeric(scale(NDVI_within)))

# 9. Add a column of pop 2000 data only (year 2000 as constant) for each SALID1
data_prec_new <- data_prec_new %>%
  group_by(SALID1) %>%
  mutate(pop_2000 = total_pop[YEAR == 2000][1]) %>%
  ungroup()

# 10. Calculate annual number of days above the 95 percentile for pop exposed
data_daily <- read.csv("Data/GSMaP_L1_1998_2024.csv")

data_daily <- data_daily %>%
  select(SALID1, date, prec_L1AD) %>% 
  mutate(date = as.Date(date), year = year(date))

prec_95p <- data_daily %>%
  filter(prec_L1AD >= 1, year >= 2000, year <= 2024) %>%
  group_by(SALID1) %>%
  mutate(p95 = quantile(prec_L1AD, 0.95, na.rm = TRUE)) %>%
  mutate(extreme = ifelse(prec_L1AD > p95, 1, 0)) %>%
  ungroup()

annual_pre95p <- prec_95p %>% 
  group_by(SALID1, year) %>%
  summarise(n_extreme_days = sum(extreme, na.rm = TRUE),.groups = "drop")  
  
# 11. join 
data_final <- data_prec_new %>% 
  left_join(annual_pre95p, by = c("SALID1" = "SALID1", "YEAR" = "year"))

#12. remove city with polar climate
data_final_wp <- data_final %>%
  filter(CLZ != "Polar")

data_final_wp %>%
  group_by(CLZ) %>%
  summarise(n_L1AD = n_distinct(SALID1))

length(unique(data_final_wp$SALID1))

# export final dataset parquet
write_parquet(data_final_wp, "Data/data_prec_final_wht_polar.parquet")

# export excel for stata
write_xlsx(data_final_wp, "Data/data_prec_final_wht_polar.xlsx")

