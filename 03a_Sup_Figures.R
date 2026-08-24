############################################################
# MS278: Descriptive precipitation indices
#
# Supplementary figures
#
############################################################

library(dplyr); library(arrow); library(RColorBrewer)
library(tidyr); library(tidyverse); library(gtsummary)
library(rlang); library(ggthemes); library(ggplot2)
library(flextable); library(purrr); library(sf)
library(ggspatial); library(RColorBrewer); library(scales)
library(ggpubr); library(xlsx); library(readxl)
library(grid); library(patchwork)

#--------------------------------------------------
# Sup Figure 1
#--------------------------------------------------
slopes_R95PCC <- read_csv("Model_results/City_center/R95PCC/R95PCC_city_random_slopes.csv")
slopes_Rx1dayCC <- read_csv("Model_results/City_center/Rx1dayCC/Rx1dayCC_city_random_slopes.csv")
slopes_Rx5dayCC <- read_csv("Model_results/City_center/Rx5dayCC/Rx5dayCC_city_random_slopes.csv")

shapefile <- st_read("Data/SHP/L1AD_centroid.shp")
shp_SALURBAL_countries <- st_read("Data/SHP/LA_countries.shp")
shp_base <- st_read("Data/SHP/LA_base.shp")

# Prepare data
slopes_R95PCC <- slopes_R95PCC %>% 
  rename(R95PCC = slope_total) %>% 
  mutate(R95PCC = round(R95PCC, 0)) %>% 
  select(R95PCC, SALID1)

slopes_Rx1dayCC <- slopes_Rx1dayCC %>% 
  rename(Rx1dayCC = slope_total) %>% 
  mutate(Rx1dayCC = round(Rx1dayCC, 0)) %>% 
  select(Rx1dayCC, SALID1)

slopes_Rx5dayCC <- slopes_Rx5dayCC %>% 
  rename(Rx5dayCC = slope_total) %>% 
  mutate(Rx5dayCC = round(Rx5dayCC, 0)) %>% 
  select(Rx5dayCC, SALID1)

dfs <- list(slopes_R95PCC, slopes_Rx1dayCC, slopes_Rx5dayCC)
slope_data <- reduce(dfs, left_join, by = "SALID1")

shp_SALURBAL_countries <- st_transform(shp_SALURBAL_countries, crs = 4326)

# joins slope and shp
data_map <- slope_data %>% left_join(shapefile, by = "SALID1")

data_sf <- st_as_sf(data_map)

# R95PCC map
rdylbu <- RColorBrewer::brewer.pal(11, "RdYlBu")

R95PCC_slope <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = data_sf, aes(fill = R95PCC), shape = 21, color = "#4d4d4d", size = 2, stroke = 0.3) +          
  scale_fill_gradientn(colours = rdylbu,
                       values = rescale(c(-85, -1, 0, 1, 144)),
                       limits = c(-85, 144),
                       oob = squish,
                       name = "R95p (mm)") +
  ggtitle("A") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 16),
        legend.position = c(0.90, 0.2), 
        legend.title = element_text(face = "bold", size = 14),
        axis.title = element_blank()) +
  annotation_custom(grob = rectGrob(x = unit(0.13, "npc"),
                                    y = unit(0.05, "npc"),
                                    width = unit(0.25, "npc"),
                                    height = unit(0.05, "npc"),
                                    gp = gpar(fill = "white", col = NA))) +
  annotation_custom(grob = grid::grobTree(grid::polylineGrob(
    x = unit(c(0.85, 0.84, 0.85, 0.86), "npc"),
    y = unit(c(0.85, 0.87, 0.85, 0.87), "npc") + unit(0.15, "cm"),
    id = c(1, 1, 2, 2),
    gp = grid::gpar(col = "#737373", lwd = 1)),
    grid::textGrob("S", x = unit(0.85, "npc"), 
                   y = unit(0.83, "npc") + unit(0.18, "cm"),
                   gp = gpar(fontsize = 6, fontface = "plain"))))+
  ggsn::scalebar(data = shp_base,
                 location = "bottomleft",
                 dist = 500,
                 dist_unit = "km",
                 transform = TRUE,
                 model = "WGS84",
                 height = 0.02,
                 st.size = 3,
                 st.dist = 0.04,
                 box.fill = c("white", "white"),
                 box.color = "gray30",
                 border.size = 0.2,
                 st.bottom = TRUE)

R95PCC_slope

# Rx1dayCC
Rx1dayCC_slope <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = data_sf, aes(fill = Rx1dayCC), shape = 21, color = "#4d4d4d", size = 2, stroke = 0.3) +          
  scale_fill_gradientn(colours = rdylbu,
                       values = rescale(c(-9, -1, 0, 1, 10)),
                       limits = c(-9, 10),
                       oob = squish,
                       name = "RX1day (mm)") +
  ggtitle("B") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 16),
        legend.position = c(0.90, 0.2), 
        legend.title = element_text(face = "bold", size = 14),
        axis.title = element_blank()) +
  annotation_custom(grob = rectGrob(x = unit(0.13, "npc"),
                                    y = unit(0.05, "npc"),
                                    width = unit(0.25, "npc"),
                                    height = unit(0.05, "npc"),
                                    gp = gpar(fill = "white", col = NA))) +
  annotation_custom(grob = grid::grobTree(grid::polylineGrob(
    x = unit(c(0.85, 0.84, 0.85, 0.86), "npc"),
    y = unit(c(0.85, 0.87, 0.85, 0.87), "npc") + unit(0.15, "cm"),
    id = c(1, 1, 2, 2),
    gp = grid::gpar(col = "#737373", lwd = 1)),
    grid::textGrob("S", x = unit(0.85, "npc"), 
                   y = unit(0.83, "npc") + unit(0.18, "cm"),
                   gp = gpar(fontsize = 6, fontface = "plain"))))+
  ggsn::scalebar(data = shp_base,
                 location = "bottomleft",
                 dist = 500,
                 dist_unit = "km",
                 transform = TRUE,
                 model = "WGS84",
                 height = 0.02,
                 st.size = 3,
                 st.dist = 0.04,
                 box.fill = c("white", "white"),
                 box.color = "gray30",
                 border.size = 0.2,
                 st.bottom = TRUE)

Rx1dayCC_slope

# Rx5dayCC
Rx5dayCC_slope <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = data_sf, aes(fill = Rx5dayCC), shape = 21, color = "#4d4d4d", size = 2, stroke = 0.3) +          
  scale_fill_gradientn(colours = rdylbu,
                       values = rescale(c(-17, -1, 0, 1, 23)),
                       limits = c(-17, 23),
                       oob = squish,
                       name = "RX5day (mm)") +
  ggtitle("C") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 16),
        legend.position = c(0.90, 0.2), 
        legend.title = element_text(face = "bold", size = 14),
        axis.title = element_blank()) +
  annotation_custom(grob = rectGrob(x = unit(0.13, "npc"),
                                    y = unit(0.05, "npc"),
                                    width = unit(0.25, "npc"),
                                    height = unit(0.05, "npc"),
                                    gp = gpar(fill = "white", col = NA))) +
  annotation_custom(grob = grid::grobTree(grid::polylineGrob(
    x = unit(c(0.85, 0.84, 0.85, 0.86), "npc"),
    y = unit(c(0.85, 0.87, 0.85, 0.87), "npc") + unit(0.15, "cm"),
    id = c(1, 1, 2, 2),
    gp = grid::gpar(col = "#737373", lwd = 1)),
    grid::textGrob("S", x = unit(0.85, "npc"), 
                   y = unit(0.83, "npc") + unit(0.18, "cm"),
                   gp = gpar(fontsize = 6, fontface = "plain"))))+
  ggsn::scalebar(data = shp_base,
                 location = "bottomleft",
                 dist = 500,
                 dist_unit = "km",
                 transform = TRUE,
                 model = "WGS84",
                 height = 0.02,
                 st.size = 3,
                 st.dist = 0.04,
                 box.fill = c("white", "white"),
                 box.color = "gray30",
                 border.size = 0.2,
                 st.bottom = TRUE)

Rx5dayCC_slope

# join maps to figure 3 
Sup_Figure1 <- (R95PCC_slope) | (Rx1dayCC_slope) | (Rx5dayCC_slope)
Sup_Figure1

ggsave("Figures/Sup_Figure_1.png", Sup_Figure1, 
       width = 20, height = 8, dpi = 700, bg = "white")

#--------------------------------------------------
# Sup Figure 2
#--------------------------------------------------
data <- read_parquet("Data/data_prec_final_wht_polar.parquet")

Sup_Figure_2a <- data %>%
  ggplot() +
  geom_jitter(aes(x = CLZ, y = R95PCC),
              color = "#9ecae1", width = 0.2, na.rm = TRUE, size = 1, height = 0.8, alpha = 0.6) +
  geom_boxplot(aes(x = CLZ, y = R95PCC),
               color = "#252525", fill = NA, linewidth = 0.6, width = 0.6, outlier.shape = NA) +
  theme_classic() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  labs(title = "A") +
  xlab("") +
  ylab("R95PCC (mm)")
Sup_Figure_2a

Sup_Figure_2b <- data %>%
  ggplot() +
  geom_jitter(aes(x = CLZ, y = Rx1dayCC),
              color = "#4292c6", width = 0.2, na.rm = TRUE, size = 1, height = 0.8, alpha = 0.6) +
  geom_boxplot(aes(x = CLZ, y = Rx1dayCC),
               color = "#252525", fill = NA, linewidth = 0.6, width = 0.6, outlier.shape = NA) +
  theme_classic() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  labs(title = "B") +
  xlab("") +
  ylab("Rx1dayCC (mm)")
Sup_Figure_2b

Sup_Figure_2c <- data %>%
  ggplot() +
  geom_jitter(aes(x = CLZ, y = Rx5dayCC),
              color = "#08519c", width = 0.2, na.rm = TRUE, size = 1, height = 0.8, alpha = 0.6) +
  geom_boxplot(aes(x = CLZ, y = Rx5dayCC),
               color = "#252525", fill = NA, linewidth = 0.6, width = 0.6, outlier.shape = NA) +
  theme_classic() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 16, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  labs(title = "C") +
  xlab("") +
  ylab("Rx5dayCC (mm)")
Sup_Figure_2c

Sup_Figure_2 <- (Sup_Figure_2a) / (Sup_Figure_2b)  / (Sup_Figure_2c)
Sup_Figure_2

ggsave("Figures/Sup_Figure_2.png", plot = Sup_Figure_2, 
       dpi = 300, width = 12, height = 16, units = "in",  bg = "white")

#--------------------------------------------------
# Sup Figure 3
#--------------------------------------------------
df_CLZ_avg <- data %>%
  group_by(CLZ, YEAR) %>%
  summarize(mean_R95PCC = mean(R95PCC, na.rm = TRUE),
            mean_Rx1dayCC = mean(Rx1dayCC, na.rm = TRUE),
            mean_Rx5dayCC = mean(Rx5dayCC, na.rm = TRUE),.groups = "drop")

Sup_Figure_3a <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = R95PCC, group = SALID1), 
            color = "#4292c6", alpha = 0.4) +
  geom_line(data = df_CLZ_avg, aes(x = YEAR, y = mean_R95PCC), 
            color = "red", linewidth = 0.8) +
  facet_wrap(~CLZ, ncol = 7) +  
  coord_cartesian(ylim = c(0, 2280)) +
  labs(title = "A",
       x = "Year",
       y = "R95PCC (mm)") +
  theme_bw(base_size = 16) +
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    panel.spacing = unit(1, "lines"))

Sup_Figure_3a

Sup_Figure_3b <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = Rx1dayCC, group = SALID1), 
            color = "#4292c6", alpha = 0.4) +
  geom_line(data = df_CLZ_avg, aes(x = YEAR, y = mean_Rx1dayCC), 
            color = "red", linewidth = 0.8) +
  facet_wrap(~CLZ, ncol = 7) +  
  coord_cartesian(ylim = c(0, 600)) +
  labs(title = "B",
       x = "Year",
       y = "Rx1dayCC (mm)") +
  theme_bw(base_size = 16) +
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    panel.spacing = unit(1, "lines"))

Sup_Figure_3b

Sup_Figure_3c <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = Rx5dayCC, group = SALID1), 
            color = "#4292c6", alpha = 0.4) +
  geom_line(data = df_CLZ_avg, aes(x = YEAR, y = mean_Rx5dayCC), 
            color = "red", linewidth = 0.8) +
  facet_wrap(~CLZ, ncol = 7) +  
  coord_cartesian(ylim = c(0, 600)) +
  labs(title = "C",
       x = "Year",
       y = "Rx5dayCC (mm)") +
  theme_bw(base_size = 16) +
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    panel.spacing = unit(1, "lines"))

Sup_Figure_3c

Sup_Figure_3 <- (Sup_Figure_3a) / (Sup_Figure_3b)  / (Sup_Figure_3c)
Sup_Figure_3

ggsave("Figures/Sup_Figure_3.png", plot = Sup_Figure_3, 
       width = 26, height = 14, dpi = 300, bg = "white")

#--------------------------------------------------
# Sup Figure 4
#--------------------------------------------------
slopes_Rx1day <- read_csv("Model_results/L1AD/Rx1day/Rx1day_city_random_slopes.csv")
slopes_Rx5day <- read_csv("Model_results/L1AD/Rx5day/Rx5day_city_random_slopes.csv")

shapefile <- st_read("Data/SHP/L1AD_centroid.shp")
shp_SALURBAL_countries <- st_read("Data/SHP/LA_countries.shp")
shp_base <- st_read("Data/SHP/LA_base.shp")

shp_SALURBAL_countries <- st_transform(shp_SALURBAL_countries, crs = 4326)

# Prepare data
slopes_Rx1day <- slopes_Rx1day %>% 
  rename(Rx1day = slope_total) %>% 
  mutate(Rx1day = round(Rx1day, 0)) %>% 
  select(Rx1day, SALID1)

slopes_Rx5day <- slopes_Rx5day %>% 
  rename(Rx5day = slope_total) %>% 
  mutate(Rx5day = round(Rx5day, 0)) %>% 
  select(Rx5day, SALID1)

dfs <- list(slopes_Rx1day, slopes_Rx5day)
slope_data <- reduce(dfs, left_join, by = "SALID1")

# joins slope and shp
data <- slope_data %>% left_join(shapefile, by = "SALID1")

data_sf <- st_as_sf(data)

#maps
rdylbu <- RColorBrewer::brewer.pal(11, "RdYlBu")

# Rx1day
Rx1day_slope <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = data_sf, aes(fill = Rx1day), shape = 21, color = "#4d4d4d", size = 2, stroke = 0.3) +          
  scale_fill_gradientn(colours = rdylbu,
                       values = rescale(c(-9, -1, 0, 1, 10)),
                       limits = c(-9, 10),
                       oob = squish,
                       name = "RX1day (mm)") +
  ggtitle("A") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 16),
        legend.position = c(0.90, 0.2), 
        legend.title = element_text(face = "bold", size = 14),
        axis.title = element_blank()) +
  annotation_custom(grob = rectGrob(x = unit(0.13, "npc"),
                                    y = unit(0.05, "npc"),
                                    width = unit(0.25, "npc"),
                                    height = unit(0.05, "npc"),
                                    gp = gpar(fill = "white", col = NA))) +
  annotation_custom(grob = grid::grobTree(grid::polylineGrob(
    x = unit(c(0.85, 0.84, 0.85, 0.86), "npc"),
    y = unit(c(0.85, 0.87, 0.85, 0.87), "npc") + unit(0.15, "cm"),
    id = c(1, 1, 2, 2),
    gp = grid::gpar(col = "#737373", lwd = 1)),
    grid::textGrob("S", x = unit(0.85, "npc"), 
                   y = unit(0.83, "npc") + unit(0.18, "cm"),
                   gp = gpar(fontsize = 6, fontface = "plain"))))+
  ggsn::scalebar(data = shp_base,
                 location = "bottomleft",
                 dist = 500,
                 dist_unit = "km",
                 transform = TRUE,
                 model = "WGS84",
                 height = 0.02,
                 st.size = 3,
                 st.dist = 0.04,
                 box.fill = c("white", "white"),
                 box.color = "gray30",
                 border.size = 0.2,
                 st.bottom = TRUE)

Rx1day_slope

# Rx5day
Rx5day_slope <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = data_sf, aes(fill = Rx5day), shape = 21, color = "#4d4d4d", size = 2, stroke = 0.3) +          
  scale_fill_gradientn(colours = rdylbu,
                       values = rescale(c(-17, -1, 0, 1, 23)),
                       limits = c(-17, 23),
                       oob = squish,
                       name = "RX5day (mm)") +
  ggtitle("B") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 16),
        legend.position = c(0.90, 0.2), 
        legend.title = element_text(face = "bold", size = 14),
        axis.title = element_blank()) +
  annotation_custom(grob = rectGrob(x = unit(0.13, "npc"),
                                    y = unit(0.05, "npc"),
                                    width = unit(0.25, "npc"),
                                    height = unit(0.05, "npc"),
                                    gp = gpar(fill = "white", col = NA))) +
  annotation_custom(grob = grid::grobTree(grid::polylineGrob(
    x = unit(c(0.85, 0.84, 0.85, 0.86), "npc"),
    y = unit(c(0.85, 0.87, 0.85, 0.87), "npc") + unit(0.15, "cm"),
    id = c(1, 1, 2, 2),
    gp = grid::gpar(col = "#737373", lwd = 1)),
    grid::textGrob("S", x = unit(0.85, "npc"), 
                   y = unit(0.83, "npc") + unit(0.18, "cm"),
                   gp = gpar(fontsize = 6, fontface = "plain"))))+
  ggsn::scalebar(data = shp_base,
                 location = "bottomleft",
                 dist = 500,
                 dist_unit = "km",
                 transform = TRUE,
                 model = "WGS84",
                 height = 0.02,
                 st.size = 3,
                 st.dist = 0.04,
                 box.fill = c("white", "white"),
                 box.color = "gray30",
                 border.size = 0.2,
                 st.bottom = TRUE)

Rx5day_slope

# join maps to figure 3 
Sup_Figure_4 <- (Rx1day_slope) | (Rx5day_slope)
Sup_Figure_4

ggsave("Figures/Sup_Figure_4.png", Sup_Figure_4, 
       width = 16, height = 8, dpi = 700, bg = "white")

#--------------------------------------------------
# Sup Figure 5
#--------------------------------------------------
pop_annual_country <- read_xlsx("pop_annual_country.xlsx")

annual_total <- pop_annual_country %>%
  summarise(
    exposure_2000    = sum(py_exposure_2000, na.rm = TRUE),
    exposure_obs     = sum(py_exposure_2000_24, na.rm = TRUE),
    .by = YEAR) %>%
  arrange(YEAR)

annual_total_long <- annual_total %>%
  pivot_longer(
    cols = c(exposure_2000, exposure_obs),
    names_to = "scenario",
    values_to = "exposure") %>%
  mutate(
    scenario = recode(scenario,
                      "exposure_2000" = "Fixed population (2000)",
                      "exposure_obs"  = "Population (2000-2024)"),
    exposure = exposure / 1e6)

sup_fig5 <- ggplot(annual_total_long,
                   aes(x = YEAR,
                       y = exposure,
                       color = scenario,
                       fill = scenario,  
                       linetype = scenario)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess",
              se = TRUE,
              span = 0.5,
              linewidth = 1.2,
              alpha = 0.2) +
  scale_color_manual(values = c("Fixed population (2000)" = "#6baed6",
                                "Population (2000-2024)" = "#08519c")) +
  
  scale_fill_manual(values = c("Fixed population (2000)" = "#6baed6",
                               "Population (2000-2024)" = "#08519c")) +
  labs(x = "Year",
       y = "Person-years (millions)",
       color = "",
       fill = "",
       linetype = "",) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.text.x = element_text(size = 28, hjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 28, color = "black"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 28),
    legend.position = "bottom",
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24))

sup_fig5

ggsave("Figures/Sup_Figure_5.png", plot = sup_fig5, 
       width = 20, height = 12, dpi = 700, bg = "white")

#--------------------------------------------------
# Sup Figure 6
#--------------------------------------------------
pop_long <- pop_annual_country %>%
  pivot_longer(cols = c(py_exposure_2000, py_exposure_2000_24),
               names_to = "scenario",
               values_to = "exposure") %>%
  mutate(scenario = recode(scenario,
                           "py_exposure_2000" = "Fixed population (2000)",
                           "py_exposure_2000_24" = "Population (2000-2024)"))

Sup_fig_6 <- ggplot(pop_long,
                    aes(x = YEAR,
                        y = exposure,
                        color = scenario,
                        fill = scenario,
                        group = scenario)) +
  geom_smooth(method = "loess",
              se = TRUE,
              span = 0.5,
              alpha = 0.15,
              linetype = 0) +
  geom_smooth(method = "loess",
              se = FALSE,
              span = 0.5,
              linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.3),
             size = 1.2,
             alpha = 0.7) +
  facet_wrap(~ Country) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6)) +
  scale_color_manual(values = c("Fixed population (2000)" = "#6baed6",
                                "Population (2000-2024)" = "#08519c")) +
  scale_fill_manual(values = c("Fixed population (2000)" = "#6baed6",
                               "Population (2000-2024)" = "#08519c")) +
  labs(y = "Person-years (millions)",
       color = "",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 28, face = "bold"),
    axis.text.x = element_text(size = 20, angle = 90, hjust = 1, vjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24))
Sup_fig_6

ggsave("Figures/Sup_Figure_6.png", plot = Sup_fig_6, 
       width = 22, height = 14, dpi = 300, bg = "white")
