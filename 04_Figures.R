############################################################
# MS278: Descriptive precipitation indices - L1AD level
#
# Figures
#
############################################################

library(dplyr); library(arrow); library(RColorBrewer)
library(tidyr); library(tidyverse); library(gtsummary)
library(rlang); library(ggthemes); library(ggplot2)
library(flextable); library(purrr); library(sf)
library(ggspatial); library(RColorBrewer); library(scales)
library(ggpubr); library(xlsx); library(readxl)
library(grid); library(patchwork)

dir.create("Figures", recursive = TRUE, showWarnings = FALSE)

data <- read_parquet("Data/data_prec_final_wht_polar.parquet")

#--------------------------------------------------
# Figure 1
#--------------------------------------------------
set.seed(1234)

Figure_1a <- data %>%
  ggplot() +
  geom_jitter(aes(x = Country, y = R95P),
              color = "#9ecae1", width = 0.2, na.rm = TRUE, size = 1, height = 0.8, alpha = 0.6) +
  geom_boxplot(aes(x = Country, y = R95P),
               color = "#252525", fill = NA, linewidth = 0.4, width = 0.6, outlier.shape = NA) +
  theme_classic() +
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(size = 18, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  labs(title = "A") +
  xlab("") +
  ylab("R95p (mm)")
Figure_1a

Figure_1b <- data %>%
  ggplot() +
  geom_jitter(aes(x = Country, y = Rx1day),
              color = "#4292c6", width = 0.2, na.rm = TRUE, size = 1, height = 0.8, alpha = 0.6) +
  geom_boxplot(aes(x = Country, y = Rx1day),
               color = "#252525", fill = NA, linewidth = 0.4, width = 0.6, outlier.shape = NA) +
  theme_classic() +
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(size = 18, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  labs(title = "B") +
  xlab("") +
  ylab("RX1day (mm)")
Figure_1b

Figure_1c <- data %>%
  ggplot() +
  geom_jitter(aes(x = Country, y = Rx5day),
              color = "#08519c", width = 0.2, na.rm = TRUE, size = 1, height = 0.8, alpha = 0.6) +
  geom_boxplot(aes(x = Country, y = Rx5day),
               color = "#252525", fill = NA, linewidth = 0.4, width = 0.6, outlier.shape = NA) +
  theme_classic() +
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(size = 18, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  labs(title = "C") +
  xlab("") +
  ylab("RX5day (mm)")
Figure_1c

Figure_1 <- (Figure_1a) / (Figure_1b)  / (Figure_1c)
Figure_1

ggsave("Figures/Figure_1.png", plot = Figure_1, 
       dpi = 300, width = 12, height = 16, units = "in",  bg = "white")

#--------------------------------------------------
# Figure 2
#--------------------------------------------------
df_Country_avg <- data %>%
  group_by(Country, YEAR) %>%
  summarize(mean_R95p = mean(R95P, na.rm = TRUE),
            mean_Rx1day = mean(Rx1day, na.rm = TRUE),
            mean_Rx5day = mean(Rx5day, na.rm = TRUE),.groups = "drop")

Figure_2a <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = R95P, group = SALID1), 
            color = "#4292c6", alpha = 0.4) +
  geom_line(data = df_Country_avg, aes(x = YEAR, y = mean_R95p), 
            color = "red", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 2280)) +
  labs(title = "A",
       x = "Year",
       y = "R95p (mm)") +
  theme_bw(base_size = 16) +
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.spacing = unit(1, "lines"))

Figure_2a

Figure_2b <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = Rx1day, group = SALID1), 
            color = "#4292c6", alpha = 0.4) +
  geom_line(data = df_Country_avg, aes(x = YEAR, y = mean_Rx1day), 
            color = "red", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 600)) +
  labs(title = "B",
       x = "Year",
       y = "RX1day (mm)") +
  theme_bw(base_size = 16) +
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.spacing = unit(1, "lines"))

Figure_2b

Figure_2c <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = Rx5day, group = SALID1), 
            color = "#4292c6", alpha = 0.4) +
  geom_line(data = df_Country_avg, aes(x = YEAR, y = mean_Rx5day), 
            color = "red", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 600)) +
  labs(title = "C",
       x = "Year",
       y = "RX5day (mm)") +
  theme_bw(base_size = 16) +
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.spacing = unit(1, "lines"))

Figure_2c

Figure_2 <- (Figure_2a) / (Figure_2b)  / (Figure_2c)
Figure_2

ggsave("Figures/Figure_2.png", plot = Figure_2, 
       width = 26, height = 14, dpi = 300, bg = "white")

#--------------------------------------------------
# Figure 3
#--------------------------------------------------
slopes_R95P <- read_csv("Model_results/L1AD/R95P/R95P_city_random_slopes.csv")

shapefile <- st_read("Data/SHP/L1AD_centroid.shp")
shp_SALURBAL_countries <- st_read("Data/SHP/LA_countries.shp")
shp_base <- st_read("Data/SHP/LA_base.shp")

shp_SALURBAL_countries <- st_transform(shp_SALURBAL_countries, crs = 4326)

# Prepare data
slopes_R95P <- slopes_R95P %>% 
  rename(R95P = slope_total) %>% 
  mutate(R95P = round(R95P, 0)) %>% 
  select(R95P, SALID1)

# joins slope and shp
data <- slopes_R95P %>% left_join(shapefile, by = "SALID1")

data_sf <- st_as_sf(data)

# R95p map
rdylbu <- RColorBrewer::brewer.pal(11, "RdYlBu")

Figure_3 <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = data_sf, aes(fill = R95P), shape = 21, color = "#4d4d4d", size = 2, stroke = 0.3) +          
  scale_fill_gradientn(colours = rdylbu,
                       values = rescale(c(-85, -1, 0, 1, 144)),
                       limits = c(-85, 144),
                       oob = squish,
                       name = "R95p (mm)") +
  ggtitle(" ") +
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

Figure_3

ggsave("Figures/Figure_3.png", Figure_3, 
       width = 8, height = 8, dpi = 700, bg = "white")
