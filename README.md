# MS278 - Spatiotemporal trends of precipitation extremes in 353 Latin American cities

This repository contains the working code for MS278, a SALURBAL-CLIMATE study describing the spatial distribution and temporal trends of extreme precipitation, and their associations with urban characteristics across 353 Latin American cities from 2000 to 2024.  

## Prerequisites  
  
The analysis requires:  
- R [version 4.5.2]  
- Stata [version 17.0]  
- Stata package `parmest`, installed from SSC: `ssc install parmest`  
  
The R scripts require the following packages:
```text
arrow, dplyr, foreign, flextable, ggplot2, ggpubr, ggspatial, ggthemes, glue, gtsummary, lubridate, patchwork, purrr, RColorBrewer, readr, readxl, rlang, scales, sf, tidyr, tidyverse, writexl, xlsx
```

The analysis was developed and tested using the software versions specified above.  

## Required directories  
Before running the scripts, ensure that the following directories exist:  
  
- `Data/`  
- `Model_results/L1AD/R95P/`
- `Model_results/L1AD/Rx1day/`
- `Model_results/L1AD/Rx5day/`
- `Model_results/City_center/R95PCC/`
- `Model_results/City_center/Rx1dayCC/`
- `Model_results/City_center/Rx5dayCC/`
- `Figures/`  
- `Tables/`  
  

## Script Overview  
| Script | Description |
| --- | --- |
|`00_prepare_dataset` | Reads the raw city-level files, selects the study variables and eligible cities, and exports the analytical dataset used in subsequent scripts.  |  
|`01_pop_exposed` |  Calculates the population exposed to extreme precipitation  | 
|`02_R95P` | Fits univariavate and hybrid multilevel models to estimate overall temporal trends, climate zone and urban characteristics association in R95P index and exports the results. | 
|`02a_Rx1day` | Fits univariavate and hybrid multilevel models to estimate overall temporal trends, climate zone and urban characteristics association in RX1day index and exports the results. | 
|`02b_Rx5day` | Fits univariavate and hybrid multilevel models to estimate overall temporal trends, climate zone and urban characteristics association in RX5day index and exports the results. | 
|`03_sensitivity_R95P_CC` | Fits univariavate and hybrid multilevel models to estimate overall temporal trends, climate zone and urban characteristics association in R95P index (city center point) and exports the results. | 
|`03a_sensitivity_Rx1day_CC` | Fits univariavate and hybrid multilevel models to estimate overall temporal trends, climate zone and urban characteristics association in RX1day index (city center point) and exports the results. | 
|`03b_sensitivity_Rx5day_CC` | Fits univariavate and hybrid multilevel models to estimate overall temporal trends, climate zone and urban characteristics association in RX5day index (city center point) and exports the results. | 
|`04_Figures` | Plot all manuscript figures |  
|`04a_Sup_Figures` | Plot all manuscript supplementary figures |  
|`05_Tables` | Generates the manuscript tables. | 
|`05a_Sup_tables` |  Generates the supplementary tables. |


