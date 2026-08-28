# MS278 - Spatiotemporal trends of precipitation extremes in 353 Latin American cities

This repository contains the working code for MS278, a SALURBAL-CLIMATE study describing the spatial distribution and temporal trends of extreme precipitation, and their associations with urban characteristics across 353 Latin American cities from 2000 to 2024.  

## Script Overview  
| Script | Description |
| --- | --- |
|`00_prepare_dataset` | Reads the raw city-level files, selects the study variables and eligible cities, and exports the analytical dataset used in subsequent scripts.  |  
|`01_pop_exposed` |  Calculates the population exposed to extreme precipitation  | 
|`02_R95P` | Fits univariavate and hybrid multilevel models to estimate overall temporal trends, climate zone and urban characteristics association in R95P index and exports the results. | 
|`02a_Rx1day` | Fits univariavate and hybrid multilevel models to estimate overall temporal trends, climate zone and urban characteristics association in RX1day index and exports the results. | 
|`02b_Rx5day` | Fits univariavate and hybrid multilevel models to estimate overall temporal trends, climate zone and urban characteristics association in RX5day index and exports the results. | 
|`03_Figures` | Plot all manuscript figures |  
|`03a_Sup_Figures` | Plot all manuscript supplementary figures |  
|`04_Tables` | Generates the manuscript tables. | 
|`04a_Sup_tables` |  Generates the supplementary tables. |
|`05_sensitivity_R95P_CC` | Fits univariavate and hybrid multilevel models to estimate overall temporal trends, climate zone and urban characteristics association in R95P index (city center point) and exports the results. | 
|`05a_sensitivity_Rx1day_CC` | Fits univariavate and hybrid multilevel models to estimate overall temporal trends, climate zone and urban characteristics association in RX1day index (city center point) and exports the results. | 
|`05b_sensitivity_Rx5day_CC` | Fits univariavate and hybrid multilevel models to estimate overall temporal trends, climate zone and urban characteristics association in RX5day index (city center point) and exports the results. | 

