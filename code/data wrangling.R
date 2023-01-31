
# Libraries

pacman::p_load("tidyverse", "devtools", "rugarch", "rmgarch", "zoo", "factoextra",
               "forecast", "tbl2xts", "lubridate", "PerformanceAnalytics",
               "ggthemes", "dplyr", "cowplot", "fmxdat", "glue","MTS",
               "robustbase","tidyr")


# Import Data

bonds <- read_rds("data/SA_Bonds.rds")
datacheck <- fmxdat::SA_Indexes
gold <- read_rds("data/comms.rds")
bitcion <- fmxdat::cryptos

# Calculate Returns for

# 1. All share index
# 2. property index
# 3. Gold
# 4. bitcion?









