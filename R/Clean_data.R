library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Load your data (assuming it's stored in a CSV or similar file)
data <- read.csv("C:/Users/DannahAdesanya/Downloads/fusion_GLOBAL_DATAFLOW_UNICEF_1.0_all (1).csv", stringsAsFactors = FALSE)
# Step 1: Clean and standardize column names
colnames(data) <- c("DATAFLOW", "REF_AREA", "INDICATOR", "SEX", "TIME_PERIOD", "OBS_VALUE", 
                    "UNIT_MULTIPLIER", "UNIT_MEASURE", "OBS_STATUS", "OBS_CONF", 
                    "LOWER_BOUND", "UPPER_BOUND", "WGTD_SAMPL_SIZE", "OBS_FOOTNOTE", 
                    "SERIES_FOOTNOTE", "DATA_SOURCE", "SOURCE_LINK", "CUSTODIAN", 
                    "TIME_PERIOD_METHOD", "REF_PERIOD", "COVERAGE_TIME", "AGE")

# Step 2: Clean text columns and remove special characters
data <- data %>% 
  mutate(across(everything(), ~str_replace_all(., "Ã©", "é")))

# Step 3: Split 'DATAFLOW' column into 'Provider' and 'Category'
data <- data %>% 
  separate(DATAFLOW, into = c("Provider", "Category"), sep = ": ", extra = "merge", fill = "right")

# Step 4: Convert 'TIME_PERIOD' to Date format
data$TIME_PERIOD <- dmy(data$TIME_PERIOD)

# Step 5: Convert numerical columns to numeric data type
data <- data %>% 
  mutate(OBS_VALUE = as.numeric(OBS_VALUE),
         LOWER_BOUND = as.numeric(LOWER_BOUND),
         UPPER_BOUND = as.numeric(UPPER_BOUND),
         WGTD_SAMPL_SIZE = as.numeric(WGTD_SAMPL_SIZE))

# Step 6: Handle missing values or replace with appropriate indicators (e.g., NA for missing values)
data[data == ""] <- NA

# Step 7: Confirm column classes and the overall structure
str(data)

# Now, the data is cleaned and standardized for analysis
