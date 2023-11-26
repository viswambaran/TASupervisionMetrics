options(scipen = 999)
# Packages ----------------------------------------------------------------

library(dplyr)
library(readxl)
library(purrr)
library(tidyr)
library(janitor)
library(ggplot2)
library(boeCharts)
library(lubridate)
library(kableExtra)


# Sourcing utils/functions ------------------------------------------------

source("R/functions.R")


# Reading Data and cleaning -----------------------------------------------


path <- "data/DataScientist_009749_Dataset.xlsx"

# Read in all sheets 

SupervisionMetrics <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path, col_names = FALSE)

# Clean dataset to tidy data format 

General_long <- clean_data(SupervisionMetrics$`Dataset 1 - General`, type = "long")
Underwriting_long <- clean_data(SupervisionMetrics$`Dataset 2 - Underwriting`, type = "long")

General_wide <- clean_data(SupervisionMetrics$`Dataset 1 - General`, type = "wide")
Underwriting_wide <- clean_data(SupervisionMetrics$`Dataset 2 - Underwriting`, type = "wide")


# Combine general and underwriting 

combined_df_long <- bind_rows(lst(General_long, Underwriting_long), .id = "Origin")
combined_df_wide <- General_wide %>% 
  full_join(Underwriting_wide, by = c("Firms", "Year"))


# Supplementary tables 

# Changes (nominal and percent)

General_changes <- General_wide %>% 
  group_by(Firms) %>% 
  mutate(across(c(where(is.numeric), -Year), ~ . -lag(.), .names = "{col} Change"), 
         across(c(where(is.numeric), -Year), ~. / lag(.) - 1, .names = "{col} % Change"))


Underwriting_changes <- Underwriting_wide %>% 
  group_by(Firms) %>% 
  mutate(across(c(where(is.numeric), -Year), ~ . -lag(.), .names = "{col} Change"), 
         across(c(where(is.numeric), -Year), ~. / lag(.) - 1, .names = "{col} % Change"))


combined_changes <- combined_df_wide %>% 
  group_by(Firms) %>% 
  mutate(across(c(where(is.numeric), -Year), ~ . -lag(.), .names = "{col} Change"), 
         across(c(where(is.numeric), -Year), ~. / lag(.) - 1, .names = "{col} % Change"))


# Condensed tables with main metrics 

condensed_table <- combined_df_wide %>% 
  select(Firms, Year, matches("GWP|NWP|SCR|Gross claims incurred|net combined"))

condensed_table_changes <- calculate_changes(condensed_table)

# Long format for charting
condensed_table_changes_long <- condensed_table_changes %>% 
  pivot_longer(-c(Firms, Year), names_to = "Metric")

# latestYearGWPTable <- condensed_table_changes %>% 
#   filter(Year == max(Year)) %>% 
#   slice_max(`GWP (£m)`, n = 10)
# 
# top10GWP <- condensed_table_changes %>% 
#   group_by(Firms) %>% 
#   slice_max(order_by = `GWP (£m)`, n = 10)


# Visualisations ----------------------------------------------------------








