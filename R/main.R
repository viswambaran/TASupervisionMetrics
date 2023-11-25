options(scipen = 999)
# Packages ----------------------------------------------------------------

library(dplyr)
library(readxl)
library(purrr)
library(tidyr)
library(janitor)


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

General <- clean_data(SupervisionMetrics$`Dataset 1 - General`)
Underwriting <- clean_data(SupervisionMetrics$`Dataset 2 - Underwriting`)









