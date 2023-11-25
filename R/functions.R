# functions ---------------------------------------------------------------

clean_data <- function(df){
  
  # Extract years from df - both sheets were in row 2
  
  years <- as.character(df[2, ]) %>%
    na.omit() %>% 
    stringr::str_extract(., pattern = "[0-9]+")
  
  
  # Extracting Variables from first row
  Variables <- as.character(df[1,]) %>% 
    na.omit()
  
  # Combining Years and Variables to create unique column name
  new_colnames <- paste(Variables, years, sep = ".")
  
  # Creating tidy dataframe 
  ## Renaming dataframe with unique column names created earlier 
  ## Removing first two rows containing years and variables 
  ## Pivot longer to reshape dataframe into tidy data format 
  ## separating out columns into respective variable name and year per firm 
  ## Converting numerically represented character columns into numerics 
  
  tidy_df <- df %>% 
    rename_with(~c(colnames(.)[1], new_colnames), -1) %>% 
    rename(Firms = `...1`) %>% 
    slice(-1:-2) %>% 
    pivot_longer(cols = -Firms, 
                 names_to = c("Variable", "Year"),
                 names_pattern = "(.*)\\.(\\d+)") %>% 
    mutate(across(.cols = c(Year, value), as.numeric))
  
  return(tidy_df)
  
  
}