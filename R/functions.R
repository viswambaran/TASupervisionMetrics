# functions ---------------------------------------------------------------

clean_data <- function(df, type = c("long", "wide")){
  
  type <- match.arg(type)
  
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
    mutate(value = as.numeric(value), 
           Year = as.Date(Year, format = "%Y"))
    # mutate(across(.cols = c(Year, value), as.numeric))
  
  if (type == "wide"){
    
    wide_df <- tidy_df %>% 
      pivot_wider(names_from = Variable, 
                  values_from = value)
    
    return(wide_df)
    
    
  } 
    
    
    
    
  return(tidy_df)

  
  
}

## Function to calculate changes and % change on a dataframe on all numeric columns
## Known issues where changes will be calculated on the change previous created. 
calculate_changes <- function(df){
  
  df <- df %>% 
    group_by(Firms) %>% 
    mutate(across(c(where(is.numeric), -Year), ~ . -lag(.), .names = "{col} Change"), 
           across(c(where(is.numeric), -Year), ~ . / abs(lag(.)) - 1, .names = "{col} % Change")) %>% 
    ungroup()
  
  return(df)
  
}


outlier_detector <- function(df){
  
  
  outlier_column <- function(column){
    Q1 <- quantile(column, 0.25, na.rm=TRUE)
    Q3 <- quantile(column, 0.75, na.rm=TRUE)
    IQR <- Q3 - Q1
    
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    
    return(column < lower | column > upper)
    
    
  }
  
  ## Apply function to each column in dataset where its numeric
  
  df <- df %>% 
    mutate(across(where(is.numeric), ~outlier_column(.), .names = "Outlier_{col}"))
  

  
  return(df)
}
