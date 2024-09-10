library(readxl)
library(lubridate)
library(stringr)
library(dplyr)

# load the data
temp_libs = read_excel("~//Documents/Projects/Git_repositories/strain_db/libs_done_vd/input_tables/NGS_libs_tobe-libs_done.v20.xlsx", col_types = "text")   

# Convert as data.frame
temp_libs = as.data.frame(temp_libs[, -c(41:50)])

# Remove all the empty rows
temp_libs = temp_libs[!rowSums(is.na(temp_libs)) == ncol(temp_libs), ]

# IDs to keep
temp_col_ids = c("borstel_ID", "study", "source", "date_of_isolation", "organism", "owner", "num", "isolation_date")

# Subset the table to keep only those IDs
temp_libs = temp_libs[, temp_col_ids]

# # Issue 6: Correct dates with missing years where possible
# temp_source_index = which(grepl("^\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}$", temp_libs$source))
# 
# temp_mis_soure = temp_libs[temp_source_index, ]
# 
# class(temp_mis_soure[temp_source_index[1], , drop = FALSE])
# temp_mis_soure[1, c(1:(grep(pattern = "num", colnames(temp_mis_soure)) - 1))] <- c(NA, as.character(temp_mis_soure[1, c(1:(grep(pattern = "num", colnames(temp_mis_soure)) - 1))]))[c(1:(grep(pattern = "num", colnames(temp_mis_soure)) - 1))]

# Check if the 'source' column has date entries
if (sum(grepl("^\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}$", temp_libs$source)) != 0) {
  
  # Get the indices of the rows with wrongly added date information
  temp_missing_index <- which(grepl("^\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}$", temp_libs$source))
  
  # Determine the column indices that should be modified
  temp_col_index <- 1:(grep(pattern = "num", colnames(temp_libs)) - 1)
  
  # Assign modified values to the specified rows and columns
  temp_libs[temp_missing_index, temp_col_index] <- t(apply(temp_libs[temp_missing_index, temp_col_index], 1, FUN = function(y) {
    # Modify the values, assuming setting to NA and keeping characters as is
    temp_data <- c(NA, as.character(y))
    temp_data[temp_col_index] # Return modified values
  }))
  
  # Assign the isolation date to the working date column
  for (i in c(1:length(temp_missing_index))){
    temp_libs[temp_missing_index[i], grep(pattern = "isolation_date", colnames(temp_libs))] <- temp_libs[temp_missing_index[i], "date_of_isolation"]
  }
}

# Issue organism: Correct dates with missing years where possible
temp_org_index = which(grepl("^\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}$", temp_libs$organism))

temp_mis_soure = temp_libs[temp_source_index, ]

class(temp_mis_soure[temp_source_index[1], , drop = FALSE])
temp_mis_soure[1, c(1:(grep(pattern = "num", colnames(temp_mis_soure)) - 1))] <- c(NA, as.character(temp_mis_soure[1, c(1:(grep(pattern = "num", colnames(temp_mis_soure)) - 1))]))[c(1:(grep(pattern = "num", colnames(temp_mis_soure)) - 1))]









# fix the dates
temp_dates = temp_libs["date_of_isolation"]

# How are the dates added to the table
temp_dates_track = data.frame(table(temp_dates$date_of_isolation), stringsAsFactors = FALSE)

# How are the dates added to the table
temp_dates_tab = data.frame(table(temp_libs$isolation_date), stringsAsFactors = FALSE)

# Assign column names to the df
colnames(temp_dates_tab) <- c("date", "times_appeared")

# How are the dates added to the table - convert them as character
temp_dates_tab <- as.data.frame(apply(temp_dates_tab, c(1, 2), as.character))

# Issue 1: Correct dates from 1900-1924 to 2000-2024
temp_dates_tab$temp_date <- sapply(temp_dates_tab$date, FUN = function(y){
  # Check if date matches the pattern "*.*.*"
  if (grepl("(\\d{1,2}\\.\\d{1,2}\\.)1(9\\d{2})$", y)) {
    # Apply the correction for missing digits
    date <- str_replace(y, "(\\d{1,2}\\.\\d{1,2}\\.)1(9\\d{2})$", function(x) {
      year <- as.numeric(str_extract(x, "\\d{4}$"))
      if (year >= 1900 && year <= 1924) {
        new_year <- year + 100  # Transform 1900-1924 to 2000-2024
        return(str_replace(x, "\\d{4}$", as.character(new_year)))
      }
      else {
        return(x)  # Return unchanged if not matching specific pattern
      }
    })
  } else {
    return(y)
  }
}
)

# Convert to character
temp_dates_tab$temp_date = as.character(temp_dates_tab$temp_date)

#### Fix
# Issue 2: Correct years above 2024 based on matching studies
temp_dates_tab$temp_date <- sapply(temp_dates_tab$temp_date, FUN = function(y){
  
  # Check if date matches the pattern "*.*.*"
  if (grepl("(\\d{1,2}\\.\\d{1,2}\\.)(20\\d{2})$", y) & (as.numeric(str_extract(y, "\\d{4}$") > 2024)) == TRUE) {
    
    # Extract the year and check if it's above 2024
    year <- as.numeric(str_extract(y, "\\d{4}$"))
    
    # Get the same studies
    study_name = temp_libs[match(y, temp_libs$isolation_date), "study"]
    
    # Extract the last digit of the year
    last_digit <- substr(year, 4, 4)
    
    # Find all entries for the same study
    study_dates <- data %>%
      filter(study == study_name)
    
    # Find matching dates with the same last digit in the year
    matching_dates <- study_dates %>%
      filter(str_detect(isolation_date, paste0("\\d{1,2}\\.\\d{1,2}\\.(20\\d", last_digit, ")$")))
    
    # If a match is found, replace the year, otherwise keep as is or set to NA
    if (nrow(matching_dates) > 0) {
      # Extract a correct year from matching rows, prioritize the first match
      correct_year <- as.numeric(str_extract(matching_dates$isolation_date[1], "\\d{4}$"))
      
      # Replace the incorrect year with the correct one
      corrected_date <- str_replace(date, "\\d{4}$", as.character(correct_year))
      
      return(corrected_date)
    } else {
      # No match found, return NA or the original date
      return(NA)  # or use `return(date)` if you prefer to keep the original
    }
  } else {
    return(y)
  }
}
)

# date
y = "28.02.2047"

# condition to match
grepl("(\\d{1,2}\\.\\d{1,2}\\.)(20\\d{2})$", y) & (as.numeric(str_extract(y, "\\d{4}$") > 2024)) == TRUE

# match what we are looking for
match(y, temp_libs$isolation_date)

# Extract the year and check if it's above 2024
year <- as.numeric(str_extract(y, "\\d{4}$"))

# Get the same studies
study_name = temp_libs[match(y, temp_libs$isolation_date), "study"]

# Extract the last digit of the year
last_digit <- substr(year, 4, 4)

# Find all entries for the same study
study_dates <- temp_libs %>%
  filter(study == study_name)

# Subset the study dates to remove all the wrongly added year
study_dates = study_dates[!str_detect(string = study_dates$isolation_date, pattern = as.character(year)), ]

# check if the last digit is present in any of the isolation date
all(grepl(pattern = paste0("\\d{1,2}\\.\\d{1,2}\\.(20\\d", last_digit, ")$"), x = study_dates$isolation_date)) == FALSE

# get the second last digit
sec_last_digit <- substr(year, 3, 3)

#
str_detect(study_dates$isolation_date, paste0("\\d{1,2}\\.\\d{1,2}\\.(20\\d", sec_last_digit, ")$"))

# Find matching dates with the same last digit in the year
matching_dates <- study_dates %>%
  filter(str_detect(isolation_date, paste0("\\d{1,2}\\.\\d{1,2}\\.(20\\d", last_digit, ")$")))




# target_year <- as.numeric(paste0("20", substr(year, 3, 4)))
# 
# study_name = temp_libs[match(y, temp_libs$isolation_date), "study"]
# 
# filter(temp_libs, grepl(temp_libs[match(y, temp_libs$isolation_date), "study"], study))
# 
# temp_libs %>% filter(grepl(study_name, study))
# 
# study_dates <- temp_libs %>%
#   filter(study == study_name)
# 
# # Find matching dates with the same last digit in the year
# matching_dates <- study_dates %>%
#   filter(str_detect(isolation_date, paste0("\\d{1,2}\\.\\d{1,2}\\.(20\\d", last_digit, ")$")))
# 
# # If a match is found, replace the year, otherwise keep as is or set to NA
# if (nrow(matching_dates) > 0) {
#   
#   # Extract a correct year from matching rows, prioritize the first match
#   correct_year <- as.numeric(str_extract(matching_dates$isolation_date[1], "\\d{4}$"))
#   
#   # Replace the incorrect year with the correct one
#   corrected_date <- str_replace(date, "\\d{4}$", as.character(correct_year))
#   
#   return(corrected_date)
# }
# 
# # Extract the year and check if it's above 2024
# year <- as.numeric(str_extract(y, "\\d{4}$"))
# 
# if (year > 2024) {
#   # Extract the last digit of the year
#   last_digit <- substr(year, 4, 4)
#   
#   # Find all entries for the same study
#   study_dates <- data %>%
#     filter(study == study_name)
#   
#   # Find matching dates with the same last digit in the year
#   matching_dates <- study_dates %>%
#     filter(str_detect(isolation_date, paste0("\\d{1,2}\\.\\d{1,2}\\.(20\\d", last_digit, ")$")))
#   
#   # If a match is found, replace the year, otherwise keep as is or set to NA
#   if (nrow(matching_dates) > 0) {
#     # Extract a correct year from matching rows, prioritize the first match
#     correct_year <- as.numeric(str_extract(matching_dates$isolation_date[1], "\\d{4}$"))
#     
#     # Replace the incorrect year with the correct one
#     corrected_date <- str_replace(date, "\\d{4}$", as.character(correct_year))
#     
#     return(corrected_date)
#   } else {
#     # No match found, return NA or the original date
#     return(NA)  # or use `return(date)` if you prefer to keep the original
#   }
# } else {
#   # If year is not greater than 2024, return the date as is
#   return(date)
# }









date <- str_replace(date, "(\\d{1,2}\\.\\d{1,2}\\.)(20\\d{2})$", function(x) {
  year <- as.numeric(str_extract(x, "\\d{4}$"))
  if (year > 2024) {
    # Extract last two digits of the year to match with similar entries within study
    target_year <- as.numeric(paste0("20", substr(year, 3, 4)))
    # Find matching study with same last two digits in year within valid range
    matching_row <- data %>%
      filter(study == study & str_detect(DateColumn, paste0("\\d{1,2}\\.\\d{1,2}\\.", target_year))) %>%
      slice(1)  # Get the first match if any
    
    if (nrow(matching_row) > 0) {
      matched_date <- matching_row$DateColumn[1]
      matched_year <- as.numeric(str_extract(matched_date, "\\d{4}$"))
      return(str_replace(x, "\\d{4}$", as.character(matched_year)))
    } else {
      return(NA)  # No match found
    }
  }
  return(x)  # No change if year <= 2024
})






# Issue 3: Correct dates with missing digits
temp_dates_tab$temp_date <- sapply(temp_dates_tab$temp_date, FUN = function(y){
  # Check if date matches the pattern "*.*.*"
  if (grepl("^\\d{1,2}\\.\\d{1,2}\\.\\d{3,3}$", y)) {
    # Apply the correction for missing digits
    date <- str_replace(y, "(\\d{1,2}\\.\\d{1,2}\\.)(\\d{3,3})$", function(x) {
      year <- str_extract(x, "\\d{3,3}$")
      if (nchar(year) == 3 && substr(year, 1, 1) == "2") {  # Year has 3 digits and starts with '2'
        corrected_year <- paste0("20", substr(year, 2, 3))  # Add missing '20'
        return(str_replace(x, "\\d{3}$", corrected_year))
      }
    })
  } else {
    return(y)
    }
  }
  )

# Using str_replace to remove the trailing dot for any pattern "*.*.*."
temp_dates_tab$temp_date <- sapply(temp_dates_tab$temp_date, FUN = function(y) {
  # General pattern to remove the trailing dot without specifying digit counts
  corrected_date <- str_replace(y, "(.*\\..*\\..*)\\.$", "\\1")  # Matches any text ending in ".*.*.*." and removes the trailing dot
  return(corrected_date)
})

# Clean the dates
temp_dates_tab$temp = temp_dates_tab$temp_date

# Issue 1: Remove time after the space
temp_dates_tab$temp <- str_replace(temp_dates_tab$temp, "\\s.*$", "")  # Removes everything after a space

# Issue 2: Remove anything after a dash
temp_dates_tab$temp <- str_replace(temp_dates_tab$temp, "-.*$", "")    # Removes everything after a dash

# Rearrange the dates into particular format
temp_dates_tab$nice_dates = parse_date_time(temp_dates_tab$temp, orders = c("mdy", "dmy", "ymd"))

# Create year data
temp_dates_tab$years = str_replace(temp_dates_tab$nice_dates, "-.*$", "")    # Removes everything after a dash

### 2nd part

# Rows containing "NA" character
which(temp_libs$isolation_date %in% "NA")

# Rows containing "NA" character
temp_empty_years = temp_dates_tab[which(temp_dates_tab$years %in% NA), ]

temp_missing_index = which(temp_dates_tab$years %in% NA)

# Formats to look for 
"22.22.2022" or "2001"

# Which years are present in the book
table(temp_dates_tab$years)

# Issue 3: Correct dates with missing digits
temp_dates_tab$years[temp_missing_index] <- sapply(temp_dates_tab$temp_date[temp_missing_index], FUN = function(y){
  
  # Check if date matches the pattern "*.*.*"
  if (grepl("^\\d{1,2}\\.\\d{1,2}\\.\\d{4,4}$", y)) {
    
    # Extract the year and check if it's above 2024
    year <- as.numeric(str_extract(y, "\\d{4}$"))
    
    # return the year
    return(year)
  }
  
  else if (grepl("^\\d{4,4}$", y) & (y %in% c(1995:2024))) {
    # return the year
    return(y)
  }
  
  else {
    return(NA)
  }
}
)



y = 2001

## 

bb = temp_libs[which(temp_libs$date_of_isolation %in% "B10"), ]





