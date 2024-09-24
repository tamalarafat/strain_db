# load the libs
source("~/Documents/Projects/git_projects/strain_db/functions/loadRdata.R")

library(stringdist)
library(readxl)
library(lubridate)
library(stringr)
library(dplyr)

# load the file containing missing libraries
all_libs = loadRData("~/Documents/Projects/git_projects/strain_db/output_files/01_date_corrected/all_libs.RData")


# Transform all the NA to "-"
all_libs$study[which(is.na(all_libs$study))] <- "-"

# How are the dates added to the table
temp_study_tab = data.frame(table(all_libs$study), stringsAsFactors = FALSE)

# Assign column names
colnames(temp_study_tab) <- c("ID", "count")

# duplicate the column information
temp_study_tab$names = toupper(temp_study_tab$ID)

# How are the dates added to the table - convert them as character
temp_study_tab <- as.data.frame(apply(temp_study_tab, c(1, 2), as.character))


## Filter

# what are those digits doing in the name section
temp_digit_check_index = which(grepl(pattern = str_c("^\\d{1,4}$", "^-$", sep = "|"), x = temp_study_tab$names))

# Data containing only missing values
temp_study_missing = temp_study_tab[temp_digit_check_index, ]

# Data containing only missing values
temp_study_fix = temp_study_tab[-temp_digit_check_index, ]

# Remove all the double space
temp_study_fix$names <- str_replace_all(temp_study_fix$names, "\\s{2,}", " ")

# Add missing space after a period
temp_study_fix$names <- str_replace_all(temp_study_fix$names, "(\\w)\\.(\\w)", "\\1. \\2")

# Replace special characters with '-'
temp_study_fix$names <- str_replace_all(temp_study_fix$names, "[_,/]", "-")

# Add missing space after a period
temp_study_fix$names <- str_replace_all(temp_study_fix$names, "\\s*-\\s*", "-")

# Remove leading and trailing spaces
temp_study_fix$names <- str_trim(temp_study_fix$names)

# Remove trailing dots
temp_study_fix$correction <- temp_study_fix$names

# Remove dot at the end of string except for SP. or SPP.
temp_study_fix$correction <- gsub("(?<!SP|SPP)\\.$", "", temp_study_fix$correction, perl = TRUE)

# Apply the function to the 'names' column
temp_study_fix$corrected_names <- sapply(temp_study_fix$correction, replace_and_reformat, study_scientific_name_map)

# Extract all unique scientific names from the study_scientific_name_map
sci_names <- unique(unname(unlist(study_scientific_name_map)))

# Apply this function to the 'corrected_names' column after running the initial reformatting
temp_study_fix$temp_sci <- sapply(temp_study_fix$corrected_names, add_hyphen_after_sci_name, sci_names)

# Apply the replacement function to the column
temp_study_fix$temp_country <- replace_with_map(temp_study_fix$temp_sci, country_map)




# Get unique entries
unique_entries <- unique(temp_study_fix$temp1)

# Create a distance matrix based on string similarity
dist_matrix <- stringdistmatrix(unique_entries, unique_entries, method = "jw")

# Set a similarity threshold (adjust based on inspection of results)
threshold <- 0.3

# Group similar entries based on the distance matrix
groupings <- hclust(as.dist(dist_matrix)) %>%
  cutree(h = threshold)

names_in_group <- unique_entries[groupings == 1]

# From the grouping we can get similar entries
study_table = as.data.frame(sort(table(groupings), decreasing = TRUE), stringsAsFactors = FALSE)

for (i in c(1:nrow(study_table))){
  print(i)
  print(unique_entries[groupings == study_table$groupings[i]])
}
unique_entries[groupings == study_table$groupings[4]]




