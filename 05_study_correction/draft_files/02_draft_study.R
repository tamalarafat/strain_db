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


# Check for names
which(grepl(pattern = "136ekstra", x = all_libs$study))

# Check
which(grepl(pattern = "^Deutschland$", x = all_libs$source))

# Check
temp_study_1 = which(grepl(pattern = "^16S rRNA amplicon tests$", x = all_libs$study))

aa = all_libs[temp_study_1, ]

# Check
temp_study_2 = which(grepl(pattern = "^Alexandra Dangel$", x = all_libs$study))

bb = all_libs[temp_study_2, ]

# Check
temp_study_3 = which(grepl(pattern = "^DH/DN$", x = all_libs$owner))

DH = all_libs[temp_study_3, ]

temp_study_4 = which(grepl(pattern = "^unknown project Doris Hillemann$", x = all_libs$study))

dd = all_libs[temp_study_4, ]

## Filter

# what are those digits doing in the name section
temp_digit_check_index = which(grepl(pattern = str_c("^\\d{1,4}$", "^-$", sep = "|"), x = temp_study_tab$names))

# Data containig only missing values
temp_study_missing = temp_study_tab[temp_digit_check_index, ]

# Data containig only missing values
temp_study_fix = temp_study_tab[-temp_digit_check_index, ]

# Remove all the double space
temp_study_fix$names <- str_replace_all(temp_study_fix$names, "\\s{2,}", " ")

# Add missing space after a period
temp_study_fix$names <- str_replace_all(temp_study_fix$names, "(\\w)\\.(\\w)", "\\1. \\2")


#### Work from here

# Replace special characters with '-'
temp_study_fix$names <- str_replace_all(temp_study_fix$names, "[_,/]", "-")

# Add missing space after a period
temp_study_fix$names <- str_replace_all(temp_study_fix$names, "\\s*-\\s*", "-")

# Remove leading and trailing spaces
temp_study_fix$names <- str_trim(temp_study_fix$names)

# Get unique entries
unique_entries <- unique(temp_study_fix$names)

# Create a distance matrix based on string similarity
dist_matrix <- stringdistmatrix(unique_entries, unique_entries, method = "jw")

# get all the entries with a .
temp_study_5 = which(grepl(pattern = "\\.", x = temp_study_fix$names))

temp_dot = temp_study_fix[temp_study_5, ]

str_c(temp_dot$names, collapse = ", ")





# Changed 16/09/2024
# List of scientific names for validation
scientific_names <- c("M. SZULGAI", "M. KANSASII", "M. BASILIENSE", "S. MALTOPHILIA", 
                      "M. ABSCESSUS", "M. AVIUM", "M. FORTUITUM", "M. CHIMAERA", 
                      "M. TUBERCULOSIS", "S. AUREUS", "S. EPIDERMIDIS", "S. HUMI", 
                      "E. COLI", "M. BOVIS", "N. MENINGITIDIS", "P. AERUGINOSA", 
                      "S. AFRICA", "S. KOREENSIS", "S. NITRITIREDUCENS", "S. TERRAE")



temp_dot$correction <- temp_dot$names

# Remove dot at the end of string except for SP. or SPP.
temp_dot$correction <- gsub("(?<!SP|SPP)\\.$", "", temp_dot$correction, perl = TRUE)

# Step 2: Function to determine if scientific name comes first
starts_with_scientific_name <- function(x) {
  grepl(paste0("^(", paste(scientific_names, collapse = "|"), ")(\\b|-)"), x)
}

# Step 3: Function to rearrange strings if non-scientific name comes first
rearrange_if_needed <- function(x) {
  if (starts_with_scientific_name(x)) {
    return(x)  # Do not rearrange if scientific name is first
  } else {
    # Rearrange if non-scientific name precedes a hyphen
    return(gsub("^(.*?)-(.*)$", "\\2-\\1", x))
  }
}

# Step 4: Apply the rearrangement logic
temp_dot$correction <- sapply(temp_dot$correction, rearrange_if_needed)





if (any(grepl(paste0("^", scientific_names, collapse = "|"), temp_dot$corretion))) {
  temp_dot$corretion  # Do not rearrange if scientific name is first
} else {
  # Rearrange if non-scientific name precedes a hyphen
  gsub("^(.*?)-(.*)$", "\\2-\\1", temp_dot$corretion)
}


# Rearrange the string (move characters before the scientific name to after)
temp_dot$corretion <- gsub("^(.*?)-(.*)$", "\\2-\\1", temp_dot$corretion)

# Function to remove dot at the end of string except for SP. or SPP.
remove_dot <- function(x) {
  gsub("(?<!SP|SPP)\\.$", "", x, perl = TRUE)
}

# Function to rearrange the string (move characters before the scientific name to after)
rearrange_string <- function(x) {
  gsub("^(.*?)-(.*)$", "\\2-\\1", x)
}


# Set a similarity threshold (adjust based on inspection of results)
threshold <- 0.15 # Optimal

threshold <- 0.3

# Group similar entries based on the distance matrix
groupings <- hclust(as.dist(dist_matrix)) %>%
  cutree(h = threshold)

names_in_group <- unique_entries[groupings == 1]

unique_entries[groupings == 331]


# From the grouping we can get similar entries
study_table = as.data.frame(sort(table(groupings), decreasing = TRUE), stringsAsFactors = FALSE)

unique_entries[groupings == study_table$groupings[9]]

sum(study_table$Freq[1:30])

# Create a standardized mapping based on the most common entry in each group
standardized_names <- sapply(unique(groupings), function(group) {
  names_in_group <- unique_entries[groupings == group]
  most_common <- names_in_group[which.max(table(names_in_group))]
  return(most_common)
})




# Function to standardize names based on fuzzy matching
standardize_entries <- function(column) {
  # Get unique entries
  unique_entries <- unique(column)
  
  # Create a distance matrix based on string similarity
  dist_matrix <- stringdistmatrix(unique_entries, unique_entries, method = "jw")
  
  # Set a similarity threshold (adjust based on inspection of results)
  threshold <- 0.15
  
  # Group similar entries based on the distance matrix
  groupings <- hclust(as.dist(dist_matrix)) %>%
    cutree(h = threshold)
  
  # Create a standardized mapping based on the most common entry in each group
  standardized_names <- sapply(unique(groupings), function(group) {
    names_in_group <- unique_entries[groupings == group]
    most_common <- names_in_group[which.max(table(names_in_group))]
    return(most_common)
  })
  
  # Replace original names with standardized names
  column_standardized <- sapply(column, function(name) {
    group <- groupings[match(name, unique_entries)]
    standardized_names[group]
  })
  
  return(column_standardized)
}

# Apply the function to standardize the 'Study' column
data$Study <- standardize_entries(data$Study)


###### Work till here



# Replcae with a character identifier
temp_owner_tab$names[temp_digit_check_index] <- "check study"














# duplicate the column information
temp_study_tab$names = toupper(temp_study_tab$ID)

# How are the dates added to the table - convert them as character
temp_study_tab <- as.data.frame(apply(temp_study_tab, c(1, 2), as.character))


for (i in c(1:nrow(temp_study_tab))){
  if (temp_study_tab$names[i] == "EXTERNAL"){
    temp_study_tab$names[i] = "External"
  } else if (temp_study_tab$names[i] %in% c("INTERN", "INTERNAL")){
    temp_study_tab$names[i] = "Internal"
  } else {
    temp_study_tab$names[i] = "check study"
  }
}

rownames(temp_study_tab) <- NULL

write.table(temp_study_tab, file = "all_sources.tsv", row.names = FALSE, sep = "\t")
