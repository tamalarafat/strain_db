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


# "M. SZULGAI", "M. KANSASII", "M. BASILIENSE", "C. DIFFICILE | CLOS. DIFF", "S. MALTOPHILIA", "CHIMAERA", "CHLAMYDIA", "CHLAMYDIA PNEUMONIAE", "MTB", "COV19 | COVID-19", "CUPRIAVIDUS METALLIDURANS", "E. COLI", "HH",
# "BOVIS", "MICROTI", "M. AVIUM", "S. HOMINIS", "C. TRACHOMATIS", "KLEBSIA | KLEBSIELLA", "LEGIONELLA", "LEGIONELLA PNEUMOPHILA", "M. ABSCESSUS", "M. AVIUM", "M. BASILIENSE", "M. BOVIS", "M. CANETTI", "M. CHELONAE", 
# "M. FORTUITUM", "M. INTERCELULLARE", "M. SMEGMATIS", "M. SZULGAI", "M. TB | M. TUBERCULOSIS", "M. VULNERIS", "M. SIMIAE", "MCHIMAERA", "MYCOBACTERUIM SPECIES", "N. MENINGITIDIS", "ORIENTIA TSUTSUGAMUSHI", "P. AERUGINOSA", "S. MALTOPHILIA", "KANSASII", "RICKETTSIA TYPHI", "S. AUREUS", "S. EPIDERMIDIS"
# "S. MALTOPHILIA", "S. ACIDAMINIPHILIA", "S. HUMI", "S. KOREENSIS", "S. NITRITIREDUCENS", "S. RHIZOPHILIA", "S. TERRAE", "STENO | STENOTROPHOMONAS | STENOTROPHOMONAS SP."

c("M. SZULGAI", 
  "M. KANSASII",
  "KANSASII",
  "M. BASILIENSE", 
  "BOVIS",
  "M. BOVIS",
  "MYCOBACTERIUM BOVIS",
  "C. DIFFICILE",
  "CLOS. DIFF", 
  "S. MALTOPHILIA",
  "CHIMAERA",
  "M. CHIMAERA", 
  "MCHIMAERA",
  "CHLAMYDIA",
  "CHLAMYDIA PNEUMONIAE",
  "MTB", 
  "COV19",
  "COVID-19", 
  "CUPRIAVIDUS METALLIDURANS", 
  "E. COLI", 
  "HH", 
  "MICROTI", 
  "M. MICROTI", 
  "M. AVIUM", 
  "S. HOMINIS", 
  "C. TRACHOMATIS", 
  "KLEBSIA",
  "KLEBSIELLA", 
  "LEGIONELLA PNEUMOPHILA", 
  "M. ABSCESSUS", 
  "MYCOBACTERIUM ABSCESSUS", 
  "M. CANETTI", 
  "M. CHELONAE", 
  "M. FORTUITUM", 
  "M. INTERCELULLARE", 
  "M. INTRACELLULARE", 
  "M. SMEGMATIS", 
  "M. SZULGAI",
  "M. TB",
  "M. TUBERCULOSIS", 
  "M. VULNERIS", 
  "M. BOVIS-BOVIS",
  "M. BOVIS-CAPRAE",
  "M. CANETTII", 
  "M. SIMIAE", 
  "M. TUBERCULOSIS", 
  "MYCOBACTERUIM SPECIES", 
  "N. MENINGITIDIS", 
  "ORIENTIA TSUTSUGAMUSHI", 
  "P. AERUGINOSA", 
  "RICKETTSIA TYPHI", 
  "S. AUREUS", 
  "S. EPIDERMIDIS", 
  "S. ACIDAMINIPHILIA", 
  "S. HUMI", 
  "S. KOREENSIS", 
  "S. NITRITIREDUCENS", 
  "S. RHIZOPHILIA", 
  "S. TERRAE", 
  "STENO", 
  "STENOTROPHOMONAS",
  "STENOTROPHOMONAS SP.",
  "STENOTROPHOMONAS MALTOPHILIA")



# Example: M. CHIMAERAZAHNARZT, M. CHIMAERA-AUFTRAG



# Apply the function to the 'names' column
temp_study_fix$corrected_names <- sapply(temp_study_fix$correction, replace_and_reformat, study_scientific_name_map)

# Print the results
print(temp_data)



# commensal bacteria - KOMMENSALE BAKTERIEN, 
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




# Get unique entries
unique_entries <- unique(temp_study_fix$correction)

# Create a distance matrix based on string similarity
dist_matrix <- stringdistmatrix(unique_entries, unique_entries, method = "jw")

# get all the entries with a .
temp_study_5 = which(grepl(pattern = "\\.", x = temp_study_fix$names))

temp_dot = temp_study_fix[temp_study_5, ]

str_c(temp_dot$names, collapse = ", ")








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

unique_entries[groupings == study_table$groupings[1]]

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
