str_replace_all("[_,]", "-") %>%
  # Remove spaces before and after '-'
  str_replace_all("\\s*-\\s*", "-") %>%
  # Remove leading and trailing spaces
  str_trim()

# Function to remove dot at the end of string except for SP. or SPP.
remove_dot <- function(x) {
  gsub("(?<!SP|SPP)\\.$", "", x, perl = TRUE)
}

# Function to rearrange the string (move characters before the scientific name to after)
rearrange_string <- function(x) {
  gsub("^(.*?)-(.*)$", "\\2-\\1", x)
}

# List of correct scientific names
correct_names <- c("M. SZULGAI", "M. KANSASII", "M. AVIUM", "M. BASILIENSE", "S. MALTOPHILIA", "C. DIFFICILE", "E. COLI", 
                   "M. ABSCESSUS", "M. FORTUITUM", "M. INTERCELLULARE", "M. TUBERCULOSIS", "M. VULNERIS", "S. AUREUS", "P. AERUGINOSA")

# Function to correct scientific names in a string
correct_scientific_name <- function(x, correct_names) {
  for (name in correct_names) {
    pattern <- paste0("(?i)", name)  # Case insensitive matching
    x <- gsub(pattern, name, x, perl = TRUE)
  }
  return(x)
}

# Sample data (replace this with your actual column)
data <- c("ALERE-M. SZULGAI", "ALERE-M. KANSASII", "BASEL-M. BASILIENSE", "C. DIFFICILE", "CF-S. MALTOPHILIA", "CLOS. DIFF.")

# Step 1: Remove dot (except for SP. or SPP.)
data <- sapply(data, remove_dot)

# Step 2: Rearrange strings
data <- sapply(data, rearrange_string)

# Step 3: Correct scientific names
data <- sapply(data, function(x) correct_scientific_name(x, correct_names))

# View the result
data


# 16/09/2024
# List of scientific names for validation
scientific_names <- c("M. SZULGAI", "M. KANSASII", "M. BASILIENSE", "S. MALTOPHILIA", 
                      "M. ABSCESSUS", "M. AVIUM", "M. FORTUITUM", "M. CHIMAERA", 
                      "M. TUBERCULOSIS", "S. AUREUS", "S. EPIDERMIDIS", "S. HUMI", 
                      "E. COLI", "M. BOVIS", "N. MENINGITIDIS", "P. AERUGINOSA", 
                      "S. AFRICA", "S. KOREENSIS", "S. NITRITIREDUCENS", "S. TERRAE")

# Remove dots but keep "SP." or "SPP."
remove_dots <- function(x) {
  gsub("(?<!SP|SPP)\\.$", "", x, perl = TRUE)
}

# Function to rearrange strings, but don't change scientific name-first formats
rearrange_string <- function(x) {
  # Identify if the string already starts with a scientific name
  if (any(grepl(paste0("^", scientific_names, collapse = "|"), x))) {
    return(x)  # Do not rearrange if scientific name is first
  } else {
    # Rearrange if non-scientific name precedes a hyphen
    gsub("^(.*?)-(.*)$", "\\2-\\1", x)
  }
}

# Add hyphen after scientific names if missing
add_hyphen_after_scientific_name <- function(x) {
  for (name in scientific_names) {
    x <- gsub(paste0("(", name, ")(?!-)", collapse = ""), "\\1-", x, perl = TRUE)
  }
  return(x)
}

# Combine the cleaning steps
clean_column <- function(column) {
  column <- sapply(column, remove_dots)
  column <- sapply(column, rearrange_string)
  column <- sapply(column, add_hyphen_after_scientific_name)
  return(column)
}

# Example column
column <- c("ALERE-M. SZULGAI", "M. CHIMAERA-WAGNER-BRAUN", "M. MICROTI-NGS")

# Apply cleaning function
cleaned_column <- clean_column(column)
print(cleaned_column)

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

# Print results
print(temp_dot)
