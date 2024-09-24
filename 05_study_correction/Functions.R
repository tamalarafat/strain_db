# Function to replace organism name with scientific name and reformat the string
replace_and_reformat <- function(x, study_scientific_name_map) {
  for (org_name in names(study_scientific_name_map)) {
    if (grepl(org_name, x, ignore.case = TRUE)) {
      # Replace organism name with scientific name
      x <- gsub(org_name, study_scientific_name_map[[org_name]], x, ignore.case = TRUE)
      
      # Extract text before and after the organism name
      x <- gsub("(.*?)(Mycobacterium.*|Escherichia.*|Stenotrophomonas.*|Staphylococcus.*|Pseudomonas.*|SARS-CoV-2|Neisseria.*|Chlamydia.*|Clostridium.*|Cupriavidus.*|Haemophilus.*)(.*?)$", "\\2-\\1\\3", x, perl = TRUE)
      
      # Remove any extra hyphens from the start or end
      x <- gsub("-$", "", x)          # Remove trailing hyphen if no text follows
      x <- gsub("^-", "", x)          # Remove leading hyphen if no text precedes
      x <- gsub("--", "-", x)         # Replace any double hyphens with a single hyphen
      return(x)
    }
  }
  return(x)  # Return unchanged if no match is found
}


add_hyphen_after_sci_name <- function(x, sci_names) {
  for (sci_name in sci_names) {
    # Check if the string contains the scientific name and extra characters follow
    if (grepl(paste0(sci_name, "(\\s|\\S)"), x) && !grepl(paste0(sci_name, "-"), x)) {
      # Add hyphen between scientific name and the following text, if not already present
      x <- gsub(paste0("(", sci_name, ")\\s*(\\S)"), "\\1-\\2", x)
    }
  }
  return(x)
}

# New and fully functioning: Function to replace country/city names
replace_with_map <- function(study, country_map) {
  
  # Iterate over the rows of the column
  for (i in seq_along(study)) {
    
    # Iterate through the country_map key-value pairs
    for (j in seq_along(country_map)) {
      
      pattern <- names(country_map)[j]
      replacement <- country_map[[j]]
      
      # Replace the matched names
      if (grepl(pattern, study[i], ignore.case = TRUE)) {
        study[i] <- gsub(pattern, replacement, study[i], ignore.case = TRUE)
        
        # Add space if digits follow the country/city name
        study[i] <- gsub(paste0("(", replacement, ")([0-9])"), "\\1 \\2", study[i])
      }
    }
  }
  return(study)
}

# 1st and functioning without the space addign part: Function to replace country/city names
replace_country_city <- function(text, country_map) {
  # Iterate over all country and city names
  for (pattern in names(country_map)) {
    # Replace the pattern with its mapped value
    text <- gsub(pattern, country_map[[pattern]], text, ignore.case = TRUE)
    
    # Ensure proper spacing around the replaced country/city name
    text <- gsub(paste0(" ", country_map[[pattern]], " "), paste0(" ", country_map[[pattern]], " "), text)
  }
  
  # Return the cleaned text
  return(text)
}
