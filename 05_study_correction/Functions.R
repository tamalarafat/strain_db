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
