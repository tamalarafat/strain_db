# List of organism names and corresponding scientific names
study_scientific_name_map <- list(
  "M. SZULGAI" = "Mycobacterium szulgai",
  "M. KANSASII" = "Mycobacterium kansasii",
  "M. BASILIENSE" = "Mycobacterium basiliense",
  "M. BOVIS" = "Mycobacterium bovis",
  "MYCOBACTERIUM BOVIS" = "Mycobacterium bovis",
  "C. DIFFICILE" = "Clostridium difficile",
  "CLOS. DIFF" = "Clostridium difficile", 
  "S. MALTOPHILIA" = "Stenotrophomonas maltophilia",
  "M. CHIMAERA" = "Mycobacterium chimaera", 
  "MCHIMAERA" = "Mycobacterium chimaera",
  "CHLAMYDIA PNEUMONIAE" = "Chlamydia pneumoniae",
  "MTB" = "Mycobacterium tuberculosis", 
  "M. TB" = "Mycobacterium tuberculosis",
  "M. TUBERCULOSIS" = "Mycobacterium tuberculosis", 
  "COV19" = "SARS-CoV-2",
  "COVID-19" = "SARS-CoV-2", 
  "CUPRIAVIDUS METALLIDURANS" = "Cupriavidus metallidurans", 
  "E. COLI" = "Escherichia coli", 
  "HH" = "Haemophilus haemolyticus", 
  "M. MICROTI" = "Mycobacterium microti", 
  "M. AVIUM" = "Mycobacterium avium", 
  "S. HOMINIS" = "Staphylococcus hominis", 
  "C. TRACHOMATIS" = "Chlamydia trachomatis",
  "LEGIONELLA PNEUMOPHILA" = "Legionella pneumophila", 
  "M. ABSCESSUS" = "Mycobacterium abscessus", 
  "MYCOBACTERIUM ABSCESSUS" = "Mycobacterium abscessus", 
  "M. CHELONAE" = "Mycobacterium chelonae", 
  "M. FORTUITUM" = "Mycobacterium fortuitum", 
  "M. INTERCELULLARE" = "Mycobacterium intracellulare", 
  "M. INTRACELLULARE" = "Mycobacterium intracellulare", 
  "M. SMEGMATIS" = "Mycobacterium smegmatis", 
  "M. VULNERIS" = "Mycobacterium vulneris", 
  #"M. BOVIS-BOVIS" = "Mycobacterium -bovis, -bovis",
  #"M. BOVIS-CAPRAE" = "Mycobacterium -bovis, -caprae",
  "M. CANETTII" = "Mycobacterium canettii", 
  "M. SIMIAE" = "Mycobacterium simiae", 
  "MYCOBACTERUIM SPECIES" = "Mycobacterium spp.", 
  "N. MENINGITIDIS" = "Neisseria meningitidis", 
  "ORIENTIA TSUTSUGAMUSHI" = "Orientia tsutsugamushi", 
  "P. AERUGINOSA" = "Pseudomonas aeruginosa", 
  "RICKETTSIA TYPHI" = "Rickettsia typhi", 
  "S. AUREUS" = "Staphylococcus aureus", 
  "S. EPIDERMIDIS" = "Staphylococcus epidermidis", 
  "S. ACIDAMINIPHILIA" = "Stenotrophomonas acidaminiphila", 
  "S. HUMI" = "S. humi", 
  "S. KOREENSIS" = "Sphingomonas koreensis", 
  "S. NITRITIREDUCENS" = "Stenotrophomonas nitritireducens", 
  "S. RHIZOPHILIA" = "Stenotrophomonas rhizophila", 
  "S. TERRAE" = "S. terrae", 
  "STENOTROPHOMONAS SP." = "Stenotrophomonas spp.",
  "STENOTROPHOMONAS MALTOPHILIA" = "Stenotrophomonas maltophilia",
  "STENOTROPHOMONAS" = "Stenotrophomonas spp.",
  "KLEBSIA" = "Klebsiella pneumoniae",
  "KLEBSIELLA" = "Klebsiella pneumoniae", 
  "BOVIS" = "Mycobacterium bovis",
  "KANSASII" = "Mycobacterium kansasii",
  "CHIMAERA" = "Mycobacterium chimaera",
  "MICROTI" = "Mycobacterium microti",
  "STENO" = "Stenotrophomonas spp.",
  "M. CANETTI" = "Mycobacterium canettii"
)

# New
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

# Function to replace organism name with scientific name and reformat the string
replace_and_reformat <- function(x, study_scientific_name_map) {
  for (org_name in names(study_scientific_name_map)) {
    if (grepl(org_name, x, ignore.case = TRUE)) {
      # Replace organism name with the full scientific name
      x <- gsub(org_name, study_scientific_name_map[[org_name]], x, ignore.case = TRUE)
      
      # Ensure there is a hyphen after the full scientific name ONLY if text follows it
      # Modify the pattern to check if the scientific name is followed by letters, numbers, or text
      x <- gsub("([A-Za-z]+\\s+[a-z]+)([A-Za-z0-9])", "\\1-\\2", x)
      
      # Extract text before and after the scientific name and reformat the string
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

# Function to replace organism name with scientific name and reformat the string
replace_and_reformat <- function(x, study_scientific_name_map) {
  for (org_name in names(study_scientific_name_map)) {
    if (grepl(org_name, x, ignore.case = TRUE)) {
      # Replace organism name with the full scientific name
      x <- gsub(org_name, study_scientific_name_map[[org_name]], x, ignore.case = TRUE)
      
      # Ensure there is a hyphen if text follows the full scientific name (after genus and species)
      # The pattern will check for the full name and add the hyphen after it if followed by more text
      x <- gsub("([A-Za-z]+\\s+[a-z]+)(\\S)", "\\1-\\2", x)
      
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

# Function to replace organism name with scientific name and reformat the string
replace_and_reformat <- function(x, study_scientific_name_map) {
  for (org_name in names(study_scientific_name_map)) {
    if (grepl(org_name, x, ignore.case = TRUE)) {
      # Replace organism name with scientific name
      x <- gsub(org_name, study_scientific_name_map[[org_name]], x, ignore.case = TRUE)
      
      # Ensure there is a hyphen if text follows the scientific name
      x <- gsub("(Mycobacterium|Escherichia|Stenotrophomonas|Staphylococcus|Pseudomonas|SARS-CoV-2|Neisseria|Chlamydia|Clostridium|Cupriavidus|Haemophilus)\\s*(\\w+)", "\\1-\\2", x)
      
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

# Old
# Function to replace organism name with scientific name and reformat the string
replace_and_reformat <- function(x, study_scientific_name_map) {
  for (org_name in names(study_scientific_name_map)) {
    if (grepl(org_name, x, ignore.case = TRUE)) {
      # Replace organism name with scientific name
      x <- gsub(org_name, study_scientific_name_map[[org_name]], x, ignore.case = TRUE)
      # Reformat: extract the text around the organism name
      x <- gsub("(.*)(Mycobacterium.*|Escherichia.*|Stenotrophomonas.*|Staphylococcus.*|Pseudomonas.*|SARS-CoV-2|Neisseria.*|Chlamydia.*|Clostridium.*|Cupriavidus.*|Haemophilus.*)(.*)", "\\2-\\1\\3", x, perl = TRUE)
      x <- gsub("-$", "", x) # Remove any trailing hyphen if no text is left
      #x <- gsub("^(.*?)([^ ]+)$", "\\1-\\2", x)
      return(x)
    }
  }
  return(x)  # Return unchanged if no match is found
}




# New
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






# List of known scientific names and their replacements
study_scientific_name_map <- list(
  "M. SZULGAI" = "Mycobacterium szulgai",
  "M. KANSASII" = "Mycobacterium kansasii",
  "KANSASII" = "Mycobacterium kansasii",
  "M. BASILIENSE" = "Mycobacterium basiliense",
  "BOVIS" = "Mycobacterium bovis",
  "M. BOVIS" = "Mycobacterium bovis",
  "MYCOBACTERIUM BOVIS" = "Mycobacterium bovis",
  "C. DIFFICILE" = "Clostridium difficile",
  "CLOS. DIFF" = "Clostridium difficile", 
  "S. MALTOPHILIA" = "Stenotrophomonas maltophilia",
  "CHIMAERA" = "Mycobacterium chimaera",
  "M. CHIMAERA" = "Mycobacterium chimaera", 
  "MCHIMAERA" = "Mycobacterium chimaera",
  "CHLAMYDIA PNEUMONIAE" = "Chlamydia pneumoniae",
  "MTB" = "Mycobacterium tuberculosis", 
  "M. TB" = "Mycobacterium tuberculosis",
  "M. TUBERCULOSIS" = "Mycobacterium tuberculosis", 
  "COV19" = "SARS-CoV-2",
  "COVID-19" = "SARS-CoV-2", 
  "CUPRIAVIDUS METALLIDURANS" = "Cupriavidus metallidurans", 
  "E. COLI" = "Escherichia coli", 
  "HH" = "Haemophilus haemolyticus", 
  "MICROTI" = "Mycobacterium microti", 
  "M. MICROTI" = "Mycobacterium microti", 
  "M. AVIUM" = "Mycobacterium avium", 
  "S. HOMINIS" = "Staphylococcus hominis", 
  "C. TRACHOMATIS" = "Chlamydia trachomatis", 
  "KLEBSIA" = "Klebsiella pneumoniae",
  "KLEBSIELLA" = "Klebsiella pneumoniae", 
  "LEGIONELLA PNEUMOPHILA" = "Legionella pneumophila", 
  "M. ABSCESSUS" = "Mycobacterium abscessus", 
  "MYCOBACTERIUM ABSCESSUS" = "Mycobacterium abscessus", 
  "M. CANETTI" = "Mycobacterium canettii", 
  "M. CHELONAE" = "Mycobacterium chelonae", 
  "M. FORTUITUM" = "Mycobacterium fortuitum", 
  "M. INTERCELULLARE" = "Mycobacterium intracellulare", 
  "M. INTRACELLULARE" = "Mycobacterium intracellulare", 
  "M. SMEGMATIS" = "Mycobacterium smegmatis", 
  "M. VULNERIS" = "Mycobacterium vulneris", 
  "M. BOVIS-BOVIS" = "Mycobacterium -bovis, -bovis",
  "M. BOVIS-CAPRAE" = "Mycobacterium -bovis, -caprae",
  "M. CANETTII" = "Mycobacterium canettii", 
  "M. SIMIAE" = "Mycobacterium simiae", 
  "MYCOBACTERUIM SPECIES" = "Mycobacterium spp.", 
  "N. MENINGITIDIS" = "Neisseria meningitidis", 
  "ORIENTIA TSUTSUGAMUSHI" = "Orientia tsutsugamushi", 
  "P. AERUGINOSA" = "Pseudomonas aeruginosa", 
  "RICKETTSIA TYPHI" = "Rickettsia typhi", 
  "S. AUREUS" = "Staphylococcus aureus", 
  "S. EPIDERMIDIS" = "Staphylococcus epidermidis", 
  "S. ACIDAMINIPHILIA" = "Stenotrophomonas acidaminiphila", 
  "S. HUMI" = "S. humi", 
  "S. KOREENSIS" = "Sphingomonas koreensis", 
  "S. NITRITIREDUCENS" = "Stenotrophomonas nitritireducens", 
  "S. RHIZOPHILIA" = "Stenotrophomonas rhizophila", 
  "S. TERRAE" = "S. terrae", 
  "STENO" = "Stenotrophomonas spp.", 
  "STENOTROPHOMONAS" = "Stenotrophomonas spp.",
  "STENOTROPHOMONAS SP." = "Stenotrophomonas spp.",
  "STENOTROPHOMONAS MALTOPHILIA" = "Stenotrophomonas maltophilia"
)