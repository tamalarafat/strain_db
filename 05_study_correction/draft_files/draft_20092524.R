# Apply this function to the 'corrected_names' column after running the initial reformatting
temp_study_fix$temp_country <- replace_with_map(temp_study_fix$temp_sci, country_map)

# Work from here
which(grepl(pattern = "NGS", x = temp_study_fix$temp_country))

# Change all the entry names with "SWABS-Botswana"
aa = temp_study_fix[which(grepl(pattern = "SWABS-Botswana", x = temp_study_fix$temp_country)), ]
aa$temp_country = "SWABS-Botswana 2023"

# Deal with the study column
temp_swabs_botswana = which(grepl(pattern = "SWABS-Botswana", x = temp_study_fix$temp_country))

temp_study_fix$temp_country[temp_swabs_botswana] <- "SWABS-Botswana 2023"

# Deal with the actual library file - "QC-2019"
temp_qc_2019_index = which(grepl(pattern = "QC-2019", x = all_libs$borstel_ID))

# Try it out
aa = all_libs[temp_qc_2019_index, ] 

# Assign to actual data
all_libs$study[temp_qc_2019_index] = "Quality Control panel 2019"

# Deal with the actual library file - "QC-2019"
temp_qc_2020_index = which(grepl(pattern = "QC-2020", x = all_libs$borstel_ID))

# Try it out
aa = all_libs[temp_qc_2020_index, ] 

# Assign to actual data
all_libs$study[temp_qc_2020_index] = "Quality Control panel 2020"

# Cleaning dashes: with "PORTAL"

# Replace special characters with '-' # Let's do it later
# temp_study_fix$names <- str_replace_all(temp_study_fix$names, "[_,/]", "-")

# Apply replacing function to the 'corrected_names' column after running the initial reformatting
temp_study_fix$temp_entry <- replace_entry_with_map(temp_study_fix$temp_country, entry_name_map)

# 	
"TB-Sequel-South Africa"

# Extract text before the entry name
gsub("^(TB-Sequel)(-)(.*?)", "\\1 \\3", "TB-Sequel-South Africa", perl = TRUE)

# Define the output file path
output_file <- "output_26_09_2024.txt"

# Open a connection to the file
file_conn <- file(output_file, "w")

# Loop over rows of the study_table
for (i in c(1:nrow(study_table))){
  # Write the index to the file
  cat(paste("Index:", i, "\n"), file=file_conn)
  
  # Write the unique entries corresponding to the current group
  #cat("Unique Entries:\n", file=file_conn)
  cat(unique_entries[groupings == study_table$groupings[i]], sep = ", ", file = file_conn)
  cat("\n\n", file=file_conn)  # Add extra space between entries for clarity
}

# Close the file connection
close(file_conn)

# Confirmation message
cat("Output written to", output_file)

# 26/09/2024

# Complete library

# Deal with the actual library file - "TP"
temp_tp_index = which(grepl(pattern = "TP\\d{1}", x = all_libs$study))

# Assign to actual data
aa = all_libs[temp_tp_index, ]




