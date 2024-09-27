# Apply replacing function to the 'corrected_names' column after running the initial reformatting
temp_study_fix$temp_entry <- replace_entry_with_map(temp_study_fix$temp_country, entry_name_map)
# Done till this part

# Start working for here
# Apply exact matching to replace names - full entry name
#temp_study_fix$t1 <- replace_exact_names(temp_study_fix$temp_entry, entry_full_name_map)
temp_study_fix$t1 <- replace_exact_names(temp_study_fix$temp_entry, entry_people_compund_name_map)

# Apply exact matching to replace names - side names
temp_study_fix$t2 <- replace_exact_names(temp_study_fix$t1, entry_side_name_map)

# Apply exact matching to replace names - side names
# temp_study_fix$corrected_entry <- replace_exact_names(temp_study_fix$t2, entry_people_compund_name_map)
temp_study_fix$corrected_entry <- replace_exact_names(temp_study_fix$t2, entry_full_name_map)
# Subset
temp_df = temp_study_fix[, c(1, 2, 11)]

rownames(temp_df) <- NULL

write.table(temp_df, file = "all_study.tsv", row.names = FALSE, sep = "\t")
