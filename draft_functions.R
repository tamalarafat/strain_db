replace_exact_names <- function(text, side_name_map) {
  # Iterate over all country and city names
  for (pattern in names(side_name_map)) {
    # Replace the pattern with its mapped value
    text <- gsub(pattern, side_name_map[[pattern]], text, ignore.case = TRUE, fixed = TRUE)
  }
  
  # Return the cleaned text
  return(text)
}