# Started at line 235; after "temp_libs$tentative_date = temp_years$nice_dates[temp_match_index]"

which(temp_libs$years %in% NA)

temp_libs[7250, ]

# save the updated file
save(temp_libs, file = "all_libs.RData")

# Rows without the year information
temp_empty_libs = temp_libs[which(temp_libs$years %in% NA), ]

# save the updated file
save(temp_empty_libs, file = "libs_without_dates.RData")

# Rows with the year information
temp_dated_libs = temp_libs[which(!temp_libs$years %in% NA), ]

# save the updated file
save(temp_dated_libs, file = "libs_with_dates.RData")

# load the libs
source("~/Documents/Projects/Git_repositories/strain_db/functions/loadRdata.R")

library(readxl)
library(lubridate)
library(stringr)
library(dplyr)


# load the libs file
all_libs = loadRData("all_libs.RData")

# column names
colnames(all_libs)

# Issue 1: Transform ambiguous characters - "-" / "nachtr." to a standard NA character

# Transform all empty elements with a "-" to NA
"-" %in% all_libs[, 2]

for (i in c(1:ncol(all_libs))){
  if ("-" %in% all_libs[, i]) {
    all_libs[, i][all_libs[, i] == "-"] <- NA
  }
}

# Transform all empty elements with a "-" to NA
"nachtr." %in% all_libs[, 4]

for (i in c(1:ncol(all_libs))){
  if ("nachtr." %in% all_libs[, i]) {
    all_libs[, i][all_libs[, i] == "nachtr."] <- NA
  }
}

# Issue 2: Is there empty rows?

# How many rows without any entry
print(paste0("Total number of input without any content: ", sum(rowSums(is.na(all_libs)) == ncol(all_libs))))

# Remove rows containing only NA values
all_libs =  all_libs[!rowSums(is.na(all_libs)) == ncol(all_libs), ]

# Issue 3: Remove rows where five or more values are missing in the first seven columns and without a library number
all_libs <- all_libs[!(!rowSums(is.na(all_libs[, 1:7])) < 6 & is.na(all_libs$num)), ]

# save the updated file
save(temp_libs, file = "all_libs.RData")

# Rows without the year information
temp_empty_libs = temp_libs[which(temp_libs$years %in% NA), ]

# save the updated file
save(temp_empty_libs, file = "libs_without_dates.RData")

# Rows with the year information
temp_dated_libs = temp_libs[which(!temp_libs$years %in% NA), ]

# save the updated file
save(temp_dated_libs, file = "libs_with_dates.RData")

# Check issue 3

# Missing lib id and other information
which((!rowSums(is.na(all_libs[, 1:7])) < 6 & is.na(all_libs$num)))
temp_missing = all_libs[which((!rowSums(is.na(all_libs[, 1:7])) < 6 & is.na(all_libs$num))), ]



table(!rowSums(is.na(all_libs[, 1:7])) < 6)
which(!rowSums(is.na(all_libs[, 1:7])) < 6)
length(!rowSums(is.na(all_libs[, 1:7])) < 6)



table(is.na(all_libs$num))

bb = all_libs[which(rowSums(is.na(all_libs[, 1:7])) >= 5), ]
dd = all_libs[(rowSums(is.na(all_libs[, 1:7])) >= 6) & is.na(all_libs$num), ]

cc = all_libs[!(rowSums(is.na(all_libs[, 1:7])) < 6) & !(!is.na(all_libs$num)), ]





# save the updated file
save(temp_libs, file = "all_libs.RData")

# Rows without the year information
temp_empty_libs = temp_libs[which(temp_libs$years %in% NA), ]

# save the updated file
save(temp_empty_libs, file = "libs_without_dates.RData")

# Rows with the year information
temp_dated_libs = temp_libs[which(!temp_libs$years %in% NA), ]

# save the updated file
save(temp_dated_libs, file = "libs_with_dates.RData")



# 
# aa = loadRData("libs_without_dates.RData")
# aa[, 2][aa[, 2] == "-"] <- NA
# 
# # How are the dates added to the table - convert them as character
# temp_dates_tab <- as.data.frame(apply(temp_dates_tab, c(1, 2), as.character))
# 
# # Step 1: Replace all single dashes with NA
# all_libs[all_libs == "-"] <- NA
# 
# aa$source[aa$source == "-"] <- NA
# 
# aa[, 2] == "-" <- NA
# 
# aa$date_of_isolation[aa$date_of_isolation == "-"] <- NA
# 
# aa$date_of_isolation[aa$date_of_isolation == "-"] <- NA
