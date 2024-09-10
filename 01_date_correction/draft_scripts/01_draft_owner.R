# load the libs
source("~/Documents/Projects/Git_repositories/strain_db/functions/loadRdata.R")

library(readxl)
library(lubridate)
library(stringr)
library(dplyr)


# load the libs file
all_libs = loadRData("all_libs.RData")

# Convert them to upper case - later
# all_libs$owner = toupper(all_libs$owner)

# How are the dates added to the table
temp_owner_tab = data.frame(table(all_libs$owner), stringsAsFactors = FALSE)




# Check:
which(all_libs$owner %in% "Blau A7")

# Index - rows with missing owner information
temp_missing_owner = which(all_libs$owner %in% NA)

# DF - rows with missing owner information
df_missing_owner = all_libs[temp_missing_owner, ]

# DF - rows with missing owner information
df_with_owner = all_libs[which(!all_libs$owner %in% NA), ]

# 
table(df_with_owner$owner)



# Entries with missing Borstel ID
# which(grepl(pattern = "ETB", x = toupper(all_libs$borstel_ID)))