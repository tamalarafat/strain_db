# load the libs
source("~/Documents/Projects/Git_repositories/strain_db/functions/loadRdata.R")

library(readxl)
library(lubridate)
library(stringr)
library(dplyr)


# load the libs file
all_libs = loadRData("all_libs.RData")

temp_missing_libID = which(all_libs$num %in% NA)

aa = all_libs[temp_missing_libID, ]

# Entries with missing Borstel ID
# which(grepl(pattern = "ETB", x = toupper(all_libs$borstel_ID)))