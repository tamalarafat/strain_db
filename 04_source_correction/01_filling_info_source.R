# load the libs
source("~/Documents/Projects/git_projects/strain_db/functions/loadRdata.R")

library(readxl)
library(lubridate)
library(stringr)
library(dplyr)

# load the file containing missing libraries
all_libs = loadRData("~/Documents/Projects/git_projects/strain_db/output_files/01_date_corrected/all_libs.RData")

# Transform all the NA to "-"
all_libs$source[which(is.na(all_libs$source))] <- "-"

# How are the dates added to the table
temp_source_tab = data.frame(table(all_libs$source), stringsAsFactors = FALSE)

# Assign column names
colnames(temp_source_tab) <- c("ID", "count")

# duplicate the column information
temp_source_tab$names = toupper(temp_source_tab$ID)

# How are the dates added to the table - convert them as character
temp_source_tab <- as.data.frame(apply(temp_source_tab, c(1, 2), as.character))


for (i in c(1:nrow(temp_source_tab))){
  if (temp_source_tab$names[i] == "EXTERNAL"){
    temp_source_tab$names[i] = "External"
  } else if (temp_source_tab$names[i] %in% c("INTERN", "INTERNAL")){
    temp_source_tab$names[i] = "Internal"
  } else {
    temp_source_tab$names[i] = "check study"
  }
}

rownames(temp_source_tab) <- NULL

write.table(temp_source_tab, file = "all_sources.tsv", row.names = FALSE, sep = "\t")
