# load the libs
source("~/Documents/Projects/git_projects/strain_db/functions/loadRdata.R")

library(readxl)
library(lubridate)
library(stringr)
library(dplyr)

# load the file containing missing libraries
all_libs = loadRData("~/Documents/Projects/git_projects/strain_db/output_files/01_date_corrected/all_libs.RData")

# Transform all the NA to "-"
all_libs$study[which(is.na(all_libs$study))] <- "-"

# How are the dates added to the table
temp_study_tab = data.frame(table(all_libs$study), stringsAsFactors = FALSE)

# Assign column names
colnames(temp_study_tab) <- c("ID", "count")

# duplicate the column information
temp_org_tab$names = toupper(temp_org_tab$ID)

# How are the dates added to the table - convert them as character
temp_org_tab <- as.data.frame(apply(temp_org_tab, c(1, 2), as.character))

# Remove all the double space
temp_org_tab$names <- str_replace_all(temp_org_tab$names, "\\s{2,}", " ")

# Add missing space after a period
temp_org_tab$names <- str_replace_all(temp_org_tab$names, "(\\w)\\.(\\w)", "\\1. \\2")










# duplicate the column information
temp_study_tab$names = toupper(temp_study_tab$ID)

# How are the dates added to the table - convert them as character
temp_study_tab <- as.data.frame(apply(temp_study_tab, c(1, 2), as.character))


for (i in c(1:nrow(temp_study_tab))){
  if (temp_study_tab$names[i] == "EXTERNAL"){
    temp_study_tab$names[i] = "External"
  } else if (temp_study_tab$names[i] %in% c("INTERN", "INTERNAL")){
    temp_study_tab$names[i] = "Internal"
  } else {
    temp_study_tab$names[i] = "check study"
  }
}

rownames(temp_study_tab) <- NULL

write.table(temp_study_tab, file = "all_sources.tsv", row.names = FALSE, sep = "\t")
