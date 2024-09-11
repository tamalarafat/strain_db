# load the libs
source("~/Documents/Projects/git_projects/strain_db/functions/loadRdata.R")

library(readxl)
library(lubridate)
library(stringr)
library(dplyr)

# load the file containing missing libraries
all_libs = loadRData("~/Documents/Projects/git_projects/strain_db/output_files/01_date_corrected/all_libs.RData")

# Transform all the NA to "-"
all_libs$owner[which(is.na(all_libs$owner))] <- "-"

# Convert them to upper case - later
# all_libs$owner = toupper(all_libs$owner)

# How are the dates added to the table
temp_owner_tab = data.frame(table(all_libs$owner), stringsAsFactors = FALSE)

# Assign column names
colnames(temp_owner_tab) <- c("ID", "count")

# duplicate the column information
temp_owner_tab$names = temp_owner_tab$ID

# How are the dates added to the table - convert them as character
temp_owner_tab <- as.data.frame(apply(temp_owner_tab, c(1, 2), as.character))

# what are those digits doing in the name section
temp_digit_check_index = which(grepl(pattern = str_c("\\d{1,3}", "-", "controls", "Dag", "Deeplex", "Deutschland", "DNA", "external", "FIND", "internal", "Internal", "M.bovis", "M.chimaera", "Mailand", "Maputo", "NRZ", "NGS-Aufträge", "Rasmussen", "roter Ständer", sep = "|"), x = temp_owner_tab$names))

# Replcae with a character identifier
temp_owner_tab$names[temp_digit_check_index] <- "check study"

# Get the libs with owner information
temp_owner_owned = temp_owner_tab[!temp_owner_tab$names %in% "check study", ]

temp_names = temp_owner_owned$names

for (i in c(1:nrow(temp_owner_owned))){
  if (temp_owner_owned$names[i] == "Alejandro"){
    temp_owner_owned$names[i] = "AV"
  } else if (temp_owner_owned$names[i] == "Anna"){
    temp_owner_owned$names[i] = "AM"
  } else if (temp_owner_owned$names[i] == "Annelies Yinkernagel"){
    temp_owner_owned$names[i] = "AY"
  } else if (temp_owner_owned$names[i] == "Annemarie"){
    temp_owner_owned$names[i] = "AH"
  } else if (temp_owner_owned$names[i] == "Aya/ Matthias"){
    temp_owner_owned$names[i] = "MM"
  } else if (temp_owner_owned$names[i] == "Barbara Kalsdorf"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "Christoph Lange"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "CU, TK"){
    temp_owner_owned$names[i] = "CU/TK"
  } else if (temp_owner_owned$names[i] == "CZ"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "DH"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "DH/DN"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "DN/PB"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] %in% c("Glennah", "GK")){
    temp_owner_owned$names[i] = "GK"
  } else if (temp_owner_owned$names[i] == "Harald Hoffman"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "HB"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "HC"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "Holger Heine"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "IB, LA"){
    temp_owner_owned$names[i] = "IB/LA"
  } else if (temp_owner_owned$names[i] %in% c("Irena", "Irena Zivanovic")){
    temp_owner_owned$names[i] = "IZ"
  } else if (temp_owner_owned$names[i] == "JA"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "Jana Schönfeld"){
    temp_owner_owned$names[i] = "JS"
  } else if (temp_owner_owned$names[i] %in% c("Johanna", "Johanna Pèrez", "JP")){
    temp_owner_owned$names[i] = "JP"
  } else if (temp_owner_owned$names[i] == "John"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] %in% c("Julia", "JZ")){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "Justine"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "Karen Björn-Mortensen"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "Knobloch"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "LA,CU"){
    temp_owner_owned$names[i] = "LA/CU"
  } else if (temp_owner_owned$names[i] %in% c("Leila", "LJ")){
    temp_owner_owned$names[i] = "LJ"
  } else if (temp_owner_owned$names[i] == "Leonardo de Araujo"){
    temp_owner_owned$names[i] = "LA"
  } else if (temp_owner_owned$names[i] == "LI"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] %in% c("Lindsay", "LT", "LS")){
    temp_owner_owned$names[i] = "LS"
  } else if (temp_owner_owned$names[i] %in% c("Margo Diricks")){
    temp_owner_owned$names[i] = "MD"
  } else if (temp_owner_owned$names[i] == "Matthias"){
    temp_owner_owned$names[i] = "MM"
  } else if (temp_owner_owned$names[i] == "MR"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "NIP/UNAM"){
    temp_owner_owned$names[i] = "CU/IB/VD"
  } else if (temp_owner_owned$names[i] %in% c("nuebel", "Nuebel", "Ulrich Nübel")){
    temp_owner_owned$names[i] = "UN"
  } else if (temp_owner_owned$names[i] == "Patrick"){
    temp_owner_owned$names[i] = "PB"
  } else if (temp_owner_owned$names[i] == "SF/ TK"){
    temp_owner_owned$names[i] = "SF/TK"
  } else if (temp_owner_owned$names[i] == "SG"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] %in% c("Silke")){
    temp_owner_owned$names[i] = "SF"
  } else if (temp_owner_owned$names[i] == "Silvia Maaß"){
    temp_owner_owned$names[i] = "SM"
  } else if (temp_owner_owned$names[i] == "Sönke"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "SP"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "Stefan"){
    temp_owner_owned$names[i] = "SN"
  } else if (temp_owner_owned$names[i] %in% c("Susanne", "Susanne Homolka")){
    temp_owner_owned$names[i] = "SH"
  } else if (temp_owner_owned$names[i] == "Tanja/Stefan"){
    temp_owner_owned$names[i] = "TN/SN"
  } else if (temp_owner_owned$names[i] %in% c("Thomas", "Thomas Kohl", "Tk", "TK")){
    temp_owner_owned$names[i] = "TK"
  } else if (temp_owner_owned$names[i] == "TMW"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "Trisha"){
    temp_owner_owned$names[i] = "TP"
  } else if (temp_owner_owned$names[i] == "TU"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "Uwe Mamat"){
    temp_owner_owned$names[i] = "UM"
  } else if (temp_owner_owned$names[i] == "VD or MD"){
    temp_owner_owned$names[i] = "VD/MD"
  } else if (temp_owner_owned$names[i] %in% c("VD, MM", "VD,MM", "VD/MM")){
    temp_owner_owned$names[i] = "VD/MM"
  } else if (temp_owner_owned$names[i] == "Vera Katalinic-Jankovic"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] %in% c("Viola", "VS", "VS, high cov")){
    temp_owner_owned$names[i] = "VD"
  } else if (temp_owner_owned$names[i] == "Wolfgang Streit"){
    temp_owner_owned$names[i] = "ask Viola"
  } else if (temp_owner_owned$names[i] == "Yassir"){
    temp_owner_owned$names[i] = "YS"
  } else if (temp_owner_owned$names[i] == "Yassir/GK"){
    temp_owner_owned$names[i] = "YS/GK"
  } else {
    temp_owner_owned$names[i] = temp_owner_owned$names[i]
  }
}

# # Check 
# if (temp_names[i] == "Alejandro"){
#   temp_names[i] = "AV"
# } else if (temp_names[i] == "Anna"){
#   temp_names[i] = "AM"
# } else if (temp_names[i] == "Annelies Yinkernagel"){
#   temp_names[i] = "AY"
# } else if (temp_names[i] == "Annemarie"){
#   temp_names[i] = "AH"
# } else if (temp_names[i] == "Aya/ Matthias"){
#   temp_names[i] = "MM"
# } else if (temp_names[i] == "Barbara Kalsdorf"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "Christoph Lange"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "CU, TK"){
#   temp_names[i] = "CU/TK"
# } else if (temp_names[i] == "CZ"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "DH"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "DH/DN"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "DN/PB"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] %in% c("Glennah", "GK")){
#   temp_names[i] = "GK"
# } else if (temp_names[i] == "Harald Hoffman"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "HB"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "HC"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "Holger Heine"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "IB, LA"){
#   temp_names[i] = "IB/LA"
# } else if (temp_names[i] %in% c("Irena", "Irena Zivanovic")){
#   temp_names[i] = "IZ"
# } else if (temp_names[i] == "JA"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "Jana Schönfeld"){
#   temp_names[i] = "JS"
# } else if (temp_names[i] %in% c("Johanna", "Johanna Pèrez", "JP")){
#   temp_names[i] = "JP"
# } else if (temp_names[i] == "John"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] %in% c("Julia", "JZ")){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "Justine"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "Karen Björn-Mortensen"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "Knobloch"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "LA,CU"){
#   temp_names[i] = "LA/CU"
# } else if (temp_names[i] %in% c("Leila", "LJ")){
#   temp_names[i] = "LJ"
# } else if (temp_names[i] == "Leonardo de Araujo"){
#   temp_names[i] = "LA"
# } else if (temp_names[i] == "LI"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] %in% c("Lindsay", "LT", "LS")){
#   temp_names[i] = "LS"
# } else if (temp_names[i] %in% c("Margo Diricks")){
#   temp_names[i] = "MD"
# } else if (temp_names[i] == "Matthias"){
#   temp_names[i] = "MM"
# } else if (temp_names[i] == "MR"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "NIP/UNAM"){
#   temp_names[i] = "CU/IB/VD"
# } else if (temp_names[i] %in% c("nuebel", "Nuebel", "Ulrich Nübel")){
#   temp_names[i] = "UN"
# } else if (temp_names[i] == "Patrick"){
#   temp_names[i] = "PB"
# } else if (temp_names[i] == "SF/ TK"){
#   temp_names[i] = "SF/TK"
# } else if (temp_names[i] == "SG"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] %in% c("Silke")){
#   temp_names[i] = "SF"
# } else if (temp_names[i] == "Silvia Maaß"){
#   temp_names[i] = "SM"
# } else if (temp_names[i] == "Sönke"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "SP"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "Stefan"){
#   temp_names[i] = "SN"
# } else if (temp_names[i] %in% c("Susanne", "Susanne Homolka")){
#   temp_names[i] = "SH"
# } else if (temp_names[i] == "Tanja/Stefan"){
#   temp_names[i] = "TN/SN"
# } else if (temp_names[i] %in% c("Thomas", "Thomas Kohl", "Tk", "TK")){
#   temp_names[i] = "TK"
# } else if (temp_names[i] == "TMW"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "Trisha"){
#   temp_names[i] = "TP"
# } else if (temp_names[i] == "TU"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "Uwe Mamat"){
#   temp_names[i] = "UM"
# } else if (temp_names[i] == "VD or MD"){
#   temp_names[i] = "VD/MD"
# } else if (temp_names[i] %in% c("VD, MM", "VD,MM", "VD/MM")){
#   temp_names[i] = "VD/MM"
# } else if (temp_names[i] == "Vera Katalinic-Jankovic"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] %in% c("Viola", "VS", "VS, high cov")){
#   temp_names[i] = "VD"
# } else if (temp_names[i] == "Wolfgang Streit"){
#   temp_names[i] = "ask Viola"
# } else if (temp_names[i] == "Yassir"){
#   temp_names[i] = "YS"
# } else if (temp_names[i] == "Yassir/GK"){
#   temp_names[i] = "YS/GK"
# } else {
#   temp_names[i] = "missed"
# }


# Get the libs with owner information
temp_owner_missing = temp_owner_tab[temp_owner_tab$names %in% "check study", ]



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