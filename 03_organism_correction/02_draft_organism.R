# load the libs
source("~/Documents/Projects/git_projects/strain_db/functions/loadRdata.R")

library(readxl)
library(lubridate)
library(stringr)
library(dplyr)

# load the file containing missing libraries
all_libs = loadRData("~/Documents/Projects/git_projects/strain_db/output_files/01_date_corrected/all_libs.RData")

# Transform all the NA to "-"
all_libs$organism[which(is.na(all_libs$organism))] <- "-"

# NA ones are correct - confirmed with Ivan
which(grepl(pattern = "^NA$", x = all_libs$organism))

# NA ones are correct - confirmed with Ivan
which(grepl(pattern = "^Acinetobacter$", x = all_libs$organism))

# How are the dates added to the table
temp_org_tab = data.frame(table(all_libs$organism), stringsAsFactors = FALSE)

# Assign column names
colnames(temp_org_tab) <- c("ID", "count")

# duplicate the column information
temp_org_tab$names = toupper(temp_org_tab$ID)

# How are the dates added to the table - convert them as character
temp_org_tab <- as.data.frame(apply(temp_org_tab, c(1, 2), as.character))

# Clean spaces and add space after period if missing
which(grepl(pattern = ".*\\s\\s.*", x = temp_org_tab$names))

# Remove all the double space
temp_org_tab$names <- str_replace_all(temp_org_tab$names, "\\s{2,}", " ")

# Add missing space after a period
temp_org_tab$names <- str_replace_all(temp_org_tab$names, "(\\w)\\.(\\w)", "\\1. \\2")

# List of known scientific names and their replacements
scientific_name_map <- list(
  "-" = NA,
  "?" = NA,
  "A. BAUMANNII" = "Acinetobacter baumannii",
  "ACIDOBACILLUS FERROOXIDANS" = "Acidithiobacillus ferrooxidans",
  "ACINETOBACTER" = "Acinetobacter spp.",
  "ACINETOBACTER LWOFFII" = "Acinetobacter lwoffii",
  "ACTINOBACILLUS MURIS" = "Actinobacillus muris",
  "ARTHROBACTER" = "Arthrobacter spp.",
  "BACILLUS MEGATERIUM" = "Bacillus megaterium",
  "BCG" = "Mycobacterium bovis BCG",
  "BEIJING" = NA,  # If referring to a lineage or variant, specify
  "BLAER1" = "BLaER1 Human B-cell Precursor Leukemia Cell Line",
  "BLAER1 GFP-" = "BLaER1 Human B-cell Precursor Leukemia Cell Line (GFP)",
  "BLAU D5" = NA,
  "BLAU H10" = NA,
  "BLAU H11" = NA,
  "BOLLETII" = "Mycobacterium bolletii",
  "C. METALLIDURANS" = "Cupriavidus metallidurans",
  "C. PNEUMONIAE" = "Chlamydia pneumoniae",
  "C. TRACHOMATIS" = "Chlamydia trachomatis",
  "CHLAMYDIA" = "Chlamydia spp.",
  "CHLAMYDIA PNEUMONIAE" = "Chlamydia pneumoniae",
  "CHLAMYDIA TRACHOMATIS" = "Chlamydia trachomatis",
  "COMAMONAS SP." = "Comamonas sp.",
  "CORYNEBACTERIUM SPEC" = "Corynebacterium sp.",
  "COVID-19" = "SARS-CoV-2",
  "CUPRIAVIDUS METALLIDURANS" = "Cupriavidus metallidurans",
  "EURO-AMERICAN" = NA,  # Clarification needed if lineage-related
  "H. HAEMOLYTICUS" = "Haemophilus haemolyticus",
  "H. INFLUENZAE" = "Haemophilus influenzae",
  "H37RV" = "Mycobacterium tuberculosis H37Rv",
  "HAARLEM" = NA,  # Lineage specification needed
  "HAEMOPHILUS HAEMOLYTICUS" = "Haemophilus haemolyticus",
  "HAEMOPHILUS INFLUENZAE" = "Haemophilus influenzae",
  "HAEMOPHILUS PARAINFLUENZAE" = "Haemophilus parainfluenzae",
  "HOMO SAPIENS" = "Homo sapiens",
  "HUMAN 10 NG" = "Homo sapiens (10ng)",  # General human entry
  "K. PNEUMONIAE" = "Klebsiella pneumoniae",
  "KOCURIA KRISTINAE" = "Kocuria kristinae",
  "LACTOBACILLUS SPEC." = "Lactobacillus sp.",
  "LACTOCOCCUS LACTIS" = "Lactococcus lactis",
  "LEGIONELLA" = "Legionella spp.",
  "LEGIONELLA PNEUMOPHILA" = "Legionella pneumophila",
  "LEPTOSPIRILLUM FERRIPHILUM" = "Leptospirillum ferriphilum",
  "M. ABSCESSUS" = "Mycobacterium abscessus",
  "M. ABSCESSUS/ IMMUNOGENSE (PRIMÄRKULTUR)" = "Mycobacterium abscessus/immunogense (primary culture)",
  "M. AFRICANUM" = "Mycobacterium africanum",
  "M. AVIUM" = "Mycobacterium avium",
  "M. AVIUM COMPLEX" = "Mycobacterium avium complex",
  "M. AVIUM COMPLEX?" = "Mycobacterium avium complex",
  "M. AVIUM C" = "Mycobacterium avium complex",
  "M. BOVIS" = "Mycobacterium bovis",
  "M. BOVIS AFRICANUM" = "Mycobacterium bovis africanum",
  "M. BOVIS_CAPRAE" = "Mycobacterium -bovis, -caprae",
  "M. BOVIS/BOVIS" = "Mycobacterium bovis",
  "M. BASILIENSE" = "Mycobacterium basiliense",
  "M. BASILLENCE" = "Mycobacterium basiliense",
  "M. (PARA)INTRACELLULARE, CHIMAERA, YONGONENSE, INDICUS PRANII" = "Mycobacterium -(para)intracellulare, -chimaera, -yonogonense, -indicus pranii",
  "M. CANETTI" = "Mycobacterium canettii",
  "M. CANETTII" = "Mycobacterium canettii",
  "M. CAPRAE" = "Mycobacterium caprae",
  "M. CHELONAE" = "Mycobacterium chelonae",
  "M. CHIMAERA" = "Mycobacterium chimaera",
  "M. FORTUITUM" = "Mycobacterium fortuitum",
  "M. GASTRI" = "Mycobacterium gastri",
  "M. GASTRI/KANSASII" = "Mycobacterium -gastri, -kansasii",
  "M. GASTRI/KANSASII-LIKE (N=2)" = "Mycobacterium -gastri, -kansasii-like (2 nt)",
  "M. GILVUM" = "Mycobacterium gilvum",
  "M. GORDONAE" = "Mycobacterium gordonae",
  "M. INTRACELLULARE" = "Mycobacterium intracellulare",
  "M. INTRACELULLARE" = "Mycobacterium intracellulare",
  "M. INTRACELLULARE (PRIMÄRKULTUR)" = "Mycobacterium intracellulare (primary culture)",
  "M. INTRACELLULARE, M. MARSEILLENSE, M. YONGONENSE" = "Mycobacterium -intracellulare, -marseillense, -yonogonense",
  "M. INTRACELLULARE, M. YONGONENSE, MARSEILLENSE" = "Mycobacterium -intracellulare, -marseillense, -yonogonense",
  "M. INTRACELLULARE GRUPPE" = "Mycobacterium intracellulare group",
  "M. KANSASII" = "Mycobacterium kansasii",
  "M. KANSASII (PRIMÄRKULTUR)" = "Mycobacterium kansasii (primary culture)",
  "M. KANSASII-LIKE (N=1)" = "Mycobacterium kansasii-like (1 nt)",
  "M. KANSASII/GASTRI" = "Mycobacterium -gastri, -kansasii",
  "M. KANSASII/GASTRI-LIKE (1 NT)" = "Mycobacterium -kansasii, -gastri-like (1 nt)",
  "M. MARINUM (PRIMÄRKULTUR)" = "Mycobacterium marinum (primary culture)",
  "M. MARSEILLENSE, M. YONGONENSE, M. INTRACELLULARE" = "Mycobacterium -intracellulare, -marseillense, -yonogonense",
  "M. ORYGIS" = "Mycobacterium orygis",
  "M. SPEZIES" = "Mycobacterium spp.",
  "M. SPECIES" = "Mycobacterium spp.",
  "M. TUBERCULOSIS" = "Mycobacterium tuberculosis",
  "M. TUBERCULOSIS H37RV" = "Mycobacterium tuberculosis H37Rv",
  "M. TUBERCULOSIS (ERDMAN)" = "Mycobacterium tuberculosis (Erdman)",
  "M. UNBEKANNTE ART" = NA,
  "M. TUBERCULOSIS, M. AVIUM" = "Mycobacterium -tuberculosis, -avium",
  "M. CHELONE" = "Mycobacterium chelonae",
  "M. CHIMARERA" = "Mycobacterium chimaera",
  "M. MALMOENSE" = "Mycobacterium malmoense",
  "M. MICROTI" = "Mycobacterium microti",
  "M. PINNIPEDII" = "Mycobacterium pinnipedii",
  "M. SIMIAE" = "Mycobacterium simiae",
  "M. SMEGMATIS" = "Mycobacterium smegmatis",
  "M. SZULGAI" = "Mycobacterium szulgai",
  "M. TRIPLEX" = "Mycobacterium triplex",
  "M. ULCERANS" = "Mycobacterium ulcerans",
  "M. VULNERIS" = "Mycobacterium vulneris",
  "M. BOVIS BCG, ATCC" = "Mycobacterium bovis BCG, ATCC",
  "M. CELATUM" = "Mycobacterium celatum",
  "M. CHIMAERA VARIANTE" = "Mycobacterium chimaera variante",
  "M. CHIMEARA" = "Mycobacterium chimaera",
  "M. INTERJECTUM" = "Mycobacterium interjectum",
  "M. INTRACELLUAR" = "Mycobacterium intracellulare",
  "M. MARINUM/ULCERANS" = "Mycobacterium -marinum, -ulcerans",
  "M. XENOPI" = "Mycobacterium xenopi",
  "MAC" = "Mycobacterium avium complex",
  "MASSILIENSE" = "Mycobacterium massiliense",
  "MICROBE MIX" = NA,
  "MIKROBIOM" = NA,
  "MTB H37RV" = "Mycobacterium tuberculosis H37Rv",
  "MYCOBACTERIUM BOVIS" = "Mycobacterium bovis",
  "MYCOBACTERIUM CHIMAERA" = "Mycobacterium chimaera",
  "MYCOBACTERIUM MICROTI" = "Mycobacterium microti",
  "MYCOBACTERIUM SMEGMATIS" = "Mycobacterium smegmatis",
  "NA" = "undefined by responsible person",
  "NTM" = "Nontuberculous mycobacteria",
  "P. AERUGINOSA" = "Pseudomonas aeruginosa",
  "PANTOEA AGGLOMERANS" = "Pantoea agglomerans",
  "PASTEURELLA PNEUMOTROPICA" = "Pasteurella pneumotropica",
  "PLASMID" = NA,
  "PSEUDOALTEROMONAS TUNICATA" = "Pseudoalteromonas tunicata",
  "S-TYPE" = NA,
  "S. HOMINIS" = "Staphylococcus hominis",
  "S. MALTOPHILIA" = "Stenotrophomonas maltophilia",
  "SARS-COV-2" = "SARS-CoV-2",
  "SERRATIA \"BATHROOM\"" = "Serratia spp.",
  "SERRATIA GRIMESII" = "Serratia grimesii",
  "STAPHYLOCOCCUS EPIDERMIS" = "Staphylococcus epidermidis",
  "STAPHYLOCOCCUS HAEMOLYTICUS" = "Staphylococcus haemolyticus",
  "STAPHYLOCOCCUS PASTEURI" = "Staphylococcus pasteuri",
  "STAPHYLOCOCCUS XYLOSUS" = "Staphylococcus xylosus",
  "STENOTROPHOMONAS MALTOPHILIA" = "Stenotrophomonas maltophilia",
  "STREPTOCOCCUS PYOGENES" = "Streptococcus pyogenes",
  "TB (PRIMÄRKULTUR)" = "Mycobacterium tuberculosis (Primary culture)",
  "TB (WACHSTUMSKONTROLLE)" = "Mycobacterium tuberculosis (Growth control)",
  "TOTAL BACTERIA GENES" = NA,
  "UNKNOWN (M. HEIDELBERGENSE)" = "Mycobacterium heidelbergense",
  "URAL" = NA,
  "WEST-AFRICA 1" = NA,
  "WEST-AFRICA 2" = NA,
  "X-TYPE" = NA
)

# Standardize to scientific names using the map
data <- temp_org_tab %>%
  rowwise() %>%
  mutate(names = ifelse(names %in% names(scientific_name_map),
                        scientific_name_map[[names]], 
                        names))














# what are those digits doing in the name section
temp_digit_check_index = which(grepl(pattern = str_c("\\d", "-", sep = "|"), x = temp_org_tab$names))

# Get the libs with owner information
temp_org_missing = temp_org_tab[temp_digit_check_index, ]

# Remove all the double space
temp_org_tab$names <- gsub(x = temp_org_tab$names, pattern = ".*\\s\\s.*", replacement = " ")  # Removes everything after a space






# Replcae with a character identifier
temp_org_tab$names[temp_digit_check_index] <- "check study"


which(grepl(pattern = "*\\s.*", x = temp_org_tab$names))


"*\\s.*"

# temp_missing_organism = which(all_libs$organism %in% NA)
# 
# aa = all_libs[temp_missing_organism, ]

# Entries with missing Borstel ID
# which(grepl(pattern = "ETB", x = toupper(all_libs$borstel_ID)))