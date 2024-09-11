data <- data %>%
  mutate(organism = str_replace_all(organism, "\\s{2,}", " "),        # Replace double spaces
         organism = str_replace_all(organism, "(\\w)\\.(\\w)", "\\1. \\2")) # Add missing space after a period

# Comprehensive list of known scientific names and their replacements
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
  "BLAER1" = NA,
  "BLAER1 GFP-" = NA,
  "BLAU D5" = NA,
  "BLAU H10" = NA,
  "BLAU H11" = NA,
  "BOLLETII" = NA,
  "C. METALLIDURANS" = "Cupriavidus metallidurans",
  "C. PNEUMONIAE" = "Chlamydia pneumoniae",
  "C. TRACHOMATIS" = "Chlamydia trachomatis",
  "CHLAMYDIA" = "Chlamydia spp.",
  "CHLAMYDIA PNEUMONIAE" = "Chlamydia pneumoniae",
  "CHLAMYDIA TRACHOMATIS" = "Chlamydia trachomatis",
  "COMAMONAS SP." = "Comamonas spp.",
  "CORYNEBACTERIUM SPEC" = "Corynebacterium spp.",
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
  "HUMAN 10 NG" = "Homo sapiens",  # General human entry
  "K. PNEUMONIAE" = "Klebsiella pneumoniae",
  "KOCURIA KRISTINAE" = "Kocuria kristinae",
  "LACTOBACILLUS SPEC." = "Lactobacillus spp.",
  "LACTOCOCCUS LACTIS" = "Lactococcus lactis",
  "LEGIONELLA" = "Legionella spp.",
  "LEGIONELLA PNEUMOPHILA" = "Legionella pneumophila",
  "LEPTOSPIRILLUM FERRIPHILUM" = "Leptospirillum ferriphilum",
  "M. ABSCESSUS" = "Mycobacterium abscessus",
  "M. AFRICANUM" = "Mycobacterium africanum",
  "M. AVIUM" = "Mycobacterium avium",
  "M. AVIUM COMPLEX" = "Mycobacterium avium complex",
  "M. BOVIS" = "Mycobacterium bovis",
  "M. CANETTI" = "Mycobacterium canettii",
  "M. CHELONAE" = "Mycobacterium chelonae",
  "M. CHIMAERA" = "Mycobacterium chimaera",
  "M. FORTUITUM" = "Mycobacterium fortuitum",
  "M. GORDONAE" = "Mycobacterium gordonae",
  "M. INTRACELLULARE" = "Mycobacterium intracellulare",
  "M. KANSASII" = "Mycobacterium kansasii",
  "M. MALMOENSE" = "Mycobacterium malmoense",
  "M. MICROTI" = "Mycobacterium microti",
  "M. PINNIPEDII" = "Mycobacterium pinnipedii",
  "M. SIMIAE" = "Mycobacterium simiae",
  "M. SMEGMATIS" = "Mycobacterium smegmatis",
  "M. SZULGAI" = "Mycobacterium szulgai",
  "M. TRIPLEX" = "Mycobacterium triplex",
  "M. TUBERCULOSIS" = "Mycobacterium tuberculosis",
  "M. ULCERANS" = "Mycobacterium ulcerans",
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
  "NA" = NA,
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
data <- data %>%
  rowwise() %>%
  mutate(organism = ifelse(toupper(organism) %in% names(scientific_name_map),
                           scientific_name_map[[toupper(organism)]],
                           organism))

# For entries with human samples mixed with Mycobacterium tuberculosis:
for (human_mt in c("HUMAN 9,99999NG/ M.  TUBERCULOSIS 0,00001NG", "HUMAN 9,9999NG/ M.  TUBERCULOSIS 0,0001NG", 
                   "HUMAN 9,999NG/ M.  TUBERCULOSIS 0,001NG", "HUMAN 9,99NG/ M.  TUBERCULOSIS 0,01NG", 
                   "HUMAN 9,9NG/ M.  TUBERCULOSIS 0,1NG", "HUMAN 90NG/ M.  TUBERCULOSIS 10NG", 
                   "HUMAN 900NG/ M.  TUBERCULOSIS 100NG")) {
  scientific_name_map[[human_mt]] <- "Homo sapiens / Mycobacterium tuberculosis (mixed sample)"
}

# Identify problematic entries (with digits and no valid organism names)
problematic_entries <- data %>%
  filter(grepl("\\d", organism) & !grepl("\\b[A-Za-z]+\\b", organism))  # Entries with digits but no proper organism names

# Print the problematic entries
print(problematic_entries)