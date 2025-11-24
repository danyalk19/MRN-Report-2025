

rm(list = ls())

################# Set paths #################################

path_main        = ""
path_data        = paste0(path_main,"data/")


################### Load requires libraries ##################

# Store packages in a vector
packages <- c(
  "tidyverse", "ggExtra", "ggthemes",
  "ggrepel","openxlsx", "lubridate", "writexl", "zoo",
  "janitor", "readxl", "GGally", "Hmisc", "mice",
  "sjmisc", "h2o", "recipes", "parsnip", "rsample", "workflows",
  "tune", "yardstick", "dials", "vip", "RcppRoll", "slider",
  "tseries", "ggnewscale", "haven", "foreign",  "hunspell"
)

# Function to install and load packages
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
  
  print(paste(p, "is loaded"))
}
rm(p, packages)



################### Import data ##################

### HESA SUBJECT MAPPINGS
# This loads the HESA subject-to-subject broad mappings.
# If the 'mappings.xlsx' file is found, it will be loaded.
# If NOT found, the script will generate a hard-coded data frame to ensure
# the analysis is fully reproducible.


mapping_filepath <- file.path(path_data, "hesa subject mappings.xlsx")

if (file.exists(mapping_filepath)) {

  hesa_subject_mapping <- read_excel(mapping_filepath)
  print("Successfully loaded HESA subject mappings from file.")
  
} else {

  print("HESA subject mappings file not found. Creating a hard-coded version for reproducibility.")
  
  library(tibble)
  
  hesa_subject_mapping <- tribble(
    ~subject, ~subject_broad, ~subject_stem,
    "Medicine and dentistry", "Medicine, dentistry and veterinary sciences", "STEM",
    "Veterinary sciences", "Medicine, dentistry and veterinary sciences", "STEM",
    "Subjects allied to medicine", "Nursing and allied health", "STEM",
    "Biological and sport sciences", "Natural and mathematical sciences", "STEM",
    "Physical sciences", "Natural and mathematical sciences", "STEM",
    "Mathematical sciences", "Natural and mathematical sciences", "STEM",
    "Engineering and technology", "Engineering, technology and computing", "STEM",
    "Computing", "Engineering, technology and computing", "STEM",
    "Agriculture, food and related studies", "Natural and built environment", "Non-STEM",
    "Architecture, building and planning", "Natural and built environment", "Non-STEM",
    "Geography, earth and environmental studies", "Natural and built environment", "Non-STEM",
    "Social sciences", "Law and social sciences", "Non-STEM",
    "Law", "Law and social sciences", "Non-STEM",
    "Business and management", "Business and management", "Non-STEM",
    "Language and area studies", "Humanities and languages", "Non-STEM",
    "Historical, philosophical and religious studies", "Humanities and languages", "Non-STEM",
    "Combined and general studies", "Humanities and languages", "Non-STEM",
    "Media, journalism and communications", "Humanities and languages", "Non-STEM",
    "Education and teaching", "Education and teaching", "Non-STEM",
    "Design, and creative and performing arts", "Design, and creative and performing arts", "Non-STEM",
    "Psychology", "Law and social sciences", "Non-STEM"
  )
  print("Created hard-coded 'hesa_subject_mapping' object.")
}


#### OFFICE FOR STUDENTS DATA
# The raw OfS data is required for the backcasting analysis.
# Please download it from the OfS website and place the CSV file in the
# 'data/' directory.
# Link: https://www.officeforstudents.org.uk/data-and-analysis/student-characteristics-data/

ofs_filepath <- file.path(path_data, "Student-characteristics-populations-2024.csv")

if (file.exists(ofs_filepath)) {
  
  ofs_raw <- read.csv(ofs_filepath)
  print(paste("Successfully loaded public OfS data from:", ofs_filepath))
  
} else {
  
  warning(paste("OfS data file not found at:", ofs_filepath))
  print("Please download the required file and place it in the 'data/' directory.")
  
}


### MRN SURVEY DATA
# The 'Wide (filtered).xlsx' file contains proprietary survey data and is
# NOT included in the public repository.
# If the file is found, it will be loaded.
# If NOT found, the script will generate hard-coded data frames containing
# the key aggregate data needed to reproduce the report's final results.

mrn_survey_filepath <- file.path(path_data, "Wide (filtered).xlsx")

if (file.exists(mrn_survey_filepath)) {

  wide <- read_xlsx(mrn_survey_filepath)
  print("Successfully loaded proprietary MRN survey data.")
  
  subject_transitions <- wide %>%
    select(subject_hesa, postgrad_subject_hesa) %>%
    filter(!is.na(postgrad_subject_hesa) & !is.na(subject_hesa)) %>%
    left_join(select(hesa_subject_mapping, -subject_stem), by = c("subject_hesa" = "subject")) %>%
    rename(subject_from = subject_broad) %>%
    left_join(select(hesa_subject_mapping, -subject_stem), by = c("postgrad_subject_hesa" = "subject")) %>%
    rename(subject_to = subject_broad) %>%
    group_by(subject_from, subject_to) %>%
    summarise(count = n(), .groups = "drop")
  
  group_X <- wide %>%
    filter(year != "5", postgrad_degree_type != "Postgraduate conversion course", domicile == "Home", q0 > 0.5) %>%
    mutate(q0 = ifelse(q0 == 0, 0.001, q0),
           years_after_graduation = as.numeric(year) - year_graduation,
           p = 1 - (1 - q0)^(1 / years_after_graduation)) %>%
    group_by(postgrad_degree_type) %>%
    summarise(p = mean(p, na.rm = TRUE), .groups = "drop") %>%
    mutate(X_muslim = round(1 / p, 0),
           X_nonmuslim = X_muslim - 1) %>%
    pivot_longer(cols = starts_with("X"), names_to = "muslim", values_to = "X") %>%
    mutate(level = case_when(postgrad_degree_type == "Doctorate" ~ "Masters to PhD",
                             postgrad_degree_type == "Masters degree" ~ "UG to Masters"),
           muslim = ifelse(grepl("non", muslim), "Non-Muslim", "Muslim")) %>%
    select(level, muslim, X)
  
} else {
  # For public users: generate hard-coded objects to ensure reproducibility
  print("Proprietary MRN survey data not found. Creating hard-coded summary objects to ensure reproducibility.")
  
  # [cite_start]Hard-code the delay parameters based on Table 3.1.4.1 from the report [cite: 264]
  group_X <- tibble(
    level = c("UG to Masters", "UG to Masters", "Masters to PhD", "Masters to PhD"),
    muslim = c("Muslim", "Non-Muslim", "Muslim", "Non-Muslim"),
    X = c(3, 2, 4, 3)
  )
  print("Created 'group_X' delay parameters.")
  
  # Hard-code the subject transition matrix based on Figure 3.1.7.1 from the report
  library(tibble)
  
  subject_transitions <- tribble(
    ~subject_from, ~subject_to, ~count,
    "Business and management", "Business and management", 20,
    "Business and management", "Design, and creative and performing arts", 1,
    "Business and management", "Education and teaching", 1,
    "Business and management", "Engineering, technology and computing", 7,
    "Business and management", "Law and social sciences", 10,
    "Business and management", "Medicine, dentistry and veterinary sciences", 2,
    "Business and management", "Nursing and allied health", 1,
    "Design, and creative and performing arts", "Business and management", 1,
    "Design, and creative and performing arts", "Design, and creative and performing arts", 16,
    "Design, and creative and performing arts", "Nursing and allied health", 1,
    "Education and teaching", "Education and teaching", 6,
    "Education and teaching", "Law and social sciences", 3,
    "Engineering, technology and computing", "Business and management", 13,
    "Engineering, technology and computing", "Education and teaching", 2,
    "Engineering, technology and computing", "Engineering, technology and computing", 116,
    "Engineering, technology and computing", "Humanities and languages", 1,
    "Engineering, technology and computing", "Law and social sciences", 3,
    "Engineering, technology and computing", "Medicine, dentistry and veterinary sciences", 1,
    "Engineering, technology and computing", "Natural and built environment", 1,
    "Engineering, technology and computing", "Natural and mathematical sciences", 4,
    "Engineering, technology and computing", "Nursing and allied health", 2,
    "Humanities and languages", "Business and management", 1,
    "Humanities and languages", "Education and teaching", 4,
    "Humanities and languages", "Engineering, technology and computing", 2,
    "Humanities and languages", "Humanities and languages", 29,
    "Humanities and languages", "Law and social sciences", 3,
    "Humanities and languages", "Nursing and allied health", 2,
    "Law and social sciences", "Business and management", 12,
    "Law and social sciences", "Education and teaching", 1,
    "Law and social sciences", "Engineering, technology and computing", 8,
    "Law and social sciences", "Humanities and languages", 4,
    "Law and social sciences", "Law and social sciences", 104,
    "Law and social sciences", "Nursing and allied health", 2,
    "Medicine, dentistry and veterinary sciences", "Business and management", 6,
    "Medicine, dentistry and veterinary sciences", "Design, and creative and performing arts", 1,
    "Medicine, dentistry and veterinary sciences", "Education and teaching", 3,
    "Medicine, dentistry and veterinary sciences", "Engineering, technology and computing", 8,
    "Medicine, dentistry and veterinary sciences", "Humanities and languages", 3,
    "Medicine, dentistry and veterinary sciences", "Law and social sciences", 8,
    "Medicine, dentistry and veterinary sciences", "Medicine, dentistry and veterinary sciences", 77,
    "Medicine, dentistry and veterinary sciences", "Natural and built environment", 1,
    "Medicine, dentistry and veterinary sciences", "Natural and mathematical sciences", 10,
    "Medicine, dentistry and veterinary sciences", "Nursing and allied health", 10,
    "Natural and built environment", "Design, and creative and performing arts", 1,
    "Natural and built environment", "Education and teaching", 1,
    "Natural and built environment", "Engineering, technology and computing", 2,
    "Natural and built environment", "Humanities and languages", 1,
    "Natural and built environment", "Natural and built environment", 25,
    "Natural and built environment", "Natural and mathematical sciences", 1,
    "Natural and built environment", "Nursing and allied health", 1,
    "Natural and mathematical sciences", "Business and management", 7,
    "Natural and mathematical sciences", "Education and teaching", 3,
    "Natural and mathematical sciences", "Engineering, technology and computing", 9,
    "Natural and mathematical sciences", "Humanities and languages", 2,
    "Natural and mathematical sciences", "Law and social sciences", 2,
    "Natural and mathematical sciences", "Medicine, dentistry and veterinary sciences", 8,
    "Natural and mathematical sciences", "Natural and built environment", 1,
    "Natural and mathematical sciences", "Natural and mathematical sciences", 61,
    "Natural and mathematical sciences", "Nursing and allied health", 5,
    "Nursing and allied health", "Business and management", 1,
    "Nursing and allied health", "Engineering, technology and computing", 3,
    "Nursing and allied health", "Law and social sciences", 1,
    "Nursing and allied health", "Medicine, dentistry and veterinary sciences", 5,
    "Nursing and allied health", "Natural and mathematical sciences", 1,
    "Nursing and allied health", "Nursing and allied health", 27
  )
  
  print("Created 'subject_transitions' matrix.")
}

### Other objects

# Get a unique list of broad subjects for creating matrices
subjects_broad_list <- unique(subject_transitions$subject_from)

# Create a transition matrix assuming NO subject switching
no_switching_mat <- tibble(
  "subject_from" = subjects_broad_list,
  "subject_to" = subjects_broad_list,
  "count" = 1
)
print("Created 'no_switching_mat'.")

# Create a transition matrix assuming MAX flexible subject switching
max_flex_mat <- expand.grid(
  "subject_to" = subjects_broad_list,
  "subject_from" = subjects_broad_list
) %>%
  mutate(count = 1)
print("Created 'max_flex_mat'.")

# Define 1-year and 3-year progression rate parameters for sensitivity analysis
year_1max <- tibble(
  muslim = c("Muslim", "Muslim", "Non-Muslim", "Non-Muslim"),
  level = c("UG to Masters", "Masters to PhD", "UG to Masters", "Masters to PhD"),
  X = c(1, 1, 1, 1)
)

year_3max <- tibble(
  muslim = c("Muslim", "Muslim", "Non-Muslim", "Non-Muslim"),
  level = c("UG to Masters", "Masters to PhD", "UG to Masters", "Masters to PhD"),
  X = c(3, 3, 3, 3)
)
