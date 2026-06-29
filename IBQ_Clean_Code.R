###### IBQ R Code ########

#INFORMATION


# ====================
# Load packages
# ====================

# Load required packages for data cleaning, manipulation, and psychometric analyses
library(data.table)
library(psych)
library(lavaan)
library(tidyr)
library(dplyr)
library(psych)
library(GPArotation)
library(stringr)
library(haven)

# ====================
# Set working directory
# ====================

# Set project directory
setwd("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI")

# Store run date and analyst initials for file naming
date_str <- format(Sys.Date(), "%m-%d-%Y")
yourinitials <- "YK" #[MODIFY YOUR INITIALS]

###### Modify and double check all file paths ##########

# ====================
# Emotion study (EMO): Data cleaning
# ====================

# Import raw EMO IBQ data
emo_orig <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/raw/emo_ibq_RAW.csv")
emo <- emo_orig

# Standardize participant ID variable name
emo_clean <- emo %>%
  rename(studyid = subj)

# Standardize IBQ item naming convention
colnames(emo_clean) <- gsub("^ibqsf", "ibq", colnames(emo_clean))

# Import age information
emo_demographics <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/input/emo_demographics_age.csv")

# Merge age information into EMO dataset
emo_clean <- emo_clean %>%
  left_join(
    emo_demographics %>% dplyr::select(subj, age_days, age_months),
    by = c("studyid" = "subj")
  )

# Add study identifier
emo_clean <- emo_clean %>%
  mutate(study = "emo")

# Reorder variables for consistency
emo_clean <- emo_clean %>%
  dplyr::select(studyid, age_months, age_days, study, everything())

# Create harmonized age-group variable
emo_clean <- emo_clean %>%
  mutate(age_group = case_when(
    age_months >= 2 & age_months <= 3.99 ~ 3,
    age_months >= 4 & age_months <= 5.99 ~ 5,
    age_months >= 6 & age_months <= 8.49 ~ 7,
    age_months >= 8.50 & age_months <= 10.99 ~ 10,
    age_months >= 11 & age_months <= 12.99 ~ 12,
    TRUE ~ NA_real_
  ))

# Verify age-group counts
emo_clean %>%
  mutate(age_group = case_when(
    age_months >= 2 & age_months <= 3.99 ~ 3,
    age_months >= 4 & age_months <= 5.99 ~ 5,
    age_months >= 6 & age_months <= 8.49 ~ 7,
    age_months >= 8.50 & age_months <= 10.99 ~ 10,
    age_months >= 11 & age_months <= 12.99 ~ 12,
    TRUE ~ NA_real_
  )) %>%
  count(age_group)

# Reorder variables
emo_clean <- emo_clean %>%
  dplyr::select(studyid, age_group, age_months, study, everything())

# Remove study-specific administrative variables
emo_clean <- emo_clean %>%
  dplyr::select(-redcap_event_name)

# Recode study-specific missing value indicators
emo_clean[emo_clean == 99] <- NA
emo_clean[emo_clean == 9] <- NA

# Export cleaned EMO dataset
write.csv(emo_clean, file = "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output/emo_clean.csv", row.names = FALSE)

# ====================
# BabySteps study (BSP): Data cleaning
# ====================

# Import raw BSP IBQ data
bs_orig <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/raw/bsp_ibq_RAW.csv") 
bs <- bs_orig

# Remove study-specific administrative variables
bs_clean <- bs %>%
  dplyr::select(-redcap_event_name)

# Create harmonized age-group variable
bs_clean <- bs_clean %>%
  mutate(age_group = case_when(
    ibq_age_months >= 2 & ibq_age_months <= 3.99 ~ 3,
    ibq_age_months >= 4 & ibq_age_months <= 5.99 ~ 5,
    ibq_age_months >= 6 & ibq_age_months <= 8.49 ~ 7,
    ibq_age_months >= 8.5 & ibq_age_months <= 10.99 ~ 10,
    ibq_age_months >= 11 & ibq_age_months <= 12.99 ~ 12,
    TRUE ~ NA_real_
  ))

# Verify age-group counts
bs_clean %>%
  mutate(age_group = case_when(
    ibq_age_months >= 2 & ibq_age_months <= 3.99 ~ 3,
    ibq_age_months >= 4 & ibq_age_months <= 5.99 ~ 5,
    ibq_age_months >= 6 & ibq_age_months <= 8.49 ~ 7,
    ibq_age_months >= 8.5 & ibq_age_months <= 10.99 ~ 10,
    ibq_age_months >= 11 & ibq_age_months <= 12.99 ~ 12,
    TRUE ~ NA_real_
  )) %>%
  count(age_group)

# Retain harmonized variables for cross-study analyses
bs_clean <- bs_clean %>%
  dplyr::select(studyid, age_group, ibq_age_months, ibq1:ibq91)

# Add study identifier
bs_clean <- bs_clean %>%
  mutate(study = "bsp")

# Reorder variables for consistency
bs_clean <- bs_clean %>%
  dplyr::select(studyid, age_group, ibq_age_months, study, everything())

# Export cleaned BSP dataset
write.csv(bs_clean, file = "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output/bs_clean.csv", row.names = FALSE)


# ====================
# ISP2 study: Data cleaning
# ====================

# Import raw ISP2 IBQ data
isp_orig <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/raw/isp2_ibq_RAW.csv") 
isp <- isp_orig

# Standardize participant ID variable name
isp_clean <- isp %>%
  rename(studyid = study_id)

# Import age information from the 3-month assessment
isp_3_age <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/input/isp2_3mo_age.csv")

# Standardize event name variable for merging
isp_3_age <- isp_3_age %>%
  rename(redcap_event_name = event_name)

# Harmonize event labels across datasets
isp_3_age <- isp_3_age %>%
  mutate(
    redcap_event_name = case_when(
      redcap_event_name == "3" ~ "3_month_data_arm_3",
    ))

# Merge age information from the 3-month assessment
isp_clean <- isp_clean %>%
  left_join(
    isp_3_age %>% dplyr::select(studyid, redcap_event_name, asq_age_in_days_3),
    by = c("studyid", "redcap_event_name")
  )

# Calculate age in months from age in days
isp_clean$age_months_3 <- isp_clean$asq_age_in_days_3 / 30.44

# Create a single age variable using available age sources
isp_clean <- isp_clean %>%
  mutate(
    age_months = coalesce(age_months_3, ibq2_age_in_months)
  )

# Create harmonized age-group variable
isp_clean <- isp_clean %>%
  mutate(age_group = case_when(
    age_months >= 2 & age_months <= 3.99 ~ 3,
    age_months >= 4 & age_months <= 5.99 ~ 5,
    age_months >= 6 & age_months <= 8.49 ~ 7,
    age_months >= 8.50 & age_months <= 10.99 ~ 10,
    age_months >= 11 & age_months <= 12.99 ~ 12,
    TRUE ~ NA_real_
  ))

# Verify age-group counts
isp_clean %>%
  mutate(age_group = case_when(
    age_months >= 2 & age_months <= 3.99 ~ 3,
    age_months >= 4 & age_months <= 5.99 ~ 5,
    age_months >= 6 & age_months <= 8.49 ~ 7,
    age_months >= 8.50 & age_months <= 10.99 ~ 10,
    age_months >= 11 & age_months <= 12.99 ~ 12,
    TRUE ~ NA_real_
  )) %>%
  count(age_group)

# Remove study-specific variables not needed for analysis
isp_clean <- isp_clean %>%
  dplyr::select(
    -redcap_event_name,
    -ibq2_age_in_days,
    -ibq2_age_in_years,
    -ibq2_final_data_complete,
    -asq_age_in_days_3,
    -age_months_3
  )

# Standardize IBQ item naming convention
colnames(isp_clean) <- gsub("^ibq\\d+_ibqsf(\\d+)_f$", "ibq\\1", colnames(isp_clean))

# Add study identifier
isp_clean <- isp_clean %>%
  mutate(study = "isp")

# Reorder variables for consistency
isp_clean <- isp_clean %>%
  dplyr::select(studyid, age_group, age_months, study, ibq1:ibq91)

# Recode study-specific missing value indicators
isp_clean[isp_clean == 999] <- NA

# Export cleaned ISP2 dataset
write.csv(isp_clean, file = "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output/isp_clean.csv", row.names = FALSE)

# ====================
# Harmonize datasets and combine studies
# ====================

# Standardize age variable names across datasets
bs_clean <- bs_clean %>%
  rename(age_months = ibq_age_months)

# Remove participants outside target age groups
emo_clean <- emo_clean %>%
  filter(!is.na(age_group))

bs_clean <- bs_clean %>%
  filter(!is.na(age_group))

isp_clean <- isp_clean %>%
  filter(!is.na(age_group))

# Create copies for dataset harmonization and merging
emo_merge <- emo_clean
bs_merge <- bs_clean
isp_merge <- isp_clean

# Ensure participant IDs have consistent data types before combining datasets
emo_merge <- emo_merge %>%
  mutate(studyid = as.character(studyid))

bs_merge <- bs_merge %>%
  mutate(studyid = as.character(studyid))

isp_merge <- isp_merge %>%
  mutate(studyid = as.character(studyid))

# Combine harmonized datasets across studies
all_datasets <- bind_rows(emo_merge, bs_merge, isp_merge)

# Export merged dataset
file1 <- file.path("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output", paste0("all_datasets_", date_str,"_", yourinitials, ".csv", sep=""))
write.csv(all_datasets, file = file1, row.names = FALSE)


# ====================
# Clinical judgment data
# ====================

# Import clinical judgment datasets
ispclin_orig <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/clin_judgement/isp_clinjudge.csv") 
ispclin <- ispclin_orig
bspclin_orig <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/clin_judgement/BSP_Clinical-judgement_2026-02-06.csv")
bspclin <- bspclin_orig
emoclin_orig <-read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/clin_judgement/emo_demographics.csv")
emoclin <- emoclin_orig

# Confirm EMO participants do not appear in clinical judgment files
common_ids <- intersect(emo_merge$studyid, emoclin_orig$studyid)
length(common_ids)

# Select diagnostic variables for merging
ispclinselected <- ispclin[, c("study_id", "ASD_TD_DD")]

# Create harmonized diagnostic coding
ispclinselected$dx <- ifelse(
  ispclin$ASD_TD_DD == "TD", 0,
  ifelse(
    ispclin$ASD_TD_DD == "ASD", 1,
    ifelse(
      ispclin$ASD_TD_DD == "DD", 2,
      ifelse(ispclin$ASD_TD_DD == "UNKNOWN/NoGroup", 3, NA)
    )
  )
)

# Prepare BSP diagnostic data for merging
bspclinselected <- bspclin[, c("studyid", "dx")]

# Merge ISP diagnostic information
all_withclin <- merge(
  all_datasets,
  ispclinselected,
  by.x = "studyid",
  by.y = "study_id",
  all.x = TRUE
)

# Merge BSP diagnostic information
all_withclin <- merge(
  all_withclin,
  bspclinselected,
  by.x = "studyid",
  by.y = "studyid",
  all.x = TRUE,
  suffixes = c("_isp", "_bsp")
)

# Assign all EMO participants as typically developing (TD)
all_withclin <- all_withclin %>%
  mutate(dx = case_when(
    study == "emo" ~ 0
  ))

# Combine diagnostic variables into a single harmonized variable
all_withclin <- all_withclin %>%
  mutate(dx = coalesce(dx, dx_isp, dx_bsp))

# Remove intermediate diagnostic variables
all_withclin <- subset(
  all_withclin,
  select = -c(dx_isp, dx_bsp, ASD_TD_DD)
)

# Export final analytic dataset
file2 <- file.path("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output", paste0("all_withclin_", date_str, "_", yourinitials, ".csv", sep=""))
write.csv(all_withclin, file = file2, row.names = FALSE)


############### PART TWO: CLEANING ################
# ====================
# Load packages
# ====================

# Load required packages 
library(dplyr)
library(lavaan)
library(semTools)
library(psych)

# ====================
# Set working directory and run metadata
# ====================

# Set project directory
setwd("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI")

# Store run date and analyst initials for file naming
date_str <- format(Sys.Date(), "%m-%d-%Y")
yourinitials <- "YK"

# ====================
# Load merged dataset
# ====================

# Import merged dataset prior to reverse scoring
all_data_og <- read.csv("/Volumes/PSY/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output/all_withclin_06-19-2026_YK.csv")
all_data <- all_data_og

# Remove variables not required for analyses
all_data <- all_data %>%
  dplyr::select(-age_months, -age_days, -dx)

# ====================
# Reverse score IBQ items
# ====================

# Reverse score items according to IBQ scoring guidelines
all_data$ibq33 <- 8 - all_data$ibq33
all_data$ibq60 <- 8 - all_data$ibq60
all_data$ibq3  <- 8 - all_data$ibq3
all_data$ibq82 <- 8 - all_data$ibq82
all_data$ibq84 <- 8 - all_data$ibq84
all_data$ibq86 <- 8 - all_data$ibq86
all_data$ibq91 <- 8 - all_data$ibq91
all_data$ibq37 <- 8 - all_data$ibq37
all_data$ibq72 <- 8 - all_data$ibq72
all_data$ibq61 <- 8 - all_data$ibq61
all_data$ibq50 <- 8 - all_data$ibq50
all_data$ibq73 <- 8 - all_data$ibq73

# Export reverse-scored dataset
file3 <- file.path(
  "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output",
  paste0("all_data_", date_str, "_", yourinitials, "_reversed.csv", sep = "")
)
write.csv(all_data, file = file3, row.names = FALSE)

# ====================
# Missing data screening
# ====================

# Calculate the number of missing values for each participant
row_na_count <- apply(all_data, 1, function(row) sum(is.na(row)))

# Calculate the percentage of missing values for each participant
row_na_percentage <- row_na_count / ncol(all_data) * 100

# Calculate missingness for each variable
col_na_count <- colSums(is.na(all_data))
col_na_percentage <- (col_na_count / nrow(all_data)) * 100

# Create participant-level missing data summary
row_results <- data.frame(
  Study_ID = all_data$studyid,
  Study = all_data$study,
  Age = all_data$age_group,
  NA_Count = row_na_count,
  NA_Percentage = row_na_percentage
)

# Merge missing data metrics into analytic dataset
NA_dataset <- merge(
  all_data,
  row_results,
  by.x = c("studyid", "age_group"),
  by.y = c("Study_ID", "Age"),
  all.x = TRUE
)

# Export missing data report
file4 <- file.path(
  "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output",
  paste0("missing_data_", date_str, "_", yourinitials, ".csv", sep = "")
)
write.csv(NA_dataset, file = file4, row.names = FALSE)

# Verify number of participants exceeding missing data threshold
count(NA_dataset %>% filter(NA_Percentage >= 15))
# n = 117 participants with >15% missing data

# ====================
# Participant exclusion based on missingness
# ====================

# Exclude participants with more than 15% missing data
clean_data <- NA_dataset %>%
  filter(NA_Percentage <= 15)

# Verify sample size by study following exclusion
count(clean_data %>% filter(study == "emo"))
# n = 626

count(clean_data %>% filter(study == "bsp"))
# n = 236

count(clean_data %>% filter(study == "isp"))
# n = 157

# Export cleaned dataset
file5 <- file.path(
  "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output",
  paste0("cleaned_data_all_", date_str, "_", yourinitials, ".csv", sep = "")
)
write.csv(clean_data, file = file5, row.names = FALSE)

# ====================
# Language administration information
# ====================

# Import BSP language administration data
bsp_lang_admin <- read.csv(
  "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/input/bsp_language.csv"
)

# Ensure participant IDs are stored as character strings
bsp_lang_admin <- bsp_lang_admin %>%
  mutate(studyid = as.character(studyid))

# Merge language administration information
clean_data <- clean_data %>%
  left_join(
    bsp_lang_admin %>% dplyr::select(studyid, lang),
    by = "studyid"
  )

# Verify language distribution
count(clean_data %>% filter(lang == "English"))
# Spanish = 37; English = 199

# ====================
# Add diagnostic information
# ====================

# Merge diagnostic classification into analytic dataset
clean_data <- clean_data %>%
  left_join(
    all_withclin %>% dplyr::select(studyid, age_group, dx),
    by = c("studyid", "age_group")
  )

# Verify diagnostic group counts
clean_data %>%
  count(dx)

# ====================
# Create analysis sample
# ====================

# Exclude participants with ASD diagnoses while retaining missing diagnostic classifications
clean_data_factor <- clean_data %>%
  filter(dx != 1 | is.na(dx))
# n = 979 (DD participants retained)

# Verify sample size by study
count(clean_data_factor %>% filter(study == "emo"))
# n = 626

count(clean_data_factor %>% filter(study == "bsp"))
# n = 221

count(clean_data_factor %>% filter(study == "isp"))
# n = 132

# Export final factor analysis dataset
file6 <- file.path(
  "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output",
  paste0("data_factor_analysis_", date_str, "_", yourinitials, ".csv", sep = "")
)
write.csv(clean_data_factor, file = file6, row.names = FALSE)

# ====================
# Duplicate participant check: ISP
# ====================

# Identify participants appearing in multiple age groups
duplicate <- clean_data_factor %>%
  group_by(studyid) %>%
  filter(n() > 1) %>%
  summarise(
    n = n(),
    age_groups = paste(unique(age_group), collapse = ", ")
  )

# Display all duplicate participant records
print(duplicate, n = Inf)

############### PART 3:FACTOR ANALYSIS #################
# ====================
# Load packages
# ====================

# Load required packages for confirmatory factor analysis, exploratory factor
# analysis, measurement invariance testing, and model visualization
library(dplyr)
library(lavaan)
library(semTools)
library(semPlot)
library(psych)
library(lavaan.mi)
library(multilevel)

# ====================
# Set working directory and run metadata
# ====================

# Set project directory
setwd("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI")

# Store run date and analyst initials for file naming
date_str <- format(Sys.Date(), "%m-%d-%Y")
yourinitials <- "YK"

# ====================
# Load factor analysis dataset
# ====================

# Import cleaned dataset prepared for factor analyses
clean_data_factor <- read.csv("/Volumes/PSY/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output/data_factor_analysis_06-19-2026_YK.csv")

# Remove missing data summary variables not needed for analyses
clean_data_factor <- clean_data_factor %>%
  dplyr::select(-Study, -NA_Count, -NA_Percentage)

# ====================
# Calculate IBQ subscale scores
# ====================

# Compute subscale sum scores from individual IBQ items
clean_data_factor$activ <- rowSums(clean_data_factor[c('ibq33', 'ibq34', 'ibq35', 'ibq39', 'ibq54', 'ibq55', 'ibq70')], na.rm = TRUE)
clean_data_factor$distress <- rowSums(clean_data_factor[c('ibq2', 'ibq3', 'ibq4', 'ibq21', 'ibq52', 'ibq53', 'ibq62')], na.rm = TRUE)
clean_data_factor$fear <- rowSums(clean_data_factor[c('ibq22', 'ibq76', 'ibq77', 'ibq78', 'ibq87', 'ibq89')], na.rm = TRUE)
clean_data_factor$orient <- rowSums(clean_data_factor[c('ibq5', 'ibq6', 'ibq7', 'ibq8', 'ibq10', 'ibq25')], na.rm = TRUE)
clean_data_factor$smile <- rowSums(clean_data_factor[c('ibq9', 'ibq11', 'ibq12', 'ibq40', 'ibq42', 'ibq43', 'ibq65')], na.rm = TRUE)
clean_data_factor$hipleas <- rowSums(clean_data_factor[c('ibq16', 'ibq44', 'ibq45', 'ibq46', 'ibq47', 'ibq48', 'ibq49')], na.rm = TRUE)
clean_data_factor$lowpleas <- rowSums(clean_data_factor[c('ibq13', 'ibq14', 'ibq15', 'ibq17', 'ibq18', 'ibq19', 'ibq67')], na.rm = TRUE)
clean_data_factor$sooth <- rowSums(clean_data_factor[c('ibq81', 'ibq82', 'ibq83', 'ibq84', 'ibq85', 'ibq86', 'ibq91')], na.rm = TRUE)
clean_data_factor$react <- rowSums(clean_data_factor[c('ibq36', 'ibq37', 'ibq38', 'ibq63', 'ibq71', 'ibq72')], na.rm = TRUE)
clean_data_factor$cuddl <- rowSums(clean_data_factor[c('ibq61', 'ibq50', 'ibq51', 'ibq59', 'ibq60', 'ibq73')], na.rm = TRUE)
clean_data_factor$percept <- rowSums(clean_data_factor[c('ibq20', 'ibq27', 'ibq28', 'ibq29', 'ibq30', 'ibq31')], na.rm = TRUE)
clean_data_factor$sad <- rowSums(clean_data_factor[c('ibq64', 'ibq74', 'ibq75', 'ibq32', 'ibq79', 'ibq80')], na.rm = TRUE)
clean_data_factor$approach <- rowSums(clean_data_factor[c('ibq23', 'ibq24', 'ibq68', 'ibq69', 'ibq88', 'ibq90')], na.rm = TRUE)
clean_data_factor$voc <- rowSums(clean_data_factor[c('ibq1','ibq26','ibq41','ibq56', 'ibq57', 'ibq58','ibq66')], na.rm = TRUE)

# Convert subscale sums to mean scores
clean_data_factor$activ <- clean_data_factor$activ / 7
clean_data_factor$distress <- clean_data_factor$distress / 7
clean_data_factor$fear <- clean_data_factor$fear / 6
clean_data_factor$orient <- clean_data_factor$orient / 6
clean_data_factor$smile <- clean_data_factor$smile / 7
clean_data_factor$hipleas <- clean_data_factor$hipleas / 7
clean_data_factor$lowpleas <- clean_data_factor$lowpleas / 7
clean_data_factor$sooth <- clean_data_factor$sooth / 7
clean_data_factor$react <- clean_data_factor$react / 6
clean_data_factor$cuddl <- clean_data_factor$cuddl / 6
clean_data_factor$percept <- clean_data_factor$percept / 6
clean_data_factor$sad <- clean_data_factor$sad / 6
clean_data_factor$approach <- clean_data_factor$approach / 6
clean_data_factor$voc <- clean_data_factor$voc / 7

# ====================
# Calculate higher-order factor scores
# ====================

# Calculate Surgency factor score
clean_data_factor$sur <- rowMeans(
  clean_data_factor[, c('approach', 'voc', 'hipleas', 'smile', 'activ', 'percept')],
  na.rm = TRUE
)

# Calculate Negative Affectivity factor score
clean_data_factor$neg <- rowMeans(
  cbind(
    clean_data_factor[, c('sad', 'distress', 'fear')],
    8 - clean_data_factor$react
  ),
  na.rm = TRUE
)

# Calculate Orienting/Regulatory factor score
clean_data_factor$reg <- rowMeans(
  clean_data_factor[, c('lowpleas', 'cuddl', 'orient', 'sooth')],
  na.rm = TRUE
)

# Export dataset with calculated subscale and factor scores
file7 <- file.path(
  "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output",
  paste0("data_analysis_all_calculated_", date_str, "_", yourinitials, ".csv", sep = "")
)
write.csv(clean_data, file = file7, row.names = FALSE)

# ====================
# Assess univariate normality
# ====================

# Conduct Shapiro-Wilk tests for all subscale scores
shapiro.test(clean_data_factor$activ)
shapiro.test(clean_data_factor$distress)
shapiro.test(clean_data_factor$fear)
shapiro.test(clean_data_factor$orient)
shapiro.test(clean_data_factor$smile)
shapiro.test(clean_data_factor$hipleas)
shapiro.test(clean_data_factor$lowpleas)
shapiro.test(clean_data_factor$sooth)
shapiro.test(clean_data_factor$react)
shapiro.test(clean_data_factor$cuddl)
shapiro.test(clean_data_factor$percept)
shapiro.test(clean_data_factor$sad)
shapiro.test(clean_data_factor$approach)
shapiro.test(clean_data_factor$voc)


# ====================
# Item-total correlations
# ====================

# Create item-level datasets for each IBQ subscale
activ_sub <- clean_data_factor[c('ibq33', 'ibq34', 'ibq35', 'ibq39', 'ibq54', 'ibq55', 'ibq70')]
distress_sub <- clean_data_factor[c('ibq2', 'ibq3', 'ibq4', 'ibq21', 'ibq52', 'ibq53', 'ibq62')]
fear_sub <- clean_data_factor[c('ibq22', 'ibq76', 'ibq77', 'ibq78', 'ibq87', 'ibq89')]
orient_sub <- clean_data_factor[c('ibq5', 'ibq6', 'ibq7', 'ibq8', 'ibq10', 'ibq25')]
smile_sub <- clean_data_factor[c('ibq9', 'ibq11', 'ibq12', 'ibq40', 'ibq42', 'ibq43', 'ibq65')]
hipleas_sub <- clean_data_factor[c('ibq16', 'ibq44', 'ibq45', 'ibq46', 'ibq47', 'ibq48', 'ibq49')]
lowpleas_sub <- clean_data_factor[c('ibq13', 'ibq14', 'ibq15', 'ibq17', 'ibq18', 'ibq19', 'ibq67')]
sooth_sub <- clean_data_factor[c('ibq81', 'ibq82', 'ibq83', 'ibq84', 'ibq85', 'ibq86', 'ibq91')]
react_sub <- clean_data_factor[c('ibq36', 'ibq37', 'ibq38', 'ibq63', 'ibq71', 'ibq72')]
cuddl_sub <- clean_data_factor[c('ibq61', 'ibq50', 'ibq51', 'ibq59', 'ibq60', 'ibq73')]
percept_sub <- clean_data_factor[c('ibq20', 'ibq27', 'ibq28', 'ibq29', 'ibq30', 'ibq31')]
sad_sub <- clean_data_factor[c('ibq64', 'ibq74', 'ibq75', 'ibq32', 'ibq79', 'ibq80')]
approach_sub <- clean_data_factor[c('ibq23', 'ibq24', 'ibq68', 'ibq69', 'ibq88', 'ibq90')]
voc_sub <- clean_data_factor[c('ibq1','ibq26','ibq41','ibq56', 'ibq57', 'ibq58','ibq66')]

# Calculate item-total correlations for each subscale
item.total(activ_sub)
item.total(distress_sub)
item.total(fear_sub)
item.total(orient_sub)
item.total(smile_sub)
item.total(hipleas_sub)
item.total(lowpleas_sub)
item.total(sooth_sub)
item.total(react_sub)
item.total(cuddl_sub)
item.total(percept_sub)
item.total(sad_sub)
item.total(approach_sub)
item.total(voc_sub)

# ====================
# Internal consistency
# ====================

# Create list of subscales for reliability analyses
subscales <- list(
  activ = activ_sub,
  distress = distress_sub,
  fear = fear_sub,
  orient = orient_sub,
  smile = smile_sub,
  hipleas = hipleas_sub,
  lowpleas = lowpleas_sub,
  sooth = sooth_sub,
  react = react_sub,
  cuddl = cuddl_sub,
  percept = percept_sub,
  sad = sad_sub,
  approach = approach_sub,
  voc = voc_sub
)

# Calculate Cronbach's alpha for each subscale
sapply(subscales, function(x) psych::alpha(x)$total$raw_alpha)


# ============================================================================
# Confirmatory Factor Analysis of Published IBQ-R Models
# ============================================================================

# Evaluate previously published factor structures using CFA at the subscale level

# ----------------------------------------------------------------------------
# Gartstein & Rothbart (2003) Three-Factor Model
# ----------------------------------------------------------------------------

ibqmod3fac_mean <- '
  surg =~ approach + voc + hipleas + smile + percept + activ
  negemo =~ sad + distress + fear + react
  regu =~ lowpleas + cuddl + orient + sooth'

gart2003fit <- cfa(ibqmod3fac_mean, data=clean_data_factor, estimator="MLR")
summary(gart2003fit, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
fitMeasures( gart2003fit, c("chisq", "df", "pvalue","cfi", "tli", "rmsea", "srmr"))
semPaths(gart2003fit, "std", sizeLat = 7, edge.label.cex = 0.75)

# ----------------------------------------------------------------------------
# Gartstein & Rothbart (2003) Model Excluding Cuddliness
# ----------------------------------------------------------------------------

ibqmod3fac_nocud <- '
  surg =~ approach + voc + hipleas + smile + percept + activ
  negemo =~ sad + distress + fear + react
  regu =~ lowpleas + orient + sooth'

fit_nocud <- cfa(ibqmod3fac_nocud, data=clean_data_factor, estimator="MLR")
summary(fit_nocud, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
fitMeasures(fit_nocud, c("chisq", "df", "pvalue","cfi", "tli", "rmsea", "srmr"))
semPaths(fit_nocud, "std", sizeLat = 7, edge.label.cex = 0.75)


# ----------------------------------------------------------------------------
# Bosquet-Enlow et al. Model
# ----------------------------------------------------------------------------

bosquetfit <- '
  surg =~ hipleas + smile + percept + voc + activ
  negemo =~ distress + sad + react + fear + voc + activ
  regu =~ lowpleas + orient + fear'

bosquet_all <- cfa(bosquetfit, data=clean_data_factor, estimator="MLR")
summary(bosquet_all, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
fitMeasures(bosquet_all, c("chisq", "df", "pvalue","cfi", "tli", "rmsea", "srmr"))
semPaths(bosquet_all, "std", sizeLat = 7, edge.label.cex = 0.75)


# ----------------------------------------------------------------------------
# Sung et al. (2022) Model 1
# ----------------------------------------------------------------------------

sungmod1 <-'
surg =~ smile + percept + voc + activ + approach + fear
negemo =~ activ + smile + percept + approach + fear + distress + react + sad 
regu =~ lowpleas + orient + sooth'

sungmodel <- cfa(sungmod1, data=clean_data_factor)

summary(sungmodel, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
fitMeasures(sungmodel, c("chisq", "df", "pvalue","cfi", "tli", "rmsea", "srmr"))
semPaths(sungmodel, "std", sizeLat = 7, edge.label.cex = 0.75)


# ----------------------------------------------------------------------------
# Sung et al. (2022) Model 2
# ----------------------------------------------------------------------------

sungmod2 <-'
surg =~ smile + hipleas + approach + voc + fear + react
negemo =~ fear + distress + react + sad
regu =~ orient'

sungmodel2 <- cfa(sungmod2, data=clean_data_factor)

summary(sungmodel2, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
fitMeasures(sungmodel2, c("chisq", "df", "pvalue","cfi", "tli", "rmsea", "srmr"))
semPaths(sungmodel2, "std", sizeLat = 7, edge.label.cex = 0.75)


# ----------------------------------------------------------------------------
# Dragan et al. (2011) Model
# ----------------------------------------------------------------------------

draganfit <- '
fac1 =~ hipleas + approach + sooth + smile + cuddl + voc + react + lowpleas
fac2 =~ hipleas + distress + sad + react + activ + fear
fac3 =~ smile + cuddl + orient + percept + lowpleas + fear'

draganmodel <- cfa(draganfit, data=clean_data_factor)

summary(draganmodel, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
fitMeasures(draganmodel, c("chisq", "df", "pvalue","cfi", "tli", "rmsea", "srmr"))
semPaths(draganmodel, "std", sizeLat = 7, edge.label.cex = 0.75)

# Model did not converge.

# ============================================================================
# Exploratory Factor Analysis (EFA)
# ============================================================================

# Create datasets for exploratory analyses

efa_data <- clean_data_factor

efa_data_factor <- efa_data %>%
  dplyr::select(-studyid, -study, -dx)

efa_data_subscale <- efa_data %>%
  dplyr::select(
    activ, distress, fear, orient, smile,
    hipleas, lowpleas, sooth, react, cuddl,
    percept, sad, approach, voc
  )

# Create English-only sensitivity analysis dataset

efa_data_eng <- efa_data %>%
  dplyr::filter(lang != "Spanish" | is.na(lang))

efa_data_subscale_eng <- efa_data_eng %>%
  dplyr::select(
    activ, distress, fear, orient, smile,
    hipleas, lowpleas, sooth, react, cuddl,
    percept, sad, approach, voc
  )

# Verify language composition
sum(efa_data_subscale_eng$lang == "Spanish", na.rm = TRUE)

# ============================================================================
# Discovery and Replication Samples
# ============================================================================

efa_data_subscale_explore <- efa_data_subscale

library(caret)

set.seed(42)

n <- nrow(efa_data_subscale_explore)
efa_train <- sample(seq_len(n), size = 0.7 * n)

# Discovery sample (70%)
efa_discovery <- efa_data_subscale_explore[efa_train, ]

# Replication sample (30%)
efa_replication <- efa_data_subscale_explore[-efa_train, ]

# ============================================================================
# Exploratory Factor Analysis
# ============================================================================

# Estimate three-factor solution using maximum likelihood extraction
# and oblimin rotation

efa_cor_matrix_dis <- cor(
  efa_discovery,
  use = "pairwise.complete.obs"
)

efa_results_dis <- fa(
  r = efa_cor_matrix_dis,
  nfactors = 3,
  rotate = "oblimin",
  fm = "ml"
)

print(efa_results_dis, cut = 0.25)

# Calculate model fit indices from raw data

efa_results_dis <- fa(
  efa_discovery,
  nfactors = 3,
  rotate = "oblimin",
  fm = "ml"
)

efa_results_dis$TLI
efa_results_dis$CFI
efa_results_dis$RMSEA

# Visualize EFA solution

fa.diagram(efa_results_dis)

# ============================================================================
# Confirmatory Factor Analysis of EFA-Derived Models
# ============================================================================

# Initial three-factor model derived from EFA results

cfafromefa3 <- '
  fac1 =~ activ + fear + smile + hipleas + percept + approach + voc
  fac2 =~ activ + distress + fear + sooth + react + cuddl + sad
  fac3 =~ orient + lowpleas'

test_cfa <- cfa(cfafromefa3, data = efa_replication)

summary(test_cfa, fit.measures = TRUE, standardized = TRUE)
fitMeasures(test_cfa, c("chisq", "df", "pvalue","cfi", "tli", "rmsea", "srmr"))

# modificationIndices(test_cfa)
# 
# mi <- modificationIndices(test_cfa)
# mi_sorted <- mi[order(-mi$mi), ]

# ----------------------------------------------------------------------------
# Remove Cuddliness
# ----------------------------------------------------------------------------

cfafromefa3_nocud <- '
  fac1 =~ activ + fear + smile + hipleas + percept + approach + voc
  fac2 =~ activ + distress + fear + sooth + react + sad
  fac3 =~ orient + lowpleas'

test_cfa <- cfa(cfafromefa3_nocud, data = efa_replication)

summary(test_cfa, fit.measures = TRUE, standardized = TRUE)
fitMeasures(test_cfa, c("chisq", "df", "pvalue","cfi", "tli", "rmsea", "srmr"))


# ----------------------------------------------------------------------------
# Separate Negative Loading Indicators Into Fourth Factor
# ----------------------------------------------------------------------------

cfafromefa3_nocud_4 <- '
  fac1 =~ activ + fear + smile + hipleas + percept + approach + voc
  fac2 =~ activ + distress + fear + sad
  fac3 =~ orient + lowpleas
  fac4 =~ sooth + react '

test_cfa <- cfa(cfafromefa3_nocud_4, data = efa_replication)

summary(test_cfa, fit.measures = TRUE, standardized = TRUE)
fitMeasures(test_cfa, c("chisq", "df", "pvalue","cfi", "tli", "rmsea", "srmr"))

modificationIndices(test_cfa)

mi <- modificationIndices(test_cfa)
mi_sorted <- mi[order(-mi$mi), ]


# ----------------------------------------------------------------------------
# Final EFA-Derived Factor Structure
# ----------------------------------------------------------------------------

cfafromefa4_neg <- '
  fac1 =~ activ + smile + hipleas + percept + approach + voc
  fac2 =~ activ + distress + sad + fear
  fac3 =~ orient + lowpleas
  fac4 =~ sooth + react '

test_cfa <- cfa(cfafromefa4_neg, data = efa_replication)
summary(test_cfa, fit.measures = TRUE, standardized = TRUE)
fitMeasures(test_cfa, c("chisq", "df", "pvalue","cfi", "tli", "rmsea", "srmr"))

#####test statistic for FULL SAMPLE (n = 979):
test_cfa <- cfa(cfafromefa4_neg, data = efa_data_subscale)
summary(test_cfa, fit.measures = TRUE, standardized = TRUE)
fitMeasures(test_cfa, c("chisq", "df", "pvalue","cfi", "tli", "rmsea", "srmr"))


#####test statistic for FULL SAMPLE ENG ONLY:
test_cfa_eng <- cfa(cfafromefa4_neg, data = efa_data_subscale_eng)
summary(test_cfa_eng, fit.measures = TRUE, standardized = TRUE)
fitMeasures(test_cfa_eng, c("chisq", "df", "pvalue","cfi", "tli", "rmsea", "srmr"))



# ----------------------------------------------------------------------------
# Create graph for structure
# ----------------------------------------------------------------------------

parameterEstimates(test_cfa, standardized = TRUE)
parameterEstimates(test_cfa_eng, standardized = TRUE)

#plot CFA
semPaths(
  test_cfa,
  whatLabels = "std",    
  layout = "tree",      
  edge.color = "black",
  sizeMan = 6,          
  sizeLat = 6,          
  nCharNodes = 0,        
  residuals = TRUE,     
  exoCov = TRUE,        
  intercepts = FALSE
)


############ AGE INVARIANCE ##########

#----------------------------
# Setup age invariance data
#----------------------------
age_invar_data <- efa_data
age_invar_data$age_group <- as.factor(age_invar_data$age_group)
levels(age_invar_data$age_group)

#English Administration ONLY
age_invar_data_eng <- efa_data_eng
age_invar_data_eng$age_group <- as.factor(age_invar_data_eng$age_group)
levels(age_invar_data_eng$age_group)

#----------------------------
# Initial measurement invariance check
#----------------------------
MI_age <- cfa(model = cfafromefa4_neg, data = age_invar_data, group = "age_group")
summary(MI_age, fit.measures = TRUE, standardized = TRUE)

measurementInvariance(model = cfafromefa4_neg, data = age_invar_data, group = "age_group")

#----------------------------
# Configural model (full sample)
#----------------------------
confit <- cfa(cfafromefa4_neg, data = age_invar_data, std.lv = TRUE, group = "age_group")

#----------------------------
# Weak invariance (loadings constrained)
#----------------------------
weakfit <- measEq.syntax(
  configural.model = confit, 
  return.fit = TRUE,
  group = "age_group",
  group.equal = c("loadings")
)

modindices(weakfit, sort = TRUE, maximum.number = 20)

#----------------------------
# Strong invariance (loadings + intercepts)
#----------------------------
strongfit <- measEq.syntax(
  configural.model = confit,
  return.fit = TRUE,
  group = "age_group",
  group.equal = c("loadings", "intercepts")
)

modindices(strongfit, sort = TRUE, maximum.number = 20)

#----------------------------
# Test score diagnostics
#----------------------------
pi <- lavTestScore(strongfit, epc = TRUE, univariate = TRUE)
pi_df <- bind_rows(pi, .id = "group_id")

#View(pi_df)

# file8 <- file.path(
#   "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output",
#   paste0("patial_invariance_no3_", date_str, "_", yourinitials, ".csv", sep = "")
# )
# 
# write.csv(pi_df, file = file8, row.names = FALSE)

#----------------------------
# Strict invariance
#----------------------------
strictfit <- measEq.syntax(
  configural.model = confit,
  return.fit = TRUE,
  group = "age",
  group.equal = c("loadings", "intercepts", "residuals")
)

fit_comp <- compareFit(confit, weakfit, strongfit, strictfit, nested = TRUE)
summary(fit_comp)
#----------------------------
# Remove 3-month-olds 
#----------------------------
age_invar_data2 <- subset(age_invar_data, age_group != "3")
age_invar_data2$age_group <- droplevels(age_invar_data2$age_group)

age_invar_data2 %>%
  count(age_group)

#English only
age_invar_data_eng2 <- subset(age_invar_data_eng, age_group != "3")
age_invar_data_eng2$age_group <- droplevels(age_invar_data_eng2$age_group)


#----------------------------
# Configural model (removing 3-month-olds)
#----------------------------
confit2 <- cfa(cfafromefa4_neg, data = age_invar_data2, std.lv = TRUE, group = "age_group")

#----------------------------
# Weak invariance (loadings constrained)
#----------------------------
weakfit <- measEq.syntax(
  configural.model = confit2, 
  return.fit = TRUE,
  group = "age_group",
  group.equal = c("loadings")
)

modindices(weakfit, sort = TRUE, maximum.number = 20)

#----------------------------
# Strong invariance (loadings + intercepts)
#----------------------------
strongfit <- measEq.syntax(
  configural.model = confit2,
  return.fit = TRUE,
  group = "age_group",
  group.equal = c("loadings", "intercepts")
)

modindices(strongfit, sort = TRUE, maximum.number = 20)

#----------------------------
# Strict invariance 
#----------------------------
strictfit <- measEq.syntax(
  configural.model = confit2,
  return.fit = TRUE,
  group = "age_group",
  group.equal = c("loadings", "intercepts", "residuals")
)

fit_comp <- compareFit(confit2, weakfit, strongfit, strictfit, nested = TRUE)
summary(fit_comp)

modindices(strongfit, sort = TRUE, maximum.number = 20)

#-----------------------------------
# Partial scalar invariance
# Free intercepts identified from MIs
#-----------------------------------

##### MODIFY THE INTERCEPTS FREED ########

partialscalarfit <- measEq.syntax(
  configural.model = confit2,
  return.fit = TRUE,
  group = "age_group",
  group.equal = c("loadings", "intercepts"),
  group.partial = c(
    "approach ~1"
  )
)

summary(partialscalarfit, fit.measures = TRUE)

#-----------------------------------
# Compare metric vs. partial scalar
#-----------------------------------
fit_comp2 <- compareFit(
  weakfit,
  partialscalarfit,
  nested = TRUE
)

summary(fit_comp2)


############################################################
# AGE INVARIANCE: REMOVING 5-MONTH-OLDS
############################################################

age_invar_data3 <- subset(age_invar_data2, age_group != "5")
age_invar_data3$age <- droplevels(age_invar_data3$age_group)

#----------------------------
# Configural model (5-month-olds removed)
#----------------------------
confit3 <- cfa(cfafromefa4_neg, data = age_invar_data3, std.lv = TRUE, group = "age")

#----------------------------
# Weak invariance
#----------------------------
weakfit <- measEq.syntax(
  configural.model = confit3, 
  return.fit = TRUE,
  group = "age_group",
  group.equal = c("loadings")
)

modindices(weakfit, sort = TRUE, maximum.number = 20)

#----------------------------
# Strong invariance
#----------------------------
strongfit <- measEq.syntax(
  configural.model = confit3,
  return.fit = TRUE,
  group = "age_group",
  group.equal = c("loadings", "intercepts")
)

modindices(strongfit, sort = TRUE, maximum.number = 20)

#----------------------------
# Strict invariance
#----------------------------
strictfit <- measEq.syntax(
  configural.model = confit3,
  return.fit = TRUE,
  group = "age",
  group.equal = c("loadings", "intercepts", "residuals")
)

fit_comp <- compareFit(confit3, weakfit, strongfit, strictfit, nested = TRUE)
summary(fit_comp)

############################################################
# SENSITIVITY ANALYSIS: EMO SAMPLE
############################################################

age_invar_data_emo <- age_invar_data %>%
  filter(study == "emo")

table(age_invar_data_emo$age_group)

age_invar_data_emo$age_group <- droplevels(age_invar_data_emo$age_group)
age_invar_data_emo$age_group <- as.factor(age_invar_data_emo$age_group)

###### MODIFY: Remove each age group as needed to run sensitivity analysis 
age_invar_data_emo <- subset(age_invar_data_emo, age_group != "10")
age_invar_data_emo <- subset(age_invar_data_emo, age_group != "5")
age_invar_data_emo <- subset(age_invar_data_emo, age_group != "7")
age_invar_data_emo <- subset(age_invar_data_emo, age_group != "12")
age_invar_data_emo$age_group <- droplevels(age_invar_data_emo$age_group)

#----------------------------
# Configural model (emo)
#----------------------------
confit4 <- cfa(cfafromefa4_neg, data = age_invar_data_emo, std.lv = TRUE, group = "age_group")

#----------------------------
# Weak invariance
#----------------------------
weakfit <- measEq.syntax(
  configural.model = confit4, 
  return.fit = TRUE,
  group = "age_group",
  group.equal = c("loadings")
)

modindices(weakfit, sort = TRUE, maximum.number = 20)

#----------------------------
# Strong invariance
#----------------------------
strongfit <- measEq.syntax(
  configural.model = confit4,
  return.fit = TRUE,
  group = "age_group",
  group.equal = c("loadings", "intercepts")
)

modindices(strongfit, sort = TRUE, maximum.number = 20)

#----------------------------
# Strict invariance (emo)
#----------------------------
strictfit <- measEq.syntax(
  configural.model = confit4,
  return.fit = TRUE,
  group = "age_group",
  group.equal = c("loadings", "intercepts", "residuals")
)

fit_comp <- compareFit(confit4, weakfit, strongfit, strictfit, nested = TRUE)
summary(fit_comp)

########################################
# DEMOGRAPHICS 
########################################

#Upload demographic data
isp_demo <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/demographics/isp2_demographics_Finalized.csv")
emo_demo <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/demographics/Emo-Demographics_SES_inf_2y_trimmed.csv.csv")
bsp_demo <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/demographics/2_BSP_scrp_demographics_simple.csv")

#Filter clean data by study
clean_data_factor_isp <- clean_data_factor %>%
  filter(study == "isp")
clean_data_factor_emo <- clean_data_factor %>%
  filter(study == "emo")
clean_data_factor_bsp <- clean_data_factor %>%
  filter(study == "bsp")

#ISP 
clean_data_factor_isp <- clean_data_factor_isp %>%
  mutate(studyid = as.character(studyid))

#match variables
isp_demo <- isp_demo %>%
  mutate(study_id = as.character(study_id))

#merge data with demographics
isp_demo_final <- clean_data_factor_isp %>%
  left_join(isp_demo, by = c("studyid" = "study_id"))

write.csv(isp_demo_final, file = "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/demographics/isp_demo_pub_06192026.csv", row.names = F)

#Emotion
clean_data_factor_emo <- clean_data_factor_emo %>%
  mutate(studyid = as.character(studyid))

#match variables
emo_demo <- emo_demo %>%
  mutate(study_id = as.character(subj))

#merge data with demographics
emo_demo_final <- clean_data_factor_emo %>%
  left_join(emo_demo, by = c("studyid" = "subj"))

write.csv(emo_demo_final, file = "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/demographics/emo_demo_pub_06192026.csv", row.names = F)


#BabySteps
clean_data_factor_bsp <- clean_data_factor_bsp %>%
  mutate(studyid = as.character(studyid))

#match variables
bsp_demo <- bsp_demo %>%
  mutate(studyid = as.character(studyid))

#merge data with demographics
bsp_demo_final <- clean_data_factor_bsp %>%
  left_join(bsp_demo, by = c("studyid"))

write.csv(bsp_demo_final, file = "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/demographics/bsp_demo_pub_06192026.csv", row.names = F)



