# load libraries
library(data.table)
library(psych)
library(lavaan)
library(tidyr)
library(dplyr)
library(psych)
library(GPArotation)
library(stringr)
library(haven)


#set path 
setwd("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI")  

#run system date [MODIFY YOUR INITIALS]
date_str <- format(Sys.Date(), "%m-%d-%Y")
yourinitials <- "CL" #example: YK

#CLEAN EMO DATA
emo_orig <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/raw/emo_ibq_RAW.csv") 
emo <- emo_orig

emo_asd <- read_sav("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/raw/ASD_ibqr_nosub.sav")

#replace subj as studyid
emo_clean <- emo %>%
  rename(studyid = subj)

emo_asd_clean <- emo_asd %>%
  rename(studyid = subj)

#change ibqsf naming to ibq
colnames(emo_clean) <- gsub("^ibqsf", "ibq", colnames(emo_clean))
colnames(emo_asd_clean) <- gsub("^ibqsf", "ibq", colnames(emo_asd_clean))

#extract age column from emo_clean studyid (extract last 1 or 2 digits)
emo_clean <- emo_clean %>%
  mutate(age = as.numeric(str_extract(studyid, "(\\d{1,2})(?=\\D*$)")))

emo_asd_clean <- emo_asd_clean %>%
  mutate(age = as.numeric(str_extract(studyid, "(\\d{1,2})(?=\\D*$)")))

#add study name column
emo_clean <- emo_clean %>%
  mutate(study = "emo")

emo_asd_clean <- emo_asd_clean %>%
  mutate(study = "emo")

#move the age and study columns after study id
emo_clean <- emo_clean %>%
  dplyr::select(studyid, age, study, everything())

emo_asd_clean <- emo_asd_clean %>%
  dplyr::select(studyid, age, study, everything())

#remove redcap column
emo_clean <- emo_clean %>%
  dplyr::select(-redcap_event_name)

emo_asd_clean <- emo_asd_clean %>%
  dplyr::select(-IBQ_DOC)

#replace 99 to NA
emo_clean[emo_clean == 99] <- NA
emo_clean[emo_clean == 9] <- NA

write.csv(emo_asd_clean, file = "/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output/emo_asd.csv", row.names = FALSE)

emo_asd_dx <-read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output/emo_asd.csv")
#add in column for dx
emo_asd_dx$dx <- 1

#BABYSTEPS cleaning
bs_orig <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/raw/bsp_ibq_RAW.csv") 
bs <- bs_orig

#take out redcap column
bs_clean <- bs %>%
  dplyr::select(-redcap_event_name, -ibq_age_months)

#remove all columns not named ibq1-91
bs_clean <- bs_clean %>% dplyr::select(studyid, ibq1:ibq91)

#add column for age
bs_clean <- bs_clean %>% mutate(age = 9)

#add column for study 
bs_clean <- bs_clean %>%
  mutate(study = "bsp")

#move age and study to front
bs_clean <- bs_clean %>%
  dplyr::select(studyid, age, study, everything())

#ISP2
isp_orig <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/raw/isp2_ibq_RAW.csv") 
isp <- isp_orig

#change study_id to studyid
isp_clean <- isp %>%
  rename(studyid = study_id)

#remove unecessary column 
isp_clean <- isp_clean %>%
  dplyr::select(-redcap_event_name, -ibq2_age_in_days, -ibq2_age_in_months, -ibq2_age_in_years, -ibq2_final_data_complete)

#change visit_mo to age
isp_clean <- isp_clean %>% rename(age = visit_mo)

#change ibqsf naming to ibq
colnames(isp_clean) <- gsub("^ibq\\d+_ibqsf(\\d+)_f$", "ibq\\1", colnames(isp_clean))

#add study name column
isp_clean <- isp_clean %>%
  mutate(study = "isp")

#change ISP 999 to NA
isp_clean[isp_clean == 999] <- NA

#move the age and study columns after study id
isp_clean <- isp_clean %>%
  dplyr::select(studyid, age, study, everything())

#save cleaned dataframes into new one just in case
emo_merge <- emo_clean
emo_asd_merge <- emo_asd_clean
bs_merge <- bs_clean
isp_merge <- isp_clean

#last clean check to make sure all are same type
emo_merge <- emo_merge %>%
  mutate(studyid = as.character(studyid))  # Convert to character

emo_asd_merge <- emo_asd_merge %>%
  mutate(studyid = as.character(studyid))  # Convert to character

bs_merge <- bs_merge %>%
  mutate(studyid = as.character(studyid))  # Convert to character

isp_merge <- isp_merge %>%
  mutate(studyid = as.character(studyid))  # Convert to character

#bind all 3 csv into one big csv, not merge, just bind
## NOT REVERSE SCORED YET
## STILL MISSING 14/3 scores
all_datasets <- bind_rows(emo_merge, emo_asd_merge, bs_merge, isp_merge)
#checked if all dataset was binded into one dataset 
# yes --> n = 1165 observations

# put csv into specific folder path
file1 <- file.path("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output", paste0("all_datasets_", date_str,"_", yourinitials, ".csv", sep=""))
write.csv(all_datasets, file = file1, row.names = FALSE)

# View the result
print(all_datasets)

###### LOAD IN CLINICAL JUDGEMENT DATA ########
#load in clinical judgement data
ispclin_orig <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/clin_judgement/isp_clinjudge.csv") 
ispclin <- ispclin_orig

bspclin_orig <- read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/clin_judgement/BSP_Clinical-judgement_2025-04-04.csv")
bspclin <- bspclin_orig

emoclin_orig <-read.csv("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/clin_judgement/emo_demographics.csv")
emoclin <- emoclin_orig

#merge clinjudgement into master spreadsheet
# Select only the columns needed from df2 (e.g., ID and value2)
ispclinselected <- ispclin[, c("study_id", "ASD_TD_DD")]

ispclinselected$dx <- ifelse(ispclin$ASD_TD_DD == "TD", 0, 
                ifelse(ispclin$ASD_TD_DD == "ASD", 1, 
                       ifelse(ispclin$ASD_TD_DD == "DD", 2, 
                              ifelse(ispclin$ASD_TD_DD == "UNKNOWN/NoGroup", 3, NA))))

bspclinselected <- bspclin[, c("studyid", "fcj_24m_judgement_asd", "fcj_24m_asd_certainty")]
bspclinselected$dx <- ifelse(bspclin$fcj_24m_judgement_asd == "No Autism Spectrum Disorder", 0, 
                             ifelse(bspclin$fcj_24m_judgement_asd == "Yes Autism Spectrum Disorder", 1, 
                                    ifelse(bspclin$fcj_24m_judgement_asd == "Low ASD likelihood based on screener", 0,
                                           ifelse(bspclin$fcj_24m_judgement_asd == "", 3, NA))))

# emoclinselected$dx <- emo_asd_dx$dx
# emoclinselected <- emoclinselected %>%
#   select(-exclusion_criteria)

#Merge ispclindelected data with all datasets
all_withclin <- merge(all_datasets, ispclinselected, by.x = "studyid", by.y = "study_id", all.x = TRUE)

# Merge all_withclin with bspclinselected
all_withclin <- merge(all_withclin, bspclinselected, by.x = "studyid", by.y = "studyid", all.x = TRUE, suffixes = c("_isp", "_bsp"))

all_withclin <- all_withclin %>%
  filter(!studyid %in% emo_asd_dx$studyid) %>%
  bind_rows(emo_asd_dx)


# #checking EMO COMMON IDS
# common_ids <- intersect(all_withclin$studyid, emoclinselected$studyid)
# length(common_ids) 
# 
# tail(describe(all_withclin))
# Combine the 'dx' columns into a single column
all_withclin <- all_withclin %>%
  mutate(dx = coalesce(dx, dx_isp, dx_bsp))

# Drop the separate columns no longer needed
all_withclin <- subset(all_withclin, select = -c(dx_isp, dx_bsp, ASD_TD_DD, 
                                                 fcj_24m_judgement_asd, 
                                                 fcj_24m_asd_certainty))

# all_withclin_4.7 <- bind_rows(all_withclin, emo_asd_dx)

file2 <- file.path("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output", paste0("all_withclin_", date_str, "_", yourinitials, ".csv", sep=""))
write.csv(all_withclin, file = file2, row.names = FALSE)


######### STOP HERE, EVERYTHING BELOW THIS = EXPLORATORY/NOT USED CODE ###########
# all_withclin_4.2 <- bind_rows(all_withclin, emo_asd_dx)
# all_withclin_4.7 <- bind_rows(all_withclin, emo_asd_dx)
# all_withclin_4.7 <- bind_rows(all_withclin, emo_asd_dx)

# write.csv(all_withclin_4.7, file = "all_withclin_4.7.csv", row.names = FALSE)

#checking NAs
na_counts_all <- colSums(is.na(all_withclin))
print(na_counts_all)


# ############ DO NOT USE THIS CODE FOR IBQ STRAIGHT FACTOR ###########
# #bsp NOT reverse scored, ISP NOT reverse scored, EMO not reverse scored
# # NEED TO REVERSE SCORE DATA [DID NOT RUN, WAITING FOR CONFIRMATION]
# all_withclin_4.7$ibq33 <- 8 - all_withclin_4.7$ibq33
# all_withclin_4.7$ibq60 <- 8 - all_withclin_4.7$ibq60
# all_withclin_4.7$ibq3 <- 8 - all_withclin_4.7$ibq3
# all_withclin_4.7$ibq82 <- 8 - all_withclin_4.7$ibq82
# all_withclin_4.7$ibq84 <- 8 - all_withclin_4.7$ibq84
# all_withclin_4.7$ibq86 <- 8 - all_withclin_4.7$ibq86
# all_withclin_4.7$ibq91 <- 8 - all_withclin_4.7$ibq91
# all_withclin_4.7$ibq37 <- 8 - all_withclin_4.7$ibq37
# all_withclin_4.7$ibq72 <- 8 - all_withclin_4.7$ibq72
# all_withclin_4.7$ibq61 <- 8 - all_withclin_4.7$ibq61
# all_withclin_4.7$ibq50 <- 8 - all_withclin_4.7$ibq50
# all_withclin_4.7$ibq73 <- 8 - all_withclin_4.7$ibq73
# 
# #calculate subscale scores + total score for nonimputed data
# all_withclin_4.7$activ <- rowSums(all_withclin_4.7[c('ibq33', 'ibq34', 'ibq35', 'ibq39', 'ibq54', 'ibq55', 'ibq70')], na.rm = TRUE)
# all_withclin_4.7$distress <- rowSums(all_withclin_4.7[c('ibq2', 'ibq3', 'ibq4', 'ibq21', 'ibq52', 'ibq53', 'ibq62')], na.rm = TRUE)
# all_withclin_4.7$fear <- rowSums(all_withclin_4.7[c('ibq22', 'ibq76', 'ibq77', 'ibq78', 'ibq87', 'ibq89')], na.rm = TRUE)
# all_withclin_4.7$orient <- rowSums(all_withclin_4.7[c('ibq5', 'ibq6', 'ibq7', 'ibq8', 'ibq10', 'ibq25')], na.rm = TRUE)
# all_withclin_4.7$smile <- rowSums(all_withclin_4.7[c('ibq9', 'ibq11', 'ibq12', 'ibq40', 'ibq42', 'ibq43', 'ibq65')], na.rm = TRUE)
# all_withclin_4.7$hipleas <- rowSums(all_withclin_4.7[c('ibq16', 'ibq44', 'ibq45', 'ibq46', 'ibq47', 'ibq48', 'ibq49')], na.rm = TRUE)
# all_withclin_4.7$lowpleas <- rowSums(all_withclin_4.7[c('ibq13', 'ibq14', 'ibq15', 'ibq17', 'ibq18', 'ibq19', 'ibq67')], na.rm = TRUE)
# all_withclin_4.7$sooth <- rowSums(all_withclin_4.7[c('ibq81', 'ibq82', 'ibq83', 'ibq84', 'ibq85', 'ibq86', 'ibq91')], na.rm = TRUE)
# all_withclin_4.7$react <- rowSums(all_withclin_4.7[c('ibq36', 'ibq37', 'ibq38', 'ibq63', 'ibq71', 'ibq72')], na.rm = TRUE)
# all_withclin_4.7$cuddl <- rowSums(all_withclin_4.7[c('ibq61', 'ibq50', 'ibq51', 'ibq59', 'ibq60', 'ibq73')], na.rm = TRUE)
# all_withclin_4.7$percept <- rowSums(all_withclin_4.7[c('ibq20', 'ibq27', 'ibq28', 'ibq29', 'ibq30', 'ibq31')], na.rm = TRUE)
# all_withclin_4.7$sad <- rowSums(all_withclin_4.7[c('ibq64', 'ibq74', 'ibq75', 'ibq32', 'ibq79', 'ibq80')], na.rm = TRUE)
# all_withclin_4.7$approach <- rowSums(all_withclin_4.7[c('ibq23', 'ibq24', 'ibq68', 'ibq69', 'ibq88', 'ibq90')], na.rm = TRUE)
# all_withclin_4.7$voc <- rowSums(all_withclin_4.7[c('ibq1','ibq26','ibq41','ibq56', 'ibq57', 'ibq58','ibq66')], na.rm = TRUE)
# all_withclin_4.7$total <- rowSums(all_withclin_4.7[, paste0("ibq", 1:91)], na.rm = TRUE)
# 
# #calculate mean subscale score
# all_withclin_4.7$activ <- all_withclin_4.7$activ / 7
# all_withclin_4.7$distress <- all_withclin_4.7$distress / 7
# all_withclin_4.7$fear <- all_withclin_4.7$fear / 6
# all_withclin_4.7$orient <- all_withclin_4.7$orient / 6
# all_withclin_4.7$smile <- all_withclin_4.7$smile / 7
# all_withclin_4.7$hipleas <- all_withclin_4.7$hipleas / 7
# all_withclin_4.7$lowpleas <- all_withclin_4.7$lowpleas / 7
# all_withclin_4.7$sooth <- all_withclin_4.7$sooth / 7
# all_withclin_4.7$react <- all_withclin_4.7$react / 6
# all_withclin_4.7$cuddl <- all_withclin_4.7$cuddl / 6
# all_withclin_4.7$percept <- all_withclin_4.7$percept / 6
# all_withclin_4.7$sad <- all_withclin_4.7$sad / 6
# all_withclin_4.7$approach <- all_withclin_4.7$approach / 6
# all_withclin_4.7$voc <- all_withclin_4.7$voc / 7
# 
# #subset data
# age_3 <- subset(all_withclin4.2, age == 3)
# age_5 <- subset(all_withclin4.2, age == 5)
# age_7 <- subset(all_withclin4.2, age == 7)
# age_9 <- subset(all_withclin4.2, age == 9)
# age_12 <- subset(all_withclin4.2, age == 12)
# 
# #calculate NAs
# colSums(is.na(age_3))
# colSums(is.na(age_5))
# colSums(is.na(age_7))
# colSums(is.na(age_9))
# colSums(is.na(age_12))
# 
# print(na_counts_all)
# 
# 
# #REVERSE SCORE EMO ASD
# emo_asd_dx$ibq33 <- 8 - emo_asd_dx$ibq33
# emo_asd_dx$ibq60 <- 8 - emo_asd_dx$ibq60
# emo_asd_dx$ibq3 <- 8 - emo_asd_dx$ibq3
# emo_asd_dx$ibq82 <- 8 - emo_asd_dx$ibq82
# emo_asd_dx$ibq84 <- 8 - emo_asd_dx$ibq84
# emo_asd_dx$ibq86 <- 8 - emo_asd_dx$ibq86
# emo_asd_dx$ibq91 <- 8 - emo_asd_dx$ibq91
# emo_asd_dx$ibq37 <- 8 - emo_asd_dx$ibq37
# emo_asd_dx$ibq72 <- 8 - emo_asd_dx$ibq72
# emo_asd_dx$ibq61 <- 8 - emo_asd_dx$ibq61
# emo_asd_dx$ibq50 <- 8 - emo_asd_dx$ibq50
# emo_asd_dx$ibq73 <- 8 - emo_asd_dx$ibq73
# 
# #calculate subscale scores + total score for nonimputed data
# emo_asd_dx$activ <- rowSums(emo_asd_dx[c('ibq33', 'ibq34', 'ibq35', 'ibq39', 'ibq54', 'ibq55', 'ibq70')], na.rm = TRUE)
# emo_asd_dx$distress <- rowSums(emo_asd_dx[c('ibq2', 'ibq3', 'ibq4', 'ibq21', 'ibq52', 'ibq53', 'ibq62')], na.rm = TRUE)
# emo_asd_dx$fear <- rowSums(emo_asd_dx[c('ibq22', 'ibq76', 'ibq77', 'ibq78', 'ibq87', 'ibq89')], na.rm = TRUE)
# emo_asd_dx$orient <- rowSums(emo_asd_dx[c('ibq5', 'ibq6', 'ibq7', 'ibq8', 'ibq10', 'ibq25')], na.rm = TRUE)
# emo_asd_dx$smile <- rowSums(emo_asd_dx[c('ibq9', 'ibq11', 'ibq12', 'ibq40', 'ibq42', 'ibq43', 'ibq65')], na.rm = TRUE)
# emo_asd_dx$hipleas <- rowSums(emo_asd_dx[c('ibq16', 'ibq44', 'ibq45', 'ibq46', 'ibq47', 'ibq48', 'ibq49')], na.rm = TRUE)
# emo_asd_dx$lowpleas <- rowSums(emo_asd_dx[c('ibq13', 'ibq14', 'ibq15', 'ibq17', 'ibq18', 'ibq19', 'ibq67')], na.rm = TRUE)
# emo_asd_dx$sooth <- rowSums(emo_asd_dx[c('ibq81', 'ibq82', 'ibq83', 'ibq84', 'ibq85', 'ibq86', 'ibq91')], na.rm = TRUE)
# emo_asd_dx$react <- rowSums(emo_asd_dx[c('ibq36', 'ibq37', 'ibq38', 'ibq63', 'ibq71', 'ibq72')], na.rm = TRUE)
# emo_asd_dx$cuddl <- rowSums(emo_asd_dx[c('ibq61', 'ibq50', 'ibq51', 'ibq59', 'ibq60', 'ibq73')], na.rm = TRUE)
# emo_asd_dx$percept <- rowSums(emo_asd_dx[c('ibq20', 'ibq27', 'ibq28', 'ibq29', 'ibq30', 'ibq31')], na.rm = TRUE)
# emo_asd_dx$sad <- rowSums(emo_asd_dx[c('ibq64', 'ibq74', 'ibq75', 'ibq32', 'ibq79', 'ibq80')], na.rm = TRUE)
# emo_asd_dx$approach <- rowSums(emo_asd_dx[c('ibq23', 'ibq24', 'ibq68', 'ibq69', 'ibq88', 'ibq90')], na.rm = TRUE)
# emo_asd_dx$voc <- rowSums(emo_asd_dx[c('ibq1','ibq26','ibq41','ibq56', 'ibq57', 'ibq58','ibq66')], na.rm = TRUE)
# emo_asd_dx$total <- rowSums(emo_asd_dx[, paste0("ibq", 1:91)], na.rm = TRUE)
# 
# #calculate mean subscale score
# emo_asd_dx$activ <- emo_asd_dx$activ / 7
# emo_asd_dx$distress <- emo_asd_dx$distress / 7
# emo_asd_dx$fear <- emo_asd_dx$fear / 6
# emo_asd_dx$orient <- emo_asd_dx$orient / 6
# emo_asd_dx$smile <- emo_asd_dx$smile / 7
# emo_asd_dx$hipleas <- emo_asd_dx$hipleas / 7
# emo_asd_dx$lowpleas <- emo_asd_dx$lowpleas / 7
# emo_asd_dx$sooth <- emo_asd_dx$sooth / 7
# emo_asd_dx$react <- emo_asd_dx$react / 6
# emo_asd_dx$cuddl <- emo_asd_dx$cuddl / 6
# emo_asd_dx$percept <- emo_asd_dx$percept / 6
# emo_asd_dx$sad <- emo_asd_dx$sad / 6
# emo_asd_dx$approach <- emo_asd_dx$approach / 6
# emo_asd_dx$voc <- emo_asd_dx$voc / 7
# 
