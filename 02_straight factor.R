#Straight Factor Analysis
library(dplyr)
library(lavaan)
library(semTools)
library(psych)
#set working path
setwd("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI")

#run
date_str <- format(Sys.Date(), "%m-%d-%Y")
yourinitials <- "CL" #example: YK

#Load merged dataset (not reversed)
#[MODIFY] change file name to the one with the date that you just ran + UR INITIALS
all_data_og <- read.csv("/Volumes/PSY/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output/all_withclin_11-24-2025_CL.csv")
all_data <- all_data_og

#reverse score items (for all datasets since none were reverse scored)
all_data$ibq33 <- 8 - all_data$ibq33
all_data$ibq60 <- 8 - all_data$ibq60
all_data$ibq3 <- 8 - all_data$ibq3
all_data$ibq82 <- 8 - all_data$ibq82
all_data$ibq84 <- 8 - all_data$ibq84
all_data$ibq86 <- 8 - all_data$ibq86
all_data$ibq91 <- 8 - all_data$ibq91
all_data$ibq37 <- 8 - all_data$ibq37
all_data$ibq72 <- 8 - all_data$ibq72
all_data$ibq61 <- 8 - all_data$ibq61
all_data$ibq50 <- 8 - all_data$ibq50
all_data$ibq73 <- 8 - all_data$ibq73

#create and save CSV for later use/save
file3 <- file.path("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output", paste0("all_data_", date_str, "_", yourinitials,"_reversed.csv", sep = ""))
write.csv(all_data, file = file3, row.names = FALSE)

#remove any participants with more than 15% missing data
row_na_count <- apply(all_data, 1, function(row) sum(is.na(row)))

# Calculate the percentage of NAs in each row
row_na_percentage <- row_na_count / ncol(all_data) * 100

# Calculate the number of NAs in each column
col_na_count <- colSums(is.na(all_data))
col_na_percentage <- (col_na_count / nrow(all_data)) * 100

# Display results
row_results <- data.frame(
  Study_ID = all_data$studyid,
  Study = all_data$study,
  Age = all_data$age,
  NA_Count = row_na_count,
  NA_Percentage = row_na_percentage
)

NA_dataset<- merge(
  all_data, row_results, by.x = c("studyid", "age"), by.y = c("Study_ID", "Age"), all.x = TRUE)

# write csv for missing data 
file4 <- file.path("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output", paste0("missing_data_", date_str, "_", yourinitials,".csv", sep = ""))
write.csv(NA_dataset, file = file4, row.names = FALSE)

#sanity check
count(NA_dataset %>% filter(NA_Percentage >=15)) #133 have more than 15% missing data 
 
#remove all participants with more than 15% NA data
clean_data <- NA_dataset %>%
  filter(NA_Percentage <= 15) # SANITY CHECK n = 1032 - CL:yes

file5 <- file.path("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output", paste0("cleaned_data_all_", date_str, "_", yourinitials,".csv", sep = ""))
write.csv(clean_data, file = file5, row.names = FALSE)

#REMOVE ISP NON TD KIDS
clean_data_factor <- clean_data %>%
  filter(!(study == "isp" & dx != 0)) # SANITY CHECK; n=948 -CL:yes

file6 <- file.path("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output", paste0("data_factor_analysis_", date_str, "_", yourinitials,".csv", sep = ""))
write.csv(clean_data_factor, file = file6, row.names = FALSE)


############ STOP HERE: MOVE ONTO NEXT SCRIPT: straight factor analysis clean #########


# 
# #remove NA percentage and count columns
# clean_data_factor <- clean_data_factor %>%
#   select(-Study, -NA_Count, -NA_Percentage)
# 
# #Calculate subscale scores (to do factor analysis on a subscale level instead of item level)
# clean_data_factor$activ <- rowSums(clean_data_factor[c('ibq33', 'ibq34', 'ibq35', 'ibq39', 'ibq54', 'ibq55', 'ibq70')], na.rm = TRUE)
# clean_data_factor$distress <- rowSums(clean_data_factor[c('ibq2', 'ibq3', 'ibq4', 'ibq21', 'ibq52', 'ibq53', 'ibq62')], na.rm = TRUE)
# clean_data_factor$fear <- rowSums(clean_data_factor[c('ibq22', 'ibq76', 'ibq77', 'ibq78', 'ibq87', 'ibq89')], na.rm = TRUE)
# clean_data_factor$orient <- rowSums(clean_data_factor[c('ibq5', 'ibq6', 'ibq7', 'ibq8', 'ibq10', 'ibq25')], na.rm = TRUE)
# clean_data_factor$smile <- rowSums(clean_data_factor[c('ibq9', 'ibq11', 'ibq12', 'ibq40', 'ibq42', 'ibq43', 'ibq65')], na.rm = TRUE)
# clean_data_factor$hipleas <- rowSums(clean_data_factor[c('ibq16', 'ibq44', 'ibq45', 'ibq46', 'ibq47', 'ibq48', 'ibq49')], na.rm = TRUE)
# clean_data_factor$lowpleas <- rowSums(clean_data_factor[c('ibq13', 'ibq14', 'ibq15', 'ibq17', 'ibq18', 'ibq19', 'ibq67')], na.rm = TRUE)
# clean_data_factor$sooth <- rowSums(clean_data_factor[c('ibq81', 'ibq82', 'ibq83', 'ibq84', 'ibq85', 'ibq86', 'ibq91')], na.rm = TRUE)
# clean_data_factor$react <- rowSums(clean_data_factor[c('ibq36', 'ibq37', 'ibq38', 'ibq63', 'ibq71', 'ibq72')], na.rm = TRUE)
# clean_data_factor$cuddl <- rowSums(clean_data_factor[c('ibq61', 'ibq50', 'ibq51', 'ibq59', 'ibq60', 'ibq73')], na.rm = TRUE)
# clean_data_factor$percept <- rowSums(clean_data_factor[c('ibq20', 'ibq27', 'ibq28', 'ibq29', 'ibq30', 'ibq31')], na.rm = TRUE)
# clean_data_factor$sad <- rowSums(clean_data_factor[c('ibq64', 'ibq74', 'ibq75', 'ibq32', 'ibq79', 'ibq80')], na.rm = TRUE)
# clean_data_factor$approach <- rowSums(clean_data_factor[c('ibq23', 'ibq24', 'ibq68', 'ibq69', 'ibq88', 'ibq90')], na.rm = TRUE)
# clean_data_factor$voc <- rowSums(clean_data_factor[c('ibq1','ibq26','ibq41','ibq56', 'ibq57', 'ibq58','ibq66')], na.rm = TRUE)
# 
# ##Calculate mean subscale score 
# clean_data_factor$activ <- clean_data_factor$activ / 7
# clean_data_factor$distress <- clean_data_factor$distress / 7
# clean_data_factor$fear <- clean_data_factor$fear / 6
# clean_data_factor$orient <- clean_data_factor$orient / 6
# clean_data_factor$smile <- clean_data_factor$smile / 7
# clean_data_factor$hipleas <- clean_data_factor$hipleas / 7
# clean_data_factor$lowpleas <- clean_data_factor$lowpleas / 7
# clean_data_factor$sooth <- clean_data_factor$sooth / 7
# clean_data_factor$react <- clean_data_factor$react / 6
# clean_data_factor$cuddl <- clean_data_factor$cuddl / 6
# clean_data_factor$percept <- clean_data_factor$percept / 6
# clean_data_factor$sad <- clean_data_factor$sad / 6
# clean_data_factor$approach <- clean_data_factor$approach / 6
# clean_data_factor$voc <- clean_data_factor$voc / 7
# 
# ##Calculate FACTOR score
# clean_data_factor$sur <- rowMeans(clean_data_factor[, c('approach', 'voc', 'hipleas', 'smile', 'activ', 'percept')], na.rm = TRUE)
# clean_data_factor$neg <- rowMeans(
#   cbind(
#     clean_data_factor[, c('sad', 'distress', 'fear')], 
#     8-clean_data_factor$react),
#     na.rm = TRUE)
# clean_data_factor$reg <- rowMeans(clean_data_factor[, c('lowpleas', 'cuddl', 'orient', 'sooth')], na.rm = TRUE)
# 
# 
# ########### TEST FOR NORMALITY #############
# shapiro.test(clean_data_factor$activ)
# shapiro.test(clean_data_factor$distress)
# shapiro.test(clean_data_factor$fear)
# shapiro.test(clean_data_factor$orient)
# shapiro.test(clean_data_factor$smile)
# shapiro.test(clean_data_factor$hipleas)
# shapiro.test(clean_data_factor$lowpleas)
# shapiro.test(clean_data_factor$sooth)
# shapiro.test(clean_data_factor$react)
# shapiro.test(clean_data_factor$cuddl)
# shapiro.test(clean_data_factor$percept)
# shapiro.test(clean_data_factor$sad)
# shapiro.test(clean_data_factor$approach)
# shapiro.test(clean_data_factor$voc)
# 
# ###########ITEM TOTAL CORRELATIONS###########
# #item total correlation
# #create dataframes for each subscale
# activ_sub <- clean_data_factor[c('ibq33', 'ibq34', 'ibq35', 'ibq39', 'ibq54', 'ibq55', 'ibq70')]
# distress_sub <- clean_data_factor[c('ibq2', 'ibq3', 'ibq4', 'ibq21', 'ibq52', 'ibq53', 'ibq62')]
# fear_sub <- clean_data_factor[c('ibq22', 'ibq76', 'ibq77', 'ibq78', 'ibq87', 'ibq89')]
# orient_sub <- clean_data_factor[c('ibq5', 'ibq6', 'ibq7', 'ibq8', 'ibq10', 'ibq25')]
# smile_sub <- clean_data_factor[c('ibq9', 'ibq11', 'ibq12', 'ibq40', 'ibq42', 'ibq43', 'ibq65')]
# hipleas_sub <- clean_data_factor[c('ibq16', 'ibq44', 'ibq45', 'ibq46', 'ibq47', 'ibq48', 'ibq49')]
# lowpleas_sub <- clean_data_factor[c('ibq13', 'ibq14', 'ibq15', 'ibq17', 'ibq18', 'ibq19', 'ibq67')]
# sooth_sub <- clean_data_factor[c('ibq81', 'ibq82', 'ibq83', 'ibq84', 'ibq85', 'ibq86', 'ibq91')]
# react_sub <- clean_data_factor[c('ibq36', 'ibq37', 'ibq38', 'ibq63', 'ibq71', 'ibq72')]
# cuddl_sub <- clean_data_factor[c('ibq61', 'ibq50', 'ibq51', 'ibq59', 'ibq60', 'ibq73')]
# percept_sub <- clean_data_factor[c('ibq20', 'ibq27', 'ibq28', 'ibq29', 'ibq30', 'ibq31')]
# sad_sub <- clean_data_factor[c('ibq64', 'ibq74', 'ibq75', 'ibq32', 'ibq79', 'ibq80')]
# approach_sub <- clean_data_factor[c('ibq23', 'ibq24', 'ibq68', 'ibq69', 'ibq88', 'ibq90')]
# voc_sub <- clean_data_factor[c('ibq1','ibq26','ibq41','ibq56', 'ibq57', 'ibq58','ibq66')]
# 
# #item total correlation for each subscale
# item.total(activ_sub) 
# #weak item: 33, 70, overall alpha around 0.68
# item.total(distress_sub)
# #weak item: 21, overall around 0.76
# item.total(fear_sub)
# #weak item: 22, overall 0.7
# item.total(orient_sub)
# item.total(smile_sub)
# item.total(hipleas_sub)
# item.total(lowpleas_sub)
# item.total(sooth_sub)
# item.total(react_sub)
# item.total(cuddl_sub)
# item.total(percept_sub)
# item.total(sad_sub)
# item.total(approach_sub)
# item.total(voc_sub)
# 
# ########### START FACTOR ANALYSIS ###########
# #subset data JUST for factor analysis -- subscale level (do we need to do this? no because CFA?)
# #gartstein 2003
# ibqmod3fac_mean <- '
#   surg =~ approach + voc + hipleas + smile + percept + activ
#   negemo =~ sad + distress + fear + react
#   regu =~ lowpleas + cuddl + orient + sooth'
# gart2003fit <- cfa(ibqmod3fac_mean, data=clean_data_factor, estimator="MLR")
# summary(gart2003fit, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
# fitmeasures(gart2003fit, c("cfi", "tli", "rmsea", "srmr"))
# #  cfi   tli rmsea  srmr 
# #0.746 0.687 0.117 0.126 
# # chi sq 1038/3874.150
# 
# #remove cuddliness
# ibqmod3fac_nocud <- '
#   surg =~ approach + voc + hipleas + smile + percept + activ
#   negemo =~ sad + distress + fear + react
#   regu =~ lowpleas + orient + sooth'
# fit_nocud <- cfa(ibqmod3fac_nocud, data=clean_data_factor, estimator="MLR")
# summary(fit_nocud, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
# fitmeasures(fit_nocud, c("cfi", "tli", "rmsea", "srmr"))
# #  cfi   tli rmsea  srmr 
# #0.799 0.747 0.109 0.112 
# # chi-square:  766.215/3578.645
# 
# #enlow
# bosquetfit <- '
#   surg =~ hipleas + smile + percept + voc + activ
#   negemo =~ distress + sad + react + fear + voc + activ
#   regu =~ lowpleas + orient + fear'
# bosquet_all <- cfa(bosquetfit, data=clean_data_factor, estimator="MLR")
# summary(bosquet_all, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
# fitmeasures(bosquet_all, c("cfi", "tli", "rmsea", "srmr"))
# #  cfi   tli rmsea  srmr 
# #0.917 0.880 0.080 0.066
# #chi-square: 269.415/2839.500
# 
# #sung 2022
# sungmod1 <-'
# surg =~ smile + percept + voc + activ + approach + fear
# negemo =~ activ + smile + percept + approach + fear + distress + react + sad 
# regu =~ lowpleas + orient + sooth'
# 
# sungmodel <- cfa(sungmod1, data=clean_data_factor)
# summary(sungmodel, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
# fitmeasures(sungmodel, c("cfi", "tli", "rmsea", "srmr"))
# 
# 
# sungmod2 <-'
# surg =~ smile + hipleas + approach + voc + fear + react
# negemo =~ fear + distress + react + sad
# regu =~ orient'
# 
# sungmodel2 <- cfa(sungmod2, data=clean_data_factor)
# summary(sungmodel2, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
# fitmeasures(sungmodel2, c("cfi", "tli", "rmsea", "srmr"))
# 
# #dragan 2011
# draganfit <- '
# fac1 =~ hipleas + approach + sooth + smile + cuddl + voc + react + lowpleas
# fac2 =~ hipleas + distress + sad + react + activ + fear
# fac3 =~ smile + cuddl + orient + percept + lowpleas + fear'
# 
# draganmodel <- cfa(draganfit, data=clean_data_factor)
# summary(draganmodel, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
# fitmeasures(draganmodel, c("cfi", "tli", "rmsea", "srmr"))
# 
# 
# 
# 
# 
# 
# # ########## MERGE DEMOGRAPHIC DATA ###########
# # test_ibq <- clean_data_factor
# # 
# # test_demo <- read.csv("test_demographics.csv")
# # 
# # test_demo_joined <- inner_join(test_demo, test_ibq, by = "studyid")
# # test_demo_joined$race<- as.factor(test_demo_joined$race)
# # 
# # write.csv(test_demo_joined, file = "test_demographics_ibq_merge.csv", row.names = FALSE)
# 
# # test_demo_joined <- read.csv("test_demographics_ibq_merge.csv")
# # ibqmod3fac_mean <- '
# #   surg =~ approach + voc + hipleas + smile + percept + activ
# #   negemo =~ sad + distress + fear + react
# #   regu =~ lowpleas + cuddl + orient + sooth'
# # 
# # testfit_configural <- cfa(ibqmod3fac_mean, data = test_demo_joined, group = "race") ##toof ew in some groups
# # 
# # ##drop 
# # test_clean_data <- subset(
# #   test_demo_joined,
# #   !is.na(race) &
# #     !race %in% c("","American Indian or Alaska Native", "Other")
# # )
# # 
# # # Drop unused factor levels to avoid lavaan confusion
# # test_clean_data$race <- droplevels(as.factor(test_clean_data$race))
# # 
# # testfit_configural1 <- cfa(bosquetfit, data = test_clean_data, group = "race") ##toof ew in some groups
# # summary(testfit_configural1, fit.measures = TRUE, standardized = TRUE) #doesnt work if include the smaller groups (AI, Other, AAPI)
# # measurementInvariance(model = bosquetfit, data = test_clean_data, group = "race")
# # 
# # 
# # ####AGE INVARIANCE#####
# # clean_data_age <- clean_data_factor
# # clean_data_age$age <- as.factor(clean_data_age$age)
# # 
# # testfit_configural_age <- cfa(bosquetfit, data = clean_data_age, group = "age")
# # summary(testfit_configural_age, fit.measures = TRUE, standardized = TRUE)
# # measurementInvariance(model = bosquetfit, data = testfit_configural_age, group = "race")
# 
# 
# ############EXPLORATORY FACTOR ANALYSIS#############
# 
# #create new dataset for subscale data for EFA
# efa_data <- clean_data_factor
# efa_data_factor <- efa_data %>%
#   select(-studyid, -age, -study, -dx)
# efa_data_subscale <- efa_data %>%
#   select(activ, distress, fear, orient, smile, hipleas, lowpleas, sooth, react, cuddl, percept, sad, approach, voc)
# 
# # efa_data_factor <- fa(data = efa_data_subscale, nfactors = 4, rotate = "oblimin")
# # print(efa_data_factor)
# # 
# # efa_cor_matrix <- cor(efa_data_subscale, use = "pairwise.complete.obs")
# # efa_results <- fa(r = efa_data_subscale, nfactors = 3, rotate = "oblimin", fm = "ml")
# # print(efa_results, cut = 0.3) 
# # efa_results$TLI
# # efa_results$CFI
# # efa_results$RMSEA
# # fa.diagram(efa_results)
# # 
# # efa_results$TLI   # Tucker-Lewis Index
# # efa_results$CFI   # Comparative Fit Index
# # 
# # # cfafromefa_og_nocross <- '
# # #   fac1 =~ activ + smile + hipleas + sooth + react + percept + approach + voc
# # #   fac2 =~ distress + fear + sad
# # #   fac3 =~ orient + lowpleas
# # '
# 
# #create discovery and replication sample 
# efa_data_subscale_explore <- efa_data_subscale
# install.packages("caret")
# library(caret)
# 
# set.seed(42)
# n <- nrow(efa_data_subscale_explore)
# efa_train<- sample(seq_len(n), size = 0.7 * n)
# 
# efa_discovery   <- efa_data_subscale_explore[efa_train, ]
# efa_replication <- efa_data_subscale_explore[-efa_train, ]
# 
# #create cor matrix to run EFA
# efa_cor_matrix_dis <- cor(efa_discovery, use = "pairwise.complete.obs")
# efa_results_dis <- fa(r = efa_cor_matrix_dis, nfactors = 3, rotate = "oblimin", fm = "ml")
# print(efa_results_dis, cut = 0.3) 
# 
# #use raw data to calculate EFA CFI, TLI and other fit indices
# efa_results_dis <- fa(efa_discovery, nfactors = 3, rotate = "oblimin", fm = "ml")
# efa_results_dis$TLI
# efa_results_dis$CFI
# efa_results_dis$RMSEA
# 
# #graph
# fa.diagram(efa_results_dis)
# 
# 
# ################ CONFIRMATORY FACTOR ANALYSIS ####################
# cfafromefa3 <- '
#   fac1 =~ activ + fear + smile + hipleas + percept + approach + voc
#   fac2 =~ activ + distress + fear + sooth + react + cuddl + sad
#   fac3 =~ orient + lowpleas'
# 
# #remove cuddl
# cfafromefa3_nocud <- '
#   fac1 =~ activ + fear + smile + hipleas + percept + approach + voc
#   fac2 =~ activ + distress + fear + sooth + react + sad
#   fac3 =~ orient + lowpleas'
# 
# #move neg loadings to 4th factor
# cfafromefa3_nocud_4 <- '
#   fac1 =~ activ + fear + smile + hipleas + percept + approach + voc
#   fac2 =~ activ + distress + fear + sad
#   fac3 =~ orient + lowpleas
#   fac4 =~ sooth + react ' 
# 
# #remove fear from fac 1 --> final factor structure
# cfafromefa4_neg <- '
#   fac1 =~ activ + smile + hipleas + percept + approach + voc
#   fac2 =~ activ + distress + fear + sad
#   fac3 =~ orient + lowpleas
#   fac4 =~ sooth + react '
# 
# # cfafromefa <- '
# #   fac1 =~ activ + fear + smile + hipleas + cuddl + approach + voc + percept + distress
# #   fac2 =~ activ + distress + fear + sooth + react + cuddl + sad + percept
# #   fac3 =~ orient + lowpleas + cuddl + percept
# 
# # cfafromefa_3fac <- '
# #   fac1 =~ activ + smile + hipleas + sooth + percept + approach + voc
# #   fac2 =~ distress + fear + react + sad 
# #   fac3 =~ orient + lowpleas + percept
# #   fac4 =~ activ + sooth + cuddl'
# 
# #### [MODIFY: model structure] fit indices for replication data for each model   
# test_cfa <- cfa(cfafromefa4_neg, data = efa_replication)
# summary(test_cfa, fit.measures = TRUE, standardized = TRUE)
# modificationIndices(test_cfa)
# mi <- modificationIndices(test_cfa)
# mi_sorted <- mi[order(-mi$mi), ]
# 
# ########### FOR AGE ANALYSIS BUT NOT USED ############
# # efa_data_3 <- efa_data %>%
# #   filter(age == 3)
# # efa_data_3_sub <- efa_data_3 %>%
# #   select(activ, distress, fear, orient, smile, hipleas, lowpleas, sooth, react, cuddl, percept, sad, approach, voc)
# # efa_cor_matrix3 <- cor(efa_data_3_sub, use = "pairwise.complete.obs")
# # efa_results3 <- fa(r = efa_cor_matrix3, nfactors = 3, rotate = "oblimin", fm = "ml")
# # print(efa_results3, cut = 0) 
# # fa.diagram(efa_results3)
# # 
# # 
# # efa_data_5 <- efa_data %>%
# #   filter(age == 5)
# # 
# # efa_data_7 <- efa_data %>%
# #   filter(age == 7)
# # 
# # efa_data_9 <- efa_data %>%
# #   filter(age == 9)
# # 
# # efa_data_12 <- efa_data %>%
# #   filter(age == 12)
# # 
