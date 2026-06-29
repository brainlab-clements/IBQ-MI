################# IBQ SCORING SYSTEM ##################


################ LOAD DATASETS #################

# [MODIFY] Import IBQ dataset
data_ibq <- read.csv("")
ibq_scoring <- data_ibq

# IF NEEDED: Reverse score items according to IBQ scoring guidelines
ibq_scoring$ibq33 <- 8 - ibq_scoring$ibq33
ibq_scoring$ibq60 <- 8 - ibq_scoring$ibq60
ibq_scoring$ibq3  <- 8 - ibq_scoring$ibq3
ibq_scoring$ibq82 <- 8 - ibq_scoring$ibq82
ibq_scoring$ibq84 <- 8 - ibq_scoring$ibq84
ibq_scoring$ibq86 <- 8 - ibq_scoring$ibq86
ibq_scoring$ibq91 <- 8 - ibq_scoring$ibq91
ibq_scoring$ibq37 <- 8 - ibq_scoring$ibq37
ibq_scoring$ibq72 <- 8 - ibq_scoring$ibq72
ibq_scoring$ibq61 <- 8 - ibq_scoring$ibq61
ibq_scoring$ibq50 <- 8 - ibq_scoring$ibq50
ibq_scoring$ibq73 <- 8 - ibq_scoring$ibq73

# Compute subscale sum scores from individual IBQ items
ibq_scoring$activ <- rowSums(ibq_scoring[c('ibq33', 'ibq34', 'ibq35', 'ibq39', 'ibq54', 'ibq55', 'ibq70')], na.rm = TRUE)
ibq_scoring$distress <- rowSums(ibq_scoring[c('ibq2', 'ibq3', 'ibq4', 'ibq21', 'ibq52', 'ibq53', 'ibq62')], na.rm = TRUE)
ibq_scoring$fear <- rowSums(ibq_scoring[c('ibq22', 'ibq76', 'ibq77', 'ibq78', 'ibq87', 'ibq89')], na.rm = TRUE)
ibq_scoring$orient <- rowSums(ibq_scoring[c('ibq5', 'ibq6', 'ibq7', 'ibq8', 'ibq10', 'ibq25')], na.rm = TRUE)
ibq_scoring$smile <- rowSums(ibq_scoring[c('ibq9', 'ibq11', 'ibq12', 'ibq40', 'ibq42', 'ibq43', 'ibq65')], na.rm = TRUE)
ibq_scoring$hipleas <- rowSums(ibq_scoring[c('ibq16', 'ibq44', 'ibq45', 'ibq46', 'ibq47', 'ibq48', 'ibq49')], na.rm = TRUE)
ibq_scoring$lowpleas <- rowSums(ibq_scoring[c('ibq13', 'ibq14', 'ibq15', 'ibq17', 'ibq18', 'ibq19', 'ibq67')], na.rm = TRUE)
ibq_scoring$sooth <- rowSums(ibq_scoring[c('ibq81', 'ibq82', 'ibq83', 'ibq84', 'ibq85', 'ibq86', 'ibq91')], na.rm = TRUE)
ibq_scoring$react <- rowSums(ibq_scoring[c('ibq36', 'ibq37', 'ibq38', 'ibq63', 'ibq71', 'ibq72')], na.rm = TRUE)
ibq_scoring$cuddl <- rowSums(ibq_scoring[c('ibq61', 'ibq50', 'ibq51', 'ibq59', 'ibq60', 'ibq73')], na.rm = TRUE)
ibq_scoring$percept <- rowSums(ibq_scoring[c('ibq20', 'ibq27', 'ibq28', 'ibq29', 'ibq30', 'ibq31')], na.rm = TRUE)
ibq_scoring$sad <- rowSums(ibq_scoring[c('ibq64', 'ibq74', 'ibq75', 'ibq32', 'ibq79', 'ibq80')], na.rm = TRUE)
ibq_scoring$approach <- rowSums(ibq_scoring[c('ibq23', 'ibq24', 'ibq68', 'ibq69', 'ibq88', 'ibq90')], na.rm = TRUE)
ibq_scoring$voc <- rowSums(ibq_scoring[c('ibq1','ibq26','ibq41','ibq56', 'ibq57', 'ibq58','ibq66')], na.rm = TRUE)

# Convert subscale sums to mean scores
ibq_scoring$activ <- ibq_scoring$activ / 7
ibq_scoring$distress <- ibq_scoring$distress / 7
ibq_scoring$fear <- ibq_scoring$fear / 6
ibq_scoring$orient <- ibq_scoring$orient / 6
ibq_scoring$smile <- ibq_scoring$smile / 7
ibq_scoring$hipleas <- ibq_scoring$hipleas / 7
ibq_scoring$lowpleas <- ibq_scoring$lowpleas / 7
ibq_scoring$sooth <- ibq_scoring$sooth / 7
ibq_scoring$react <- ibq_scoring$react / 6
ibq_scoring$cuddl <- ibq_scoring$cuddl / 6
ibq_scoring$percept <- ibq_scoring$percept / 6
ibq_scoring$sad <- ibq_scoring$sad / 6
ibq_scoring$approach <- ibq_scoring$approach / 6
ibq_scoring$voc <- ibq_scoring$voc / 7

# ====================
# Calculate higher-order dimension scores
# ====================

# Create dataframes for dimension calculation 
ibq_scoring_3fac <- ibq_scoring 
ibq_scoring_4fac <- ibq_scoring

##################### ORIGINAL 3 FACTOR MODEL  ###################

# Calculate Surgency dimension score
ibq_scoring_3fac$sur <- rowMeans(
  ibq_scoring_3fac[, c('approach', 'voc', 'hipleas', 'smile', 'activ', 'percept')],
  na.rm = TRUE
)

# Calculate Negative Affectivity dimension score
ibq_scoring_3fac$neg <- rowMeans(
  cbind(
    ibq_scoring_3fac[, c('sad', 'distress', 'fear')],
    8 - ibq_scoring_3fac$react
  ),
  na.rm = TRUE
)

# Calculate Orienting/Regulatory dimension score
ibq_scoring_3fac$reg <- rowMeans(
  ibq_scoring_3fac[, c('lowpleas', 'cuddl', 'orient', 'sooth')],
  na.rm = TRUE
)

# [MODIFY]: Add in file path for file output
write.csv(ibq_scoring_3fac, file = "IBQ_Scoring_3FactorModel.csv", row.names = FALSE)

####################### PROPOSED 4 FACTOR MODEL #######################

# Calculate Surgency dimension score
ibq_scoring_4fac$sur <- rowMeans(
  ibq_scoring_4fac[, c('approach', 'voc', 'hipleas', 'smile', 'activ', 'percept')],
  na.rm = TRUE
)

# Calculate Negative Affectivity dimension score
ibq_scoring_4fac$neg <- rowMeans(
  ibq_scoring_4fac[, c('sad', 'distress', 'fear', 'activ')],
  na.rm = TRUE
)

# Calculate Regulatory dimension score
ibq_scoring_4fac$reg <- rowMeans(
  cbind(
    ibq_scoring_4fac$sooth, 8 - ibq_scoring_4fac$react),
  na.rm = TRUE
)

# Calculate Sustained Engagement dimension score
ibq_scoring_4fac$suseng <- rowMeans(
  ibq_scoring_4fac[, c('lowpleas', 'orient')],
  na.rm = TRUE
)

# [MODIFY]: Add in file path for file output
write.csv(ibq_scoring_4fac, file = "IBQ_Scoring_4FactorModel.csv", row.names = FALSE)

