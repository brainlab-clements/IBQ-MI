################# IBQ SCORING SYSTEM ##################


################ LOAD DATASETS #################
##### After running IBQ_Clean_Code.R, import dataset after removing 15% missing and dx

# [MODIFY] Import cleaned dataset prepared for factor analyses 
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
ibq_scoring$activ <- rowMeans(ibq_scoring[c('ibq33', 'ibq34', 'ibq35', 'ibq39', 'ibq54', 'ibq55', 'ibq70')], na.rm = TRUE)
ibq_scoring$distress <- rowMeans(ibq_scoring[c('ibq2', 'ibq3', 'ibq4', 'ibq21', 'ibq52', 'ibq53', 'ibq62')], na.rm = TRUE)
ibq_scoring$fear <- rowMeans(ibq_scoring[c('ibq22', 'ibq76', 'ibq77', 'ibq78', 'ibq87', 'ibq89')], na.rm = TRUE)
ibq_scoring$orient <- rowMeans(ibq_scoring[c('ibq5', 'ibq6', 'ibq7', 'ibq8', 'ibq10', 'ibq25')], na.rm = TRUE)
ibq_scoring$smile <- rowMeans(ibq_scoring[c('ibq9', 'ibq11', 'ibq12', 'ibq40', 'ibq42', 'ibq43', 'ibq65')], na.rm = TRUE)
ibq_scoring$hipleas <- rowMeans(ibq_scoring[c('ibq16', 'ibq44', 'ibq45', 'ibq46', 'ibq47', 'ibq48', 'ibq49')], na.rm = TRUE)
ibq_scoring$lowpleas <- rowMeans(ibq_scoring[c('ibq13', 'ibq14', 'ibq15', 'ibq17', 'ibq18', 'ibq19', 'ibq67')], na.rm = TRUE)
ibq_scoring$sooth <- rowMeans(ibq_scoring[c('ibq81', 'ibq82', 'ibq83', 'ibq84', 'ibq85', 'ibq86', 'ibq91')], na.rm = TRUE)
ibq_scoring$react <- rowMeans(ibq_scoring[c('ibq36', 'ibq37', 'ibq38', 'ibq63', 'ibq71', 'ibq72')], na.rm = TRUE)
ibq_scoring$cuddl <- rowMeans(ibq_scoring[c('ibq61', 'ibq50', 'ibq51', 'ibq59', 'ibq60', 'ibq73')], na.rm = TRUE)
ibq_scoring$percept <- rowMeans(ibq_scoring[c('ibq20', 'ibq27', 'ibq28', 'ibq29', 'ibq30', 'ibq31')], na.rm = TRUE)
ibq_scoring$sad <- rowMeans(ibq_scoring[c('ibq64', 'ibq74', 'ibq75', 'ibq32', 'ibq79', 'ibq80')], na.rm = TRUE)
ibq_scoring$approach <- rowMeans(ibq_scoring[c('ibq23', 'ibq24', 'ibq68', 'ibq69', 'ibq88', 'ibq90')], na.rm = TRUE)
ibq_scoring$voc <- rowMeans(ibq_scoring[c('ibq1','ibq26','ibq41','ibq56', 'ibq57', 'ibq58','ibq66')], na.rm = TRUE)


# ====================
# Calculate higher-order factor scores
# ====================

# Create dataframes for factor calculation 
ibq_scoring_3fac <- ibq_scoring 
ibq_scoring_4fac <- ibq_scoring

##################### ORIGINAL 3 FACTOR SCORING ###################

# Calculate Surgency factor score
ibq_scoring_3fac$sur <- rowMeans(
  ibq_scoring_3fac[, c('approach', 'voc', 'hipleas', 'smile', 'activ', 'percept')],
  na.rm = TRUE
)

# Calculate Negative Affectivity factor score
ibq_scoring_3fac$neg <- rowMeans(
  cbind(
    ibq_scoring_3fac[, c('sad', 'distress', 'fear')],
    8 - ibq_scoring_3fac$react
  ),
  na.rm = TRUE
)

# Calculate Orienting/Regulatory factor score
ibq_scoring_3fac$reg <- rowMeans(
  ibq_scoring_3fac[, c('lowpleas', 'cuddl', 'orient', 'sooth')],
  na.rm = TRUE
)

# [MODIFY]: Add in file path for file output
write.csv(ibq_scoring_3fac, file = "IBQ_Scoring_3Factor.csv", row.names = FALSE)

####################### PROPOSED 4 FACTOR MODEL #######################

# Calculate Surgency factor score
ibq_scoring_4fac$sur <- rowMeans(
  ibq_scoring_4fac[, c('approach', 'voc', 'hipleas', 'smile', 'activ', 'percept')],
  na.rm = TRUE
)

# Calculate Negative Affectivity factor score
ibq_scoring_4fac$neg <- rowMeans(
  ibq_scoring_4fac[, c('sad', 'distress', 'fear', 'activ')],
  na.rm = TRUE
)

# Calculate Regulatory factor score
ibq_scoring_4fac$reg <- rowMeans(
  ibq_scoring_4fac[, c('sooth', 'react')],
  na.rm = TRUE
)

# Calculate Sustained Engagement factor score
ibq_scoring_4fac$suseng <- rowMeans(
  ibq_scoring_4fac[, c('lowpleas', 'orient')],
  na.rm = TRUE
)

# [MODIFY]: Add in file path for file output
write.csv(ibq_scoring_4fac, file = "IBQ_Scoring_4Factor.csv", row.names = FALSE)

