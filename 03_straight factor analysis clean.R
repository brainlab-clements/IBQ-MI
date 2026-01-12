#Straight Factor Analysis
library(dplyr)
library(lavaan)
library(semTools)
library(semPlot)
library(psych)
library(lavaan.mi)
library(multilevel)
######START HERE########
#data is in data folder on Yvonne's factor analysis drive folder :D
setwd("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI")

#RUN: 
date_str <- format(Sys.Date(), "%m-%d-%Y")
yourinitials <- "CL" #example: YK


# [MODIFY] CHANGE THE DATE OF WHEN U RAN IT TO ENSURE YOU ARE UPLOADING CORRECT DATASET 
#[MODIFY] change file name to the one with the date that you just ran + UR INITIALS
#upload cleaned factor data from google drive
clean_data_factor <- read.csv("/Volumes/PSY/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output/data_factor_analysis_11-24-2025_CL.csv")

#remove NA percentage and count columns
clean_data_factor <- clean_data_factor %>%
  select(-Study, -NA_Count, -NA_Percentage)

#Calculate subscale scores (to do factor analysis on a subscale level instead of item level)
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

##Calculate mean subscale score 
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

##Calculate FACTOR score
clean_data_factor$sur <- rowMeans(clean_data_factor[, c('approach', 'voc', 'hipleas', 'smile', 'activ', 'percept')], na.rm = TRUE)
clean_data_factor$neg <- rowMeans(
  cbind(
    clean_data_factor[, c('sad', 'distress', 'fear')], 
    8-clean_data_factor$react),
  na.rm = TRUE)
clean_data_factor$reg <- rowMeans(clean_data_factor[, c('lowpleas', 'cuddl', 'orient', 'sooth')], na.rm = TRUE)

file7 <- file.path("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output", paste0("data_analysis_all_calculated_", date_str, "_", yourinitials,".csv", sep = ""))
write.csv(clean_data, file = file7, row.names = FALSE) #CL(Nov 24,2025): clean_data not found


#check with Yvonne's data_analysis_all_calculated_10-22-2025_YK.csv to compare 

# write.csv(clean_data_factor, file = "data_analysis_all_calculated.csv", row.names = FALSE)

########### TEST FOR NORMALITY #############
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

###########ITEM TOTAL CORRELATIONS###########
#item total correlation
#create dataframes for each subscale
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

#item total correlation for each subscale
item.total(activ_sub) 
#weak item: 33, 70, overall alpha around 0.68
item.total(distress_sub)
#weak item: 21, overall around 0.76
item.total(fear_sub)
#weak item: 22, overall 0.7
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

########### START FACTOR ANALYSIS ###########
#subset data JUST for factor analysis -- subscale level (do we need to do this? no because CFA?)
#gartstein 2003
ibqmod3fac_mean <- '
  surg =~ approach + voc + hipleas + smile + percept + activ
  negemo =~ sad + distress + fear + react
  regu =~ lowpleas + cuddl + orient + sooth'
gart2003fit <- cfa(ibqmod3fac_mean, data=clean_data_factor, estimator="MLR")
summary(gart2003fit, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
fitmeasures(gart2003fit, c("cfi", "tli", "rmsea", "srmr"))
semPaths(gart2003fit, "std", sizeLat = 7, edge.label.cex = 0.75)
##### COMPARE OUTPUT WITH THESE
#  cfi   tli rmsea  srmr 
#0.746 0.687 0.117 0.126 
# chi sq 1038/3874.150

#remove cuddliness
ibqmod3fac_nocud <- '
  surg =~ approach + voc + hipleas + smile + percept + activ
  negemo =~ sad + distress + fear + react
  regu =~ lowpleas + orient + sooth'
fit_nocud <- cfa(ibqmod3fac_nocud, data=clean_data_factor, estimator="MLR")
summary(fit_nocud, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
fitmeasures(fit_nocud, c("cfi", "tli", "rmsea", "srmr"))
semPaths(fit_nocud, "std", sizeLat = 7, edge.label.cex = 0.75)
#  cfi   tli rmsea  srmr 
#0.799 0.747 0.109 0.112 
# chi-square:  766.215/3578.645

#enlow
bosquetfit <- '
  surg =~ hipleas + smile + percept + voc + activ
  negemo =~ distress + sad + react + fear + voc + activ
  regu =~ lowpleas + orient + fear'
bosquet_all <- cfa(bosquetfit, data=clean_data_factor, estimator="MLR")
summary(bosquet_all, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
fitmeasures(bosquet_all, c("cfi", "tli", "rmsea", "srmr"))
semPaths(bosquet_all, "std", sizeLat = 7, edge.label.cex = 0.75)
#  cfi   tli rmsea  srmr 
#0.917 0.880 0.080 0.066
#chi-square: 269.415/2839.500

#sung 2022
sungmod1 <-'
surg =~ smile + percept + voc + activ + approach + fear
negemo =~ activ + smile + percept + approach + fear + distress + react + sad 
regu =~ lowpleas + orient + sooth'

sungmodel <- cfa(sungmod1, data=clean_data_factor)
summary(sungmodel, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
fitmeasures(sungmodel, c("cfi", "tli", "rmsea", "srmr"))
semPaths(sungmodel, "std", sizeLat = 7, edge.label.cex = 0.75)
#   cfi   tli rmsea  srmr 
#0.857 0.795 0.100 0.088 

sungmod2 <-'
surg =~ smile + hipleas + approach + voc + fear + react
negemo =~ fear + distress + react + sad
regu =~ orient'

sungmodel2 <- cfa(sungmod2, data=clean_data_factor)
summary(sungmodel2, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
fitmeasures(sungmodel2, c("cfi", "tli", "rmsea", "srmr"))
semPaths(sungmodel2, "std", sizeLat = 7, edge.label.cex = 0.75)
#   cfi   tli rmsea  srmr 
#0.954 0.928 0.069 0.044 

#dragan 2011
draganfit <- '
fac1 =~ hipleas + approach + sooth + smile + cuddl + voc + react + lowpleas
fac2 =~ hipleas + distress + sad + react + activ + fear
fac3 =~ smile + cuddl + orient + percept + lowpleas + fear'

draganmodel <- cfa(draganfit, data=clean_data_factor)
summary(draganmodel, rsquare=TRUE, stand=TRUE, fit.measures=TRUE)
fitmeasures(draganmodel, c("cfi", "tli", "rmsea", "srmr"))
semPaths(draganmodel, "std", sizeLat = 7, edge.label.cex = 0.75)
### DID NOT CONVERGE


############EXPLORATORY FACTOR ANALYSIS#############
#create new dataset for subscale data for EFA
efa_data <- clean_data_factor
efa_data_factor <- efa_data %>%
  dplyr::select(-studyid, -age, -study, -dx)
efa_data_subscale <- efa_data %>%
  dplyr::select(activ, distress, fear, orient, smile, hipleas, lowpleas, sooth, react, cuddl, percept, sad, approach, voc)

#create discovery and replication sample 
efa_data_subscale_explore <- efa_data_subscale
##install.packages("caret")
library(caret)

set.seed(42)
n <- nrow(efa_data_subscale_explore)
efa_train<- sample(seq_len(n), size = 0.7 * n)

efa_discovery   <- efa_data_subscale_explore[efa_train, ]
efa_replication <- efa_data_subscale_explore[-efa_train, ]

#create cor matrix to run EFA
efa_cor_matrix_dis <- cor(efa_discovery, use = "pairwise.complete.obs")
efa_results_dis <- fa(r = efa_cor_matrix_dis, nfactors = 3, rotate = "oblimin", fm = "ml")
print(efa_results_dis, cut = 0.3) 

#use raw data to calculate EFA CFI, TLI and other fit indices
efa_results_dis <- fa(efa_discovery, nfactors = 3, rotate = "oblimin", fm = "ml")
efa_results_dis$TLI
efa_results_dis$CFI
efa_results_dis$RMSEA

#graph
fa.diagram(efa_results_dis)


################ CONFIRMATORY FACTOR ANALYSIS ####################
cfafromefa3 <- '
  fac1 =~ activ + fear + smile + hipleas + percept + approach + voc
  fac2 =~ activ + distress + fear + sooth + react + cuddl + sad
  fac3 =~ orient + lowpleas'

test_cfa <- cfa(cfafromefa3, data = efa_replication)
summary(test_cfa, fit.measures = TRUE, standardized = TRUE)
modificationIndices(test_cfa)
mi <- modificationIndices(test_cfa)
mi_sorted <- mi[order(-mi$mi), ]

#remove cuddl
cfafromefa3_nocud <- '
  fac1 =~ activ + fear + smile + hipleas + percept + approach + voc
  fac2 =~ activ + distress + fear + sooth + react + sad
  fac3 =~ orient + lowpleas'

test_cfa <- cfa(cfafromefa3_nocud, data = efa_replication)
summary(test_cfa, fit.measures = TRUE, standardized = TRUE)
modificationIndices(test_cfa)
mi <- modificationIndices(test_cfa)
mi_sorted <- mi[order(-mi$mi), ]

#move neg loadings to 4th factor
cfafromefa3_nocud_4 <- '
  fac1 =~ activ + fear + smile + hipleas + percept + approach + voc
  fac2 =~ activ + distress + fear + sad
  fac3 =~ orient + lowpleas
  fac4 =~ sooth + react ' 

test_cfa <- cfa(cfafromefa3_nocud_4, data = efa_replication)
summary(test_cfa, fit.measures = TRUE, standardized = TRUE)
modificationIndices(test_cfa)
mi <- modificationIndices(test_cfa)
mi_sorted <- mi[order(-mi$mi), ]

#remove fear from fac 1 --> final factor structure
cfafromefa4_neg <- '
  fac1 =~ activ + smile + hipleas + percept + approach + voc
  fac2 =~ activ + distress + fear + sad
  fac3 =~ orient + lowpleas
  fac4 =~ sooth + react '

test_cfa <- cfa(cfafromefa4_neg, data = efa_replication)
summary(test_cfa, fit.measures = TRUE, standardized = TRUE)
modificationIndices(test_cfa)
mi <- modificationIndices(test_cfa)
mi_sorted <- mi[order(-mi$mi), ]

#####test statistic for FULL SAMPLE:
test_cfa <- cfa(cfafromefa4_neg, data = efa_data_subscale)
summary(test_cfa, fit.measures = TRUE, standardized = TRUE)
modificationIndices(test_cfa)
mi <- modificationIndices(test_cfa)
mi_sorted <- mi[order(-mi$mi), ]


#plot CFA
semPaths(
  test_cfa,
  whatLabels = "std",    # show standardized loadings
  layout = "tree",       # hierarchical layout
  edge.color = "black",
  sizeMan = 6,           # item node size
  sizeLat = 10,          # factor node size
  nCharNodes = 0,        # full item names
  residuals = TRUE,      # show residuals
  exoCov = TRUE,        # hide covariances among exogenous variables
  intercepts = FALSE
)


############ AGE INVARIANCE ##########
age_invar_data <- efa_data
age_invar_data$age <- as.factor(age_invar_data$age)

MI_age <- cfa(model = cfafromefa4_neg, data = age_invar_data, group = "age")
summary(MI_age, fit.measures = TRUE, standardized = TRUE)
measurementInvariance(model = cfafromefa4_neg, data = age_invar_data, group = "age")

confit <- cfa(cfafromefa4_neg, data = age_invar_data, std.lv = TRUE, group = "age")

#remove 3 year olds becauase model did not converge
age_invar_data2 <- subset(age_invar_data, age != "3")
age_invar_data2$age <- droplevels(age_invar_data2$age)

# age_invar_data_5 <- age_invar_data2 %>%
#   filter(age_invar_data2$age == "5")
# 
# age_invar_data_7 <- age_invar_data2 %>%
#   filter(age_invar_data2$age == "7")
# 
# age_invar_data_9 <- age_invar_data2 %>%
#   filter(age_invar_data2$age == "9")
# 
# age_invar_data_12 <- age_invar_data2 %>%
#   filter(age_invar_data2$age == "12")

confit2 <- cfa(cfafromefa4_neg, data = age_invar_data2, std.lv = TRUE, group = "age")

weakfit <- measEq.syntax(configural.model = confit2, 
                         return.fit = TRUE,
                         group = "age",
                         group.equal = c("loadings"))
modindices(weakfit, sort = TRUE, maximum.number = 20)

strongfit <- measEq.syntax(configural.model = confit2,
                           return.fit = TRUE,
                           group = "age",
                           group.equal = c("loadings",
                                           "intercepts"),
                           group.partial = c("fear ~1", "approach ~1", "sad ~1"))

modindices(strongfit, sort = TRUE, maximum.number = 20)
pi <- lavTestScore(strongfit, epc = TRUE, univariate = TRUE)
pi_df <- bind_rows(pi, .id = "group_id") #this is so it saves as a dataframe because diff groups will be saved as diff lists

View(pi_df) 

file8 <- file.path("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output", paste0("patial_invariance_no3_", date_str, "_", yourinitials, ".csv", sep = ""))
write.csv(pi_df, file = file8, row.names = FALSE)

strictfit <- measEq.syntax(configural.model = confit2,
                           return.fit = TRUE,
                           group = "age",
                           group.equal = c("loadings",
                                           "intercepts",
                                           "residuals"))

fit_comp <- compareFit(confit2, weakfit, strongfit, strictfit, nested = TRUE)
summary(fit_comp)

######## AGE INVARIANCE REMOVING 5 MO OLDS
age_invar_data3 <- subset(age_invar_data2, age !="5")
age_invar_data3$age <- droplevels(age_invar_data3$age)

confit3 <- cfa(cfafromefa4_neg, data = age_invar_data3, std.lv = TRUE, group = "age")

weakfit <- measEq.syntax(configural.model = confit3, 
                         return.fit = TRUE,
                         group = "age",
                         group.equal = c("loadings"))
modindices(weakfit, sort = TRUE, maximum.number = 20)

strongfit <- measEq.syntax(configural.model = confit3,
                           return.fit = TRUE,
                           group = "age",
                           group.equal = c("loadings",
                                           "intercepts"),
                           group.partial = c("fear ~1", "approach ~1", "sad ~1"))

modindices(strongfit, sort = TRUE, maximum.number = 20)
pi <- lavTestScore(strongfit, epc = TRUE, univariate = TRUE)
pi_df <- bind_rows(pi, .id = "group_id") #this is so it saves as a dataframe because diff groups will be saved as diff lists

View(pi_df) 

file9 <- file.path("/Volumes/psy/BrainLab/BrainLab-Everyone-write/StudentWIPs/01_Yvonne_IBQ_ageMI/data/output", paste0("patial_invariance_no35_", date_str, "_", yourinitials,".csv", sep = ""))
write.csv(pi_df, file = file9, row.names = FALSE)

strictfit <- measEq.syntax(configural.model = confit3,
                           return.fit = TRUE,
                           group = "age",
                           group.equal = c("loadings",
                                           "intercepts",
                                           "residuals"))

fit_comp <- compareFit(confit3, weakfit, strongfit, strictfit, nested = TRUE)
summary(fit_comp)
#model did not converge 




