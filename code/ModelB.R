load("./data/df.RData")
setwd("./results")
attach(df)
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, car, ordinal, lmtest, gtsummary, reshape2, ggplot2, gtools, MASS)

### MODEL B ###
## outcome: dx    predictor: CT   cov: gender, site, totalCT

# Defines names of ROIs 
roi_names <- c("Left caudal anterior cingulate", "Left caudal middle frontal",
               "Left cuneus", "Left entorhinal", "Left fusiform", "Left inferior parietal",
               "Left inferior temporal", "Left isthmus cingulate", "Left lateral occipital",
               "Left lateral orbitofrontal", "Left lingual", "Left medial orbitofrontal",
               "Left middle temporal", "Left parahippocampal", "Left paracentral",
               "Left pars opercularis", "Left pars orbitalis", "Left pars triangularis",
               "Left pericalcarine", "Left postcentral", "Left posterior cingulate",
               "Left precentral", "Left precuneus", "Left rostral anterior cingulate",
               "Left rostral middle frontal", "Left superior frontal", "Left superior parietal",
               "Left superior temporal", "Left supramarginal", "Left transverse temporal",
               "Left insula", "Right caudal anterior cingulate", "Right caudal middle frontal",
               "Right cuneus", "Right entorhinal", "Right fusiform", "Right inferior parietal",
               "Right inferior temporal", "Right isthmus cingulate", "Right lateral occipital",
               "Right lateral orbitofrontal", "Right lingual", "Right medial orbitofrontal",
               "Right middle temporal", "Right parahippocampal", "Right paracentral",
               "Right pars opercularis", "Right pars orbitalis", "Right pars triangularis",
               "Right pericalcarine", "Right postcentral", "Right posterior cingulate",
               "Right precentral",  "Right precuneus", "Right rostral anterior cingulate",
               "Right rostral middle frontal", "Right superior frontal",
               "Right superior parietal", "Right superior temporal", "Right supramarginal",
               "Right transverse temporal", "Right insula")

### AGE RANGE 1 -> 6 to 10 yo ###
# Defines age range
df6_10 <- subset(df, AGE<11)
attach(df6_10)

# Defines blank array for model results
results_m1_1 <- array(0,c(62,4))
results_m2_1 <- array(0,c(62,4))
results_m3_1 <- array(0,c(62,4))
results_m4_1 <- array(0,c(62,4))
results_m5_1 <- array(0,c(62,4))

# Linear model for each ROI including combinations of the covariates
# Completes blank table with coefficients from linear model
for (i in 1:62){
  m1_1 <-glm(DX_GROUP ~ df6_10[,i+32], family = binomial(logit), data=df6_10)
  m2_1 <-glm(DX_GROUP ~ df6_10[,i+32] * SEX, family = binomial(logit), data=df6_10)
  m3_1 <-glm(DX_GROUP ~ df6_10[,i+32] * SEX + SITE_ID, family = binomial(logit), data=df6_10)
  m4_1 <-glm(DX_GROUP ~ df6_10[,i+32] * SEX + TOTAL_VOLUME, family = binomial(logit), data=df6_10)
  m5_1 <-glm(DX_GROUP ~ df6_10[,i+32] * SEX + SITE_ID + TOTAL_VOLUME, family = binomial(logit), data=df6_10)
  
  results_m1_1[i,] <-summary(m1_1)$coefficient[2,1:4]
  results_m2_1[i,] <-summary(m2_1)$coefficient[2,1:4]
  results_m3_1[i,] <-summary(m3_1)$coefficient[2,1:4]
  results_m4_1[i,] <-summary(m4_1)$coefficient[2,1:4]
  results_m5_1[i,] <-summary(m5_1)$coefficient[2,1:4]
  
}
# Table for FDR correction and exp(estimate)
results_m1_1fdr <- data.frame(roi_names, results_m1_1)
results_m2_1fdr <- data.frame(roi_names, results_m2_1)
results_m3_1fdr <- data.frame(roi_names, results_m3_1)
results_m4_1fdr <- data.frame(roi_names, results_m4_1)
results_m5_1fdr <- data.frame(roi_names, results_m5_1)

# OR
results_m1_1fdr[,2] <- exp(results_m1_1fdr[,2])
results_m2_1fdr[,2] <- exp(results_m2_1fdr[,2])
results_m3_1fdr[,2] <- exp(results_m3_1fdr[,2])
results_m4_1fdr[,2] <- exp(results_m4_1fdr[,2])
results_m5_1fdr[,2] <- exp(results_m5_1fdr[,2])

# FDR correction
results_m1_1fdr[,5] <- p.adjust(results_m1_1[,4],method = "fdr")
results_m2_1fdr[,5] <- p.adjust(results_m2_1[,4],method = "fdr")
results_m3_1fdr[,5] <- p.adjust(results_m3_1[,4],method = "fdr")
results_m4_1fdr[,5] <- p.adjust(results_m4_1[,4],method = "fdr")
results_m5_1fdr[,5] <- p.adjust(results_m5_1[,4],method = "fdr")

# Significance stars
results_m1_1fdr[,6] <- stars.pval(results_m1_1fdr[,5])
results_m2_1fdr[,6] <- stars.pval(results_m2_1fdr[,5])
results_m3_1fdr[,6] <- stars.pval(results_m3_1fdr[,5])
results_m4_1fdr[,6] <- stars.pval(results_m4_1fdr[,5])
results_m5_1fdr[,6] <- stars.pval(results_m5_1fdr[,5])

# Saves summary table with results
write.table(format(results_m1_1fdr, digits = 5, scientific = F),"6_10ModelB.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m2_1fdr, digits = 5, scientific = F),"6_10M_B_Sex.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m3_1fdr, digits = 5, scientific = F),"6_10M_B_Sex_site.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m4_1fdr, digits = 5, scientific = F),"6_10M_B_Sex_totalV.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m5_1fdr, digits = 5, scientific = F),"6_10M_B_Sex_site_totalV.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))



### AGE RANGE 2 -> 11 to 14 yo ###
# Defines age range
df11_14 <- subset(df, AGE>=11 & AGE<15)
attach(df11_14)

# Defines blank array for model results
results_m1_2 <- array(0,c(62,4))
results_m2_2 <- array(0,c(62,4))
results_m3_2 <- array(0,c(62,4))
results_m4_2 <- array(0,c(62,4))
results_m5_2 <- array(0,c(62,4))

# Linear model for each ROI including combinations of the covariates
# Completes blank table with coefficients from linear model
for (i in 1:62){
  m1_2 <-glm(DX_GROUP ~ df11_14[,i+32], family = binomial(logit), data=df11_14)
  m2_2 <-glm(DX_GROUP ~ df11_14[,i+32] * SEX, family = binomial(logit), data=df11_14)
  m3_2 <-glm(DX_GROUP ~ df11_14[,i+32] * SEX + SITE_ID, family = binomial(logit), data=df11_14)
  m4_2 <-glm(DX_GROUP ~ df11_14[,i+32] * SEX + TOTAL_VOLUME, family = binomial(logit), data=df11_14)
  m5_2 <-glm(DX_GROUP ~ df11_14[,i+32] * SEX + SITE_ID + TOTAL_VOLUME, family = binomial(logit), data=df11_14)
  
  results_m1_2[i,] <-summary(m1_2)$coefficient[2,1:4]
  results_m2_2[i,] <-summary(m2_2)$coefficient[2,1:4]
  results_m3_2[i,] <-summary(m3_2)$coefficient[2,1:4]
  results_m4_2[i,] <-summary(m4_2)$coefficient[2,1:4]
  results_m5_2[i,] <-summary(m5_2)$coefficient[2,1:4]
}
# Table for FDR correction and exp(estimate)
results_m1_2fdr <- data.frame(roi_names, results_m1_2)
results_m2_2fdr <- data.frame(roi_names, results_m2_2)
results_m3_2fdr <- data.frame(roi_names, results_m3_2)
results_m4_2fdr <- data.frame(roi_names, results_m4_2)
results_m5_2fdr <- data.frame(roi_names, results_m5_2)

# OR
results_m1_2fdr[,2] <- exp(results_m1_2fdr[,2])
results_m2_2fdr[,2] <- exp(results_m2_2fdr[,2])
results_m3_2fdr[,2] <- exp(results_m3_2fdr[,2])
results_m4_2fdr[,2] <- exp(results_m4_2fdr[,2])
results_m5_2fdr[,2] <- exp(results_m5_2fdr[,2])

# FDR correction
results_m1_2fdr[,5] <- p.adjust(results_m1_2[,4],method = "fdr")
results_m2_2fdr[,5] <- p.adjust(results_m2_2[,4],method = "fdr")
results_m3_2fdr[,5] <- p.adjust(results_m3_2[,4],method = "fdr")
results_m4_2fdr[,5] <- p.adjust(results_m4_2[,4],method = "fdr")
results_m5_2fdr[,5] <- p.adjust(results_m5_2[,4],method = "fdr")

# Significance stars
results_m1_2fdr[,6] <- stars.pval(results_m1_2fdr[,5])
results_m2_2fdr[,6] <- stars.pval(results_m2_2fdr[,5])
results_m3_2fdr[,6] <- stars.pval(results_m3_2fdr[,5])
results_m4_2fdr[,6] <- stars.pval(results_m4_2fdr[,5])
results_m5_2fdr[,6] <- stars.pval(results_m5_2fdr[,5])

# Saves summary table with results
write.table(format(results_m1_2fdr, digits = 5, scientific = F),"11_14ModelB.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m2_2fdr, digits = 5, scientific = F),"11_14M_B_Sex.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m3_2fdr, digits = 5, scientific = F),"11_14M_B_Sex_site.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m4_2fdr, digits = 5, scientific = F),"11_14M_B_Sex_totalV.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m5_2fdr, digits = 5, scientific = F),"11_14M_B_Sex_site_totalV.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))



### AGE RANGE 3 -> 15 to 17 yo ###
# Defines age range
df15_17 <- subset(df, AGE>=15 & AGE<18)
attach(df15_17)

# Defines blank array for model results
results_m1_3 <- array(0,c(62,4))
results_m2_3 <- array(0,c(62,4))
results_m3_3 <- array(0,c(62,4))
results_m4_3 <- array(0,c(62,4))
results_m5_3 <- array(0,c(62,4))

# Linear model for each ROI including combinations of the covariates
# Completes blank table with coefficients from linear model
for (i in 1:62){
  m1_3 <-glm(DX_GROUP ~ df15_17[,i+32], family = binomial(logit), data=df15_17)
  m2_3 <-glm(DX_GROUP ~ df15_17[,i+32] * SEX, family = binomial(logit), data=df15_17)
  m3_3 <-glm(DX_GROUP ~ df15_17[,i+32] * SEX + SITE_ID, family = binomial(logit), data=df15_17)
  m4_3 <-glm(DX_GROUP ~ df15_17[,i+32] * SEX + TOTAL_VOLUME, family = binomial(logit), data=df15_17)
  m5_3 <-glm(DX_GROUP ~ df15_17[,i+32] * SEX + SITE_ID + TOTAL_VOLUME, family = binomial(logit), data=df15_17)
  
  results_m1_3[i,] <-summary(m1_3)$coefficient[2,1:4]
  results_m2_3[i,] <-summary(m2_3)$coefficient[2,1:4]
  results_m3_3[i,] <-summary(m3_3)$coefficient[2,1:4]
  results_m4_3[i,] <-summary(m4_3)$coefficient[2,1:4]
  results_m5_3[i,] <-summary(m5_3)$coefficient[2,1:4]
}
# Table for FDR correction and exp(estimate)
results_m1_3fdr <- data.frame(roi_names, results_m1_3)
results_m2_3fdr <- data.frame(roi_names, results_m2_3)
results_m3_3fdr <- data.frame(roi_names, results_m3_3)
results_m4_3fdr <- data.frame(roi_names, results_m4_3)
results_m5_3fdr <- data.frame(roi_names, results_m5_3)

# OR
results_m1_3fdr[,2] <- exp(results_m1_3fdr[,2])
results_m2_3fdr[,2] <- exp(results_m2_3fdr[,2])
results_m3_3fdr[,2] <- exp(results_m3_3fdr[,2])
results_m4_3fdr[,2] <- exp(results_m4_3fdr[,2])
results_m5_3fdr[,2] <- exp(results_m5_3fdr[,2])

# FDR correction
results_m1_3fdr[,5] <- p.adjust(results_m1_3[,4],method = "fdr")
results_m2_3fdr[,5] <- p.adjust(results_m2_3[,4],method = "fdr")
results_m3_3fdr[,5] <- p.adjust(results_m3_3[,4],method = "fdr")
results_m4_3fdr[,5] <- p.adjust(results_m4_3[,4],method = "fdr")
results_m5_3fdr[,5] <- p.adjust(results_m5_3[,4],method = "fdr")

# Significance stars
results_m1_3fdr[,6] <- stars.pval(results_m1_3fdr[,5])
results_m2_3fdr[,6] <- stars.pval(results_m2_3fdr[,5])
results_m3_3fdr[,6] <- stars.pval(results_m3_3fdr[,5])
results_m4_3fdr[,6] <- stars.pval(results_m4_3fdr[,5])
results_m5_3fdr[,6] <- stars.pval(results_m5_3fdr[,5])

# Saves summary table with results
write.table(format(results_m1_3fdr, digits = 5, scientific = F),"15_17ModelB.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m2_3fdr, digits = 5, scientific = F),"15_17M_B_Sex.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m3_3fdr, digits = 5, scientific = F),"15_17M_B_Sex_site.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m4_3fdr, digits = 5, scientific = F),"15_17M_B_Sex_totalV.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m5_3fdr, digits = 5, scientific = F),"15_17M_B_Sex_site_totalV.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))



### AGE RANGE 4 -> 18 to 24 yo ###
# Defines age range
df18_24 <- subset(df, AGE>17)
attach(df18_24)

# Defines blank array for model results
results_m1_4 <- array(0,c(62,4))
results_m2_4 <- array(0,c(62,4))
results_m3_4 <- array(0,c(62,4))
results_m4_4 <- array(0,c(62,4))
results_m5_4 <- array(0,c(62,4))

# Linear model for each ROI including combinations of the covariates
# Completes blank table with coefficients from linear model
for (i in 1:62){
  m1_4 <-glm(DX_GROUP ~ df18_24[,i+32], family = binomial(logit), data=df18_24)
  m2_4 <-glm(DX_GROUP ~ df18_24[,i+32] * SEX, family = binomial(logit), data=df18_24)
  m3_4 <-glm(DX_GROUP ~ df18_24[,i+32] * SEX + SITE_ID, family = binomial(logit), data=df18_24)
  m4_4 <-glm(DX_GROUP ~ df18_24[,i+32] * SEX + TOTAL_VOLUME, family = binomial(logit), data=df18_24)
  m5_4 <-glm(DX_GROUP ~ df18_24[,i+32] * SEX + SITE_ID + TOTAL_VOLUME, family = binomial(logit), data=df18_24)
  
  results_m1_4[i,] <-summary(m1_4)$coefficient[2,1:4]
  results_m2_4[i,] <-summary(m2_4)$coefficient[2,1:4]
  results_m3_4[i,] <-summary(m3_4)$coefficient[2,1:4]
  results_m4_4[i,] <-summary(m4_4)$coefficient[2,1:4]
  results_m5_4[i,] <-summary(m5_4)$coefficient[2,1:4]
}
# Table for FDR correction and exp(estimate)
results_m1_4fdr <- data.frame(roi_names, results_m1_4)
results_m2_4fdr <- data.frame(roi_names, results_m2_4)
results_m3_4fdr <- data.frame(roi_names, results_m3_4)
results_m4_4fdr <- data.frame(roi_names, results_m4_4)
results_m5_4fdr <- data.frame(roi_names, results_m5_4)

# OR
results_m1_4fdr[,2] <- exp(results_m1_4fdr[,2])
results_m2_4fdr[,2] <- exp(results_m2_4fdr[,2])
results_m3_4fdr[,2] <- exp(results_m3_4fdr[,2])
results_m4_4fdr[,2] <- exp(results_m4_4fdr[,2])
results_m5_4fdr[,2] <- exp(results_m5_4fdr[,2])

# FDR correction
results_m1_4fdr[,5] <- p.adjust(results_m1_4[,4],method = "fdr")
results_m2_4fdr[,5] <- p.adjust(results_m2_4[,4],method = "fdr")
results_m3_4fdr[,5] <- p.adjust(results_m3_4[,4],method = "fdr")
results_m4_4fdr[,5] <- p.adjust(results_m4_4[,4],method = "fdr")
results_m5_4fdr[,5] <- p.adjust(results_m5_4[,4],method = "fdr")

# Significance stars
results_m1_4fdr[,6] <- stars.pval(results_m1_4fdr[,5])
results_m2_4fdr[,6] <- stars.pval(results_m2_4fdr[,5])
results_m3_4fdr[,6] <- stars.pval(results_m3_4fdr[,5])
results_m4_4fdr[,6] <- stars.pval(results_m4_4fdr[,5])
results_m5_4fdr[,6] <- stars.pval(results_m5_4fdr[,5])

# Saves summary table with results
write.table(format(results_m1_4fdr, digits = 5, scientific = F),"18_24ModelB.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m2_4fdr, digits = 5, scientific = F),"18_24M_B_Sex.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m3_4fdr, digits = 5, scientific = F),"18_24M_B_Sex_site.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m4_4fdr, digits = 5, scientific = F),"18_24M_B_Sex_totalV.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))
write.table(format(results_m5_4fdr, digits = 5, scientific = F),"18_24M_B_Sex_site_totalV.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "OR", "Std. Error", "t value", "p-value (FDR corrected)", ""))

