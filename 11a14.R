setwd("/home/gisele/Documentos/ABIDE")
load("df.RData")
setwd("/home/gisele/Documentos/ABIDE/descritivo")
attach(df)
library(dplyr)
library(ggplot2)

df <- subset(df, AGE>=11 & AGE<15) #faixa etaria
attach(df)

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


#demographics
summary(AGE)
summ_geral <- df %>% #tabela sumário da faixa etária
  summarise(across(AGE, list(median = median, mean = mean, sd = sd, n = length)))
summ_geral
summ_groups <- df %>% group_by(SEX, DX_GROUP) %>% #tabela sumário da faixa etária por grupo
  summarise(across(AGE, list(median = median, mean = mean, sd = sd, n = length)))
summ_groups
summ_sex <- df %>% group_by(SEX) %>% #tabela sumário da faixa etária por gênero
  summarise(across(AGE, list(median = median, mean = mean, sd = sd, n = length)))
summ_sex
summ_dx <- df %>% group_by(DX_GROUP) %>% #tabela sumário da faixa etária por diag
  summarise(across(AGE, list(median = median, mean = mean, sd = sd, n = length)))
summ_dx

write.table(format(as.data.frame(summ_geral),digits = 4), "11a14.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(summ_groups),digits = 4), "11a14grupos.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(summ_sex),digits = 4), "11a14sex.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(summ_dx),digits = 4), "11a14dx.csv", sep = ",", quote = F, row.names = F)

histog <- ggplot(df, aes(x = AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df$SEX ~ df$DX_GROUP)
histog
ggsave("11a14.png")


#primeiro modelo
#outcome: CT    predictor: dx   cov: gender, site, totalCT

results_1m1  <-array(0,c(62,4)) #definindo uma tabela em branco pra cada modelo
results_1m2  <-array(0,c(62,4))
results_1m3  <-array(0,c(62,4))

for (i in 1:62){ #for pra fazer cada modelo usando cada roi
  m1m1 <-lm(df[,i+32] ~ DX_GROUP, data=df)
  m1m2 <-lm(df[,i+32] ~ DX_GROUP * SEX, data=df)
  m1m3 <-lm(df[,i+32] ~ DX_GROUP * SEX + SITE_ID + TOTAL_VOLUME, data=df)
  
  results_1m1[i,] <-summary(m1m1)$coefficient[2,1:4] #colocando os coeficientes resultado
  results_1m2[i,] <-summary(m1m2)$coefficient[2,1:4] #do modelo na tabela q tava em branco
  results_1m3[i,] <-summary(m1m3)$coefficient[2,1:4]
}
results_1m1fdr <- data.frame(roi_names, results_1m1)#cria tabela pra corrigir fdr
results_1m2fdr <- data.frame(roi_names, results_1m2)
results_1m3fdr <- data.frame(roi_names, results_1m3)

results_1m1fdr[,4] <- p.adjust(results_1m1[,4],method = "fdr") #ajusta o p em cada tabela
results_1m2fdr[,4] <- p.adjust(results_1m2[,4],method = "fdr")
results_1m3fdr[,4] <- p.adjust(results_1m3[,4],method = "fdr")


write.table(format(results_1m1fdr, digits = 5, scientific = F),"11_14Modelo1.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)")) #salva cada tabela com o nome correspondente
write.table(format(results_1m2fdr, digits = 5, scientific = F),"11_14M1Sex.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))
write.table(format(results_1m3fdr, digits = 5, scientific = F),"11_14M1Sex_site_totalV.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))

#segundo modelo
#outcome: dx    predictor: ct   cov: gender, site, totalCT

results_2m1  <-array(0,c(62,4)) #definindo uma tabela em branco pra cada modelo
results_2m2  <-array(0,c(62,4))

for (i in 1:62){ #for pra fazer cada modelo usando cada roi
  m2m1 <-glm(DX_GROUP ~ df[,i+32] + SEX + SITE_ID + TOTAL_VOLUME, family = binomial(logit), data=df)
  m2m2 <-glm(DX_GROUP ~ df[,i+32] + SEX, family = binomial(logit), data=df)
  m2m2
  
  results_2m1[i,] <-summary(m2m1)$coefficient[2,1:4] #colocando os coeficientes resultado
  results_2m2[i,] <-summary(m2m2)$coefficient[2,1:4] #do modelo na tabela q tava em branco
}
results_2m1fdr  <- data.frame(roi_names, results_2m1)#cria tabela pra corrigir fdr
results_2m2fdr  <- data.frame(roi_names, results_2m2)
results_2m1fdr[,4]  <- p.adjust(results_2m1[,4],method = "fdr") #ajusta o p em cada tabela
results_2m2fdr[,4]  <- p.adjust(results_2m2[,4],method = "fdr")

write.table(format(results_2m1fdr, digits = 5, scientific = F),"11_14M2Sex_site_totalV.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)")) #salva cada tabela com o nome correspondente
write.table(format(results_2m2fdr, digits = 5, scientific = F),"11_14M2Sex.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))

#terceiro modelo
#outcome: adi-r social    predictor: dx   cov: gender, site, totalCT

results_3m1  <-array(0,c(62,4)) #definindo uma tabela em branco pra cada modelo
results_3m2  <-array(0,c(62,4))
results_3m3  <-array(0,c(62,4))
for (i in 1:62){ #for pra fazer cada modelo usando cada roi
  m3m1 <-lm(ADI_R_SOCIAL_TOTAL_A ~ df[,i+32], data=df)
  m3m2 <-lm(ADI_R_SOCIAL_TOTAL_A ~ df[,i+32] + SEX, data=df)
  m3m3 <-lm(ADI_R_SOCIAL_TOTAL_A ~ df[,i+32] + SEX + SITE_ID + TOTAL_VOLUME, data=df)
  
  results_3m1[i,] <-summary(m3m1)$coefficient[2,1:4] #colocando os coeficientes resultado
  results_3m2[i,] <-summary(m3m2)$coefficient[2,1:4] #do modelo na tabela q tava em branco
  results_3m3[i,] <-summary(m3m3)$coefficient[2,1:4]
}
results_3m1fdr  <- data.frame(roi_names, results_3m1)#cria tabela pra corrigir fdr
results_3m2fdr  <- data.frame(roi_names, results_3m2)
results_3m3fdr  <- data.frame(roi_names, results_3m3)
results_3m1fdr[,4]  <- p.adjust(results_3m1[,4],method = "fdr") #ajusta o p em cada tabela
results_3m2fdr[,4]  <- p.adjust(results_3m2[,4],method = "fdr")
results_3m3fdr[,4]  <- p.adjust(results_3m3[,4],method = "fdr")

write.table(format(results_3m1fdr, digits = 5, scientific = F),"11_14Modelo3.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)")) #salva cada tabela com o nome correspondente
write.table(format(results_3m2fdr, digits = 5, scientific = F),"11_14M3Sex.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))
write.table(format(results_3m3fdr, digits = 5, scientific = F),"11_14M3Sex_site_totalV.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))

#quarto modelo
#outcome: adi-r verbal    predictor: ct   cov: gender, site, totalCT

results_4m1  <-array(0,c(62,4)) #definindo uma tabela em branco pra cada modelo
results_4m2  <-array(0,c(62,4))
results_4m3  <-array(0,c(62,4))
for (i in 1:62){ #for pra fazer cada modelo usando cada roi
  m4m1 <-lm(ADI_R_VERBAL_TOTAL_BV ~ df[,i+32], data=df)
  m4m2 <-lm(ADI_R_VERBAL_TOTAL_BV ~ df[,i+32] + SEX, data=df)
  m4m3 <-lm(ADI_R_VERBAL_TOTAL_BV ~ df[,i+32] + SEX + SITE_ID + TOTAL_VOLUME, data=df)
  
  results_4m1[i,] <-summary(m4m1)$coefficient[2,1:4] #colocando os coeficientes resultado
  results_4m2[i,] <-summary(m4m2)$coefficient[2,1:4] #do modelo na tabela q tava em branco
  results_4m3[i,] <-summary(m4m3)$coefficient[2,1:4]
}
results_4m1fdr  <- data.frame(roi_names, results_4m1)#cria tabela pra corrigir fdr
results_4m2fdr  <- data.frame(roi_names, results_4m2)
results_4m3fdr  <- data.frame(roi_names, results_4m3)
results_4m1fdr[,4]  <- p.adjust(results_4m1[,4],method = "fdr") #ajusta o p em cada tabela
results_4m2fdr[,4]  <- p.adjust(results_4m2[,4],method = "fdr")
results_4m3fdr[,4]  <- p.adjust(results_4m3[,4],method = "fdr")

write.table(format(results_4m1fdr, digits = 5, scientific = F),"11_14Modelo4.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)")) #salva cada tabela com o nome correspondente
write.table(format(results_4m2fdr, digits = 5, scientific = F),"11_14M4Sex.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))
write.table(format(results_4m3fdr, digits = 5, scientific = F),"11_14M4Sex_site_totalV.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))

#quinto modelo
#outcome: adi-r verbal    predictor: ct   cov: gender, site, totalCT

results_5m1  <-array(0,c(62,4)) #definindo uma tabela em branco pra cada modelo
results_5m2  <-array(0,c(62,4))
results_5m3  <-array(0,c(62,4))
for (i in 1:62){ #for pra fazer cada modelo usando cada roi
  m5m1 <-lm(ADI_RRB_TOTAL_C ~ df[,i+32], data=df)
  m5m2 <-lm(ADI_RRB_TOTAL_C ~ df[,i+32] + SEX, data=df)
  m5m3 <-lm(ADI_RRB_TOTAL_C ~ df[,i+32] + SEX + SITE_ID + TOTAL_VOLUME, data=df)
  
  results_5m1[i,] <-summary(m5m1)$coefficient[2,1:4] #colocando os coeficientes resultado
  results_5m2[i,] <-summary(m5m2)$coefficient[2,1:4] #do modelo na tabela q tava em branco
  results_5m3[i,] <-summary(m5m3)$coefficient[2,1:4]
}
results_5m1fdr  <- data.frame(roi_names, results_5m1)#cria tabela pra corrigir fdr
results_5m2fdr  <- data.frame(roi_names, results_5m2)#e inclui labels das roi 
results_5m3fdr  <- data.frame(roi_names, results_5m3)
results_5m1fdr[,4]  <- p.adjust(results_5m1[,4],method = "fdr") #ajusta o p em cada tabela
results_5m2fdr[,4]  <- p.adjust(results_5m2[,4],method = "fdr")
results_5m3fdr[,4]  <- p.adjust(results_5m3[,4],method = "fdr")

write.table(format(results_5m1fdr, digits = 5, scientific = F),"11_14Modelo5.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)")) #salva cada tabela com o nome correspondente
write.table(format(results_5m2fdr, digits = 5, scientific = F),"11_14M5Sex.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))
write.table(format(results_5m3fdr, digits = 5, scientific = F),"11_14M5Sex_site_totalV.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))
