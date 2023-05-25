load("./data/df.RData")
attach(df)
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, car, ordinal, lmtest, gtsummary, reshape2, ggplot2, gtools, gt)


#Defining age range
df <- subset(df, AGE>=6 & AGE<11) #faixa etaria
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
               "Right precentral", "Right precuneus", "Right rostral anterior cingulate",
               "Right rostral middle frontal", "Right superior frontal",
               "Right superior parietal", "Right superior temporal", "Right supramarginal",
               "Right transverse temporal", "Right insula")



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


write.table(format(results_1m1fdr, digits = 5, scientific = F),"6_10Modelo1.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)")) #salva cada tabela com o nome correspondente
write.table(format(results_1m2fdr, digits = 5, scientific = F),"6_10M1Sex.csv", sep = ",", row.names = F, quote = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))
write.table(format(results_1m3fdr, digits = 5, scientific = F),"6_10M1Sex_site_totalV.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))

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

write.table(format(results_2m1fdr, digits = 5, scientific = F),"6_10M2Sex_site_totalV.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)")) #salva cada tabela com o nome correspondente
write.table(format(results_2m2fdr, digits = 5, scientific = F),"6_10M2Sex.csv", sep = ",", quote = F, row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))

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

write.table(format(results_3m1fdr, digits = 5, scientific = F),"6_10Modelo3.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)")) #salva cada tabela com o nome correspondente
write.table(format(results_3m2fdr, digits = 5, scientific = F),"6_10M3Sex.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))
write.table(format(results_3m3fdr, digits = 5, scientific = F),"6_10M3Sex_site_totalV.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))

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

write.table(format(results_4m1fdr, digits = 5, scientific = F),"6_10Modelo4.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)")) #salva cada tabela com o nome correspondente
write.table(format(results_4m2fdr, digits = 5, scientific = F),"6_10M4Sex.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))
write.table(format(results_4m3fdr, digits = 5, scientific = F),"6_10M4Sex_site_totalV.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))

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

write.table(format(results_5m1fdr, digits = 5, scientific = F),"6_10Modelo5.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)")) #salva cada tabela com o nome correspondente
write.table(format(results_5m2fdr, digits = 5, scientific = F),"6_10M5Sex.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))
write.table(format(results_5m3fdr, digits = 5, scientific = F),"6_10M5Sex_site_totalV.csv", quote = F, sep = ",", row.names = F, col.names = c("ROI name", "Estimate", "Std. Error", "t value", "p-value (FDR corrected)"))

#sixth model
#outcome: group    predictor: CT   cov: site

##checking multicollinearity

#defining blank array for collinearity
mltcln <- array(0,c(62,2))
#linear model for each ROI
for (i in 1:62){
  m <- lm(AGE ~ df[,i+32] + SITE_ID, data = df)
  mltcln[i,] <- car::vif(m)[,1]
}
#collinearity results, expect values<10
mltcln
confint(m)

#defining blank array for model results
results_6m1  <-array(0,c(62,3))
#defining blank string to register significant results
sig_6m1 <- rep(NA,62)
tables <- as.list(sig_6m1)

results_6m1[1,] <- a
m6m1 <- MASS::polr(GROUP ~ Mean_1002 + SITE_ID, data=df, Hess = T)
summary(m6m1)
lmtest::coeftest(m6m1)#[1,4]
coef(m6m1)
confint(m6m1)#[1,]
car::poTest(m6m1)
a <- gtsummary::tbl_regression(m6m1, exponentiate = T
                         , estimate_fun = purrr::partial(style_ratio, digits = 3)
                         # , include = !SITE_ID
                         # , label = list(df[,33] ~ roi_names[1])
                         , tidy_fun = broom.mixed::tidy
                         ) %>% 
  gtsummary::add_global_p() %>% 
  gtsummary::add_significance_stars(
    hide_p = F, hide_se = T, hide_ci = F,
    pattern = "{p.value}{stars}")
a
pr_odds     <- as.list(rep(NA,10))

## Ordinal model for each ROI
# Binds Odds Ratio with 95% CI, p values and stars for significance
for (i in 1:62){ 
  m6m1 <- MASS::polr(GROUP ~ df6_10[,i+32] + SITE_ID, data=df6_10, Hess = T)
  print(car::poTest(m6m1))
  # results_6m1[i,] <- cbind(exp(OR = coef(m6m1)[4]), exp(t(confint(m6m1)[1,])),
  #                          p = lmtest::coeftest(m6m1)[1,4],
  #                          stars.pval(lmtest::coeftest(m6m1)[1,4]))
  # if(lmtest::coeftest(m6m1)[1,4] < 0.05){
  #   sig_6m1[i] <- i+32
  # }
  # tables[[i]] <- gtsummary::tbl_regression(m6m1, exponentiate = TRUE
  #                                        , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                        , include = !SITE_ID
  # ) %>% 
  #   gtsummary::add_global_p()
}
tables
gtsummary::tbl_stack()
results_6m1
sig_6m1 <- sig_6m1[!is.na(sig_6m1)]
sig_6m1
results_6m1 <- data.frame(roi_names, results_6m1)# junta tabela dos coeficientes resultado com os nomes das regioes
write.table(format(results_6m1, digits = 5, scientific = F),"6_10ModelF.csv",
            sep = ",",row.names = F, quote = F, col.names = c("ROI name", "OR",
                                                              "p-value", "")) #salva cada tabela com o nome correspondente


#setimo modelo
#outcome: group    predictor: CT   cov: site (random effect)

#defining blank array for model results
results_7m1  <-array(0,c(62,5))
df$SITE_ID <- as.factor(df$SITE_ID)
attach(df)
m6m1 <- clmm(GROUP ~ df[,33] + (1|SITE_ID), data=df, Hess = T)
lmtest::coeftest(m6m1)[4,1:4]
# car::poTest(m6m1)
# m <- lm(Mean_1002 ~ Mean_101 + SITE_ID, data = df)
# car::vif(m)

for (i in 1:62){ #for pra fazer cada modelo usando cada roi
  m6m1 <-clmm2(GROUP ~ df[,i+32] + (1|SITE_ID), data=df, Hess = T)
  results_6m1[i,] <- as_tibble(cbind(OR = exp(coef(m6m1)[4]), t(confint(m6m1)[4,]),
                                     p = lmtest::coeftest(m6m1)[4,4]))
  #colocando os coeficientes resultado do modelo na tabela q tava em branco
  #OR = odds ratio, pega o estimate só do geral do modelo
  #segunda e terceira coluna = intervalo de confiança
  #p = p valor
  # if (inherits(t, "try-error")){
  #   m6m1 <-clmm(GROUP ~ df[,i+32] + (subject|SITE_ID), data=df, Hess = T, link = 'probit')
  #   results_6m1[i,] <- as_tibble(cbind(OR = exp(coef(m6m1)[4]), t(confint(m6m1)[4,]),
  #                          p = lmtest::coeftest(m6m1)[4,4]))
  # }
  print(results_6m1[i,])
}
results_6m1
results_6m1 <- as_tibble(cbind("Characteristic" = roi_names, results_6m1))# junta tabela dos coeficientes resultado com os nomes das regioes
write.table(format(results_6m1, digits = 5, scientific = F),"6_10Modelo6.csv",
            sep = ",",row.names = F, quote = F, col.names = c("ROI name", "OR",
                                                              "Std. Error", "t value", "p-value (FDR corrected)")) #salva cada tabela com o nome correspondente



library(reshape2)
df.m <- df %>%
  select(GROUP, all_of(sig_6m1_random))
df.m <- melt(df.m, id.vars = 'GROUP') #redefinir pra pegar só colunas que foram sig
sizes <- ggplot(df.m, aes(y = value)) +
  geom_boxplot(aes(x=GROUP, y = value))+
  facet_wrap( ~ variable)
sizes
ggsave("roimeans.png")