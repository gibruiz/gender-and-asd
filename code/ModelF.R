load("./data/df.RData")
setwd("./results")
attach(df)
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, car, ordinal, lmtest, gtsummary, reshape2, ggplot2, gtools)

######## MODELS F and G ########
#outcome: group    predictor: CT   cov: site

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
               "Right precentral", "Right precuneus", "Right rostral anterior cingulate",
               "Right rostral middle frontal", "Right superior frontal",
               "Right superior parietal", "Right superior temporal", "Right supramarginal",
               "Right transverse temporal", "Right insula")


############ AGE RANGE 1 -> 6 to 10 yo ############
# Defines age range
df6_10 <- subset(df, AGE<11)
attach(df6_10)

#### Checking multicollinearity
# Defining blank array for collinearity results
mltcln <- array(0,c(62,2))
# Linear model for each ROI
for (i in 1:62){
  m1 <- lm(AGE ~ df6_10[,i+32] + SITE_ID, data = df6_10)
  mltcln[i,] <- car::vif(m1)[,1]
}
# Collinearity results, expect values<10
mltcln
confint(m1)


###### ORDINAL MODELS
# Defining blank string to register significant results
sig_m_1     <- rep(NA,62)
# Defining blank list for model results
results_m_1 <- as.list(rep(NA,62))
# Defining blank list for the proportional odds test
pr_odds     <- as.list(rep(NA,62))
pr_odds

## MODEL F: Ordinal model for each ROI with fixed effects
# Proportional odds test
# Summary table: Odds Ratio with 95% CI, p values and stars for significance
# Registers which models were significant to run Model G
for (i in 1:62){
  m_1 <- MASS::polr(GROUP ~ df6_10[,i+32] + SITE_ID, data=df6_10, Hess = T)
  # pr_odds[i]        <- car::poTest(m_1)
  # results_m_1[[i]]  <- gtsummary::tbl_regression(m_1, exponentiate = T,
  #                                                estimate_fun = purrr::partial(style_ratio, digits = 3),
  #                                                include = !SITE_ID,
  #                                                label = list(df6_10[,i+32] ~ roi_names[i])
  #                                                ) %>% gtsummary::add_global_p() %>% 
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  if(lmtest::coeftest(m_1)[1,4] < 0.05) {sig_m_1[i] <- i+32}
}
gt::gtsave(as_gt(gtsummary::tbl_stack(results_m_1)), filename = "6_10ModelF.png")
sig_m_1 <- sig_m_1[!is.na(sig_m_1)]
sig_m_1

for(i in sig_m_1){print(i)}
df6_10$SITE_ID <- as.factor(df6_10$SITE_ID)

## MODEL G: Ordinal model with random effects for SITE_ID
# Defining blank string to register significant results
sig_m_1_random      <- rep(NA,62)
# Defining blank list for model results
results_m_1_random  <- as.list(rep(NA,62))
# Defining blank list for the proportional odds test
pr_odds_random      <- as.list(rep(NA,62))
# Proportional odds test
# Summary table: Odds Ratio with 95% CI, p values and stars for significance
for (i in sig_m_1){
  m_1_random <- clmm2(GROUP ~ df6_10[,i] + (1|SITE_ID), data=df6_10, Hess = T)
  pr_odds_random[i]        <- car::poTest(m_1_random)
  results_m_1_random[[i]]  <- results_6m1[i,] <- as_tibble(cbind(OR = exp(coef(m_1_random)[4]),
                                                                 t(confint(m_1_random)[4,]),
                                                                 p = lmtest::coeftest(m_1_random)[4,4]
                                                                 ))
  if(lmtest::coeftest(m_1_random)[1,4] < 0.05) {sig_m_1_random[i] <- i}
}
gt::gtsave(as_gt(results_m_1_random), filename = "6_10ModelG.png")

df6_10.m <- df6_10 %>%
  select(GROUP, all_of(sig_6m1_random))
df6_10.m <- melt(df6_10.m, id.vars = 'GROUP') #redefinir pra pegar só colunas que foram sig
sizes <- ggplot(df6_10.m, aes(y = value)) +
  geom_boxplot(aes(x=GROUP, y = value))+
  facet_wrap( ~ variable)
sizes
ggsave("roimeans6_10.png")


############ AGE RANGE 2 -> 11 to 14 yo ############
# Defines age range
df11_14 <- subset(df, AGE>=11 & AGE<15)
attach(df11_14)

#### Checking multicollinearity
# Defining blank array for collinearity results
mltcln  <- array(0,c(62,2))
# Linear model for each ROI
for (i in 1:62){
  m2 <- lm(AGE ~ df11_14[,i+32] + SITE_ID, data = df11_14)
  mltcln[i,] <- car::vif(m2)[,1]
}
# Collinearity results, expect values<10
mltcln
confint(m2)

# Defining blank string to register significant results
sig_m_2     <- rep(NA,62)
# Defining blank list for model results
results_m_2 <- as.list(rep(NA,62))
# Defining blank list for the propotional odds test
pr_odds     <- as.list(rep(NA,62))

## Ordinal model for each ROI with fixed effects
# Proportional odds test
# Summary table: Odds Ratio with 95% CI, p values and stars for significance
# Registers which models were significant to run Model G
for (i in 1:62){
  m_2 <- MASS::polr(GROUP ~ df11_14[,i+32] + SITE_ID, data=df11_14, Hess = T)
  pr_odds[i]        <- car::poTest(m_2)
  results_m_2[[i]]  <- gtsummary::tbl_regression(m_2, exponentiate = T,
                                                 estimate_fun = purrr::partial(style_ratio, digits = 3),
                                                 include = !SITE_ID,
                                                 label = list(df11_14[,i+32] ~ roi_names[i])
                                                 ) %>% gtsummary::add_global_p() %>% 
    gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  if(lmtest::coeftest(m_2)[1,4] < 0.05) {sig_m_2[i] <- i+32}
}
gt::gtsave(as_gt(gtsummary::tbl_stack(results_m_2)), filename = "11_14ModelF.png")
sig_m_2 <- sig_m_2[!is.na(sig_m_2)]
sig_m_2

############ AGE RANGE 3 -> 15 to 17 yo ############
# Defines age range
df15_17 <- subset(df, AGE>=15 & AGE<18)
attach(df15_17)

#### Checking multicollinearity
# Defining blank array for collinearity results
mltcln  <- array(0,c(62,2))
# Linear model for each ROI
for (i in 1:62){
  m3 <- lm(AGE ~ df15_17[,i+32] + SITE_ID, data = df15_17)
  mltcln[i,] <- car::vif(m3)[,1]
}
# Collinearity results, expect values<10
mltcln
confint(m3)

# Defining blank string to register significant results
sig_m_3     <- rep(NA,62)
# Defining blank list for model results
results_m_3 <- as.list(rep(NA,62))
# Defining blank list for the propotional odds test
pr_odds     <- as.list(rep(NA,62))

## Ordinal model for each ROI with fixed effects
# Proportional odds test
# Summary table: Odds Ratio with 95% CI, p values and stars for significance
# Registers which models were significant to run Model G
for (i in 1:62){
  m_3 <- MASS::polr(GROUP ~ df15_17[,i+32] + SITE_ID, data=df15_17, Hess = T)
  pr_odds[i]        <- car::poTest(m_3)
  results_m_3[[i]]  <- gtsummary::tbl_regression(m_3, exponentiate = T,
                                                 estimate_fun = purrr::partial(style_ratio, digits = 3),
                                                 include = !SITE_ID,
                                                 label = list(df15_17[,i+32] ~ roi_names[i])
                                                 ) %>% gtsummary::add_global_p() %>% 
    gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  if(lmtest::coeftest(m_3)[1,4] < 0.05) {sig_m_3[i] <- i+32}
}
gt::gtsave(as_gt(gtsummary::tbl_stack(results_m_3)), filename = "15_17ModelF.png")
sig_m_3 <- sig_m_3[!is.na(sig_m_3)]
sig_m_3



############ AGE RANGE 4 -> 18 to 24 yo ############
# Defines age range
df18_24 <- subset(df, AGE>17)
attach(df18_24)

#### Checking multicollinearity
# Defining blank array for collinearity results
mltcln  <- array(0,c(62,2))
# Linear model for each ROI
for (i in 1:62){
  m4 <- lm(AGE ~ df18_24[,i+32] + SITE_ID, data = df18_24)
  mltcln[i,] <- car::vif(m4)[,1]
}
# Collinearity results, expect values<10
mltcln
confint(m4)

# Defining blank string to register significant results
sig_m_4     <- rep(NA,62)
# Defining blank list for model results
results_m_4 <- as.list(rep(NA,62))
# Defining blank list for the propotional odds test
pr_odds     <- as.list(rep(NA,62))

## Ordinal model for each ROI with fixed effects
# Proportional odds test
# Summary table: Odds Ratio with 95% CI, p values and stars for significance
# Registers which models were significant to run Model G
for (i in 1:62){
  m_4 <- MASS::polr(GROUP ~ df18_24[,i+32] + SITE_ID, data=df18_24, Hess = T)
  pr_odds[i]        <- car::poTest(m_4)
  results_m_4[[i]]  <- gtsummary::tbl_regression(m_4, exponentiate = T,
                                                 estimate_fun = purrr::partial(style_ratio, digits = 3),
                                                 include = !SITE_ID,
                                                 label = list(df18_24[,i+32] ~ roi_names[i])
                                                 ) %>% gtsummary::add_global_p() %>% 
    gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  if(lmtest::coeftest(m_4)[1,4] < 0.05) {sig_m_4[i] <- i+32}
}
gt::gtsave(as_gt(gtsummary::tbl_stack(results_m_4)), filename = "18_24ModelF.png")
sig_m_4 <- sig_m_4[!is.na(sig_m_4)]
sig_m_4
