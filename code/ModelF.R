load("./data/df.RData")
setwd("./results")
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, car, ordinal, lmtest, gtsummary, reshape2, ggplot2,
               gtools, forcats, brant, labelled, gt, ggrain, ggpp, cowplot)

######## MODELS F and G ########
#outcome: group    predictor: CT   cov: site

# Defines names of ROIs
# No brain side indicators because we will merge tables. First is left and then right side
roi_names <- c("Caudal anterior cingulate", "Caudal middle frontal", "Cuneus",
               "Entorhinal", "Fusiform", "Inferior parietal", "Inferior temporal",
               "Isthmus cingulate", "Lateral occipital", "Lateral orbitofrontal",
               "Lingual", "Medial orbitofrontal", "Middle temporal", "Parahippocampal",
               "Paracentral", "Pars opercularis", "Pars orbitalis", "Pars triangularis",
               "Pericalcarine", "Postcentral", "Posterior cingulate", "Precentral",
               "Precuneus", "Rostral anterior cingulate", "Rostral middle frontal",
               "Superior frontal", "Superior parietal", "Superior temporal",
               "Supramarginal", "Transverse temporal", "Insula",
               "Caudal anterior cingulate", "Caudal middle frontal", "Cuneus",
               "Entorhinal", "Fusiform", "Inferior parietal", "Inferior temporal",
               "Isthmus cingulate", "Lateral occipital", "Lateral orbitofrontal",
               "Lingual", "Medial orbitofrontal", "Middle temporal", "Parahippocampal",
               "Paracentral", "Pars opercularis", "Pars orbitalis", "Pars triangularis",
               "Pericalcarine", "Postcentral", "Posterior cingulate", "Precentral",
               "Precuneus", "Rostral anterior cingulate", "Rostral middle frontal",
               "Superior frontal", "Superior parietal", "Superior temporal",
               "Supramarginal", "Transverse temporal", "Insula")


############ AGE RANGE 1 -> 6 to 10 yo ############
# Defines age range
df6_10 <- subset(df, AGE<11)
df6_10$SITE_ID <- as.factor(df6_10$SITE_ID)
var_label(df6_10[,33:94]) <- roi_names
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


###### ORDINAL MODELS
# Defining blank string to register significant results
sig_m_1     <- rep(NA,62)
# Defining blank list for model results
results_m_X1 <- as.list(rep(NA,62))
results_m_Y1 <- as.list(rep(NA,62))
results_m_Z1 <- as.list(rep(NA,62))
# Defining blank list for the proportional odds test
pr_odds1    <- array(NA,c(62,3))
deviance1   <- matrix(NA, 62, 3)

## MODEL F: Ordinal model for each ROI with fixed effects
# Proportional odds test
# Summary table: Odds Ratio with 95% CI, p values and stars for significance
# Registers which models were significant to run Model G
for (i in 1:62){
  try(m_1.X <- MASS::polr(GROUP.A ~ df6_10[,i+32] + SITE_ID, data=df6_10, Hess = T))
  try(m_1.Y <- MASS::polr(GROUP.B ~ df6_10[,i+32] + SITE_ID, data=df6_10, Hess = T))
  try(m_1.Z <- MASS::polr(GROUP.C ~ df6_10[,i+32] + SITE_ID, data=df6_10, Hess = T))
  # try(pr_odds1[i,1]  <- brant(m_1.X)[1,3])
  # try(pr_odds1[i,2]  <- brant(m_1.Y)[1,3])
  # try(pr_odds1[i,3]  <- brant(m_1.Z)[1,3])
  # deviance1[i,]      <- c(summary(m_1.X)$deviance, summary(m_1.Y)$deviance, summary(m_1.Z)$deviance)
  # results_m_X1[[i]]  <- gtsummary::tbl_regression(m_1.X, exponentiate = T
  #                                                , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                                , include = !SITE_ID
  #                                                ) %>% gtsummary::add_global_p() %>%
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  # results_m_Y1[[i]]  <- gtsummary::tbl_regression(m_1.Y, exponentiate = T
  #                                                , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                                , include = !SITE_ID
  #                                                ) %>% gtsummary::add_global_p() %>%
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  # results_m_Z1[[i]]  <- gtsummary::tbl_regression(m_1.Z, exponentiate = T
  #                                                , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                                , include = !SITE_ID
  #                                                ) %>% gtsummary::add_global_p() %>%
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  if(lmtest::coeftest(m_1.X)[1,4] < 0.05 | lmtest::coeftest(m_1.Y)[1,4] < 0.05 | lmtest::coeftest(m_1.Z)[1,4] < 0.05) {sig_m_1[i] <- colnames(df)[i+32]}
}
system("paplay /usr/share/sounds/freedesktop/stereo/complete.oga")
gt::gtsave(gtsummary::tbl_merge(list(tbl_stack(results_m_X1[1:31]), tbl_stack(results_m_X1[32:62])),
                                      tab_spanner = c("Left", "Right")) %>% as_gt() %>%
                   gt::tab_options(table.font.names = "Times New Roman"), filename = "6_10ModelF_X.png")
gt::gtsave(gtsummary::tbl_merge(list(tbl_stack(results_m_Y1[1:31]), tbl_stack(results_m_Y1[32:62])),
                                      tab_spanner = c("Left", "Right")) %>% as_gt() %>%
                   gt::tab_options(table.font.names = "Times New Roman"), filename = "6_10ModelF_Y.png")
gt::gtsave(gtsummary::tbl_merge(list(tbl_stack(results_m_Z1[1:31]), tbl_stack(results_m_Z1[32:62])),
                                      tab_spanner = c("Left", "Right")) %>% as_gt() %>%
                   gt::tab_options(table.font.names = "Times New Roman"), filename = "6_10ModelF_Z.png")
pr_odds1
deviance1
write.table(deviance1, "6_10Deviance.csv", sep = ",", row.names = F, quote = F, col.names = c("X", "Y", "Z"))
write.table(pr_odds1, "6_10POdds.csv", sep = ",", row.names = F, quote = F, col.names = c("X", "Y", "Z"))
sig_m_1 <- sig_m_1[!is.na(sig_m_1)]
sig_m_1



## MODEL G: Ordinal model with random effects for SITE_ID
# Defining blank string to register significant results
sig_m_1_random      <- rep(NA,length(sig_m_1))
# Defining blank list for model results
sig_m_1
sig_m_1.n <- c(paste("Left",var_label(df6_10$Mean_1002)),paste("Left",var_label(df6_10$Mean_1009)),paste("Left",var_label(df6_10$Mean_1015)),paste("Left",var_label(df6_10$Mean_1017)),
               paste("Left",var_label(df6_10$Mean_1024)),paste("Left",var_label(df6_10$Mean_1028)),paste("Right",var_label(df6_10$Mean_2002)),paste("Right",var_label(df6_10$Mean_2003)),
               paste("Right",var_label(df6_10$Mean_2007)),paste("Right",var_label(df6_10$Mean_2008)),paste("Right",var_label(df6_10$Mean_2009)),paste("Right",var_label(df6_10$Mean_2015)),
               paste("Right",var_label(df6_10$Mean_2017)),paste("Right",var_label(df6_10$Mean_2024)),paste("Right",var_label(df6_10$Mean_2028)))
results_m_1_random  <- tibble(ROI = rep(NA, length(sig_m_1)), OR = rep(NA, length(sig_m_1)),
                              "Low CI" = rep(NA, length(sig_m_1)), "High CI" = rep(NA, length(sig_m_1)),
                              "p-value" = rep(NA, length(sig_m_1)), " " = rep(NA, length(sig_m_1)))
results_m_1_random


# Individual models. If it doesn't converge with default link functions, replace with the following:
# link = c("logistic", "probit", "cloglog", "loglog", "cauchit", "Aranda-Ordaz", "log-gamma")
m_1_random1   <- clmm2(GROUP.C ~ Mean_1002, random = SITE_ID, data = df6_10, Hess = T)
m_1_random2   <- clmm2(GROUP.C ~ Mean_1009, random = SITE_ID, data = df6_10, Hess = T)
m_1_random3   <- clmm2(GROUP.C ~ Mean_1015, random = SITE_ID, data = df6_10, Hess = T)
m_1_random4   <- clmm2(GROUP.C ~ Mean_1017, random = SITE_ID, data = df6_10, Hess = T)
m_1_random5   <- clmm2(GROUP.C ~ Mean_1024, random = SITE_ID, data = df6_10, Hess = T)
m_1_random6   <- clmm2(GROUP.C ~ Mean_1028, random = SITE_ID, data = df6_10, Hess = T)
m_1_random7   <- clmm2(GROUP.C ~ Mean_2002, random = SITE_ID, data = df6_10, Hess = T)
m_1_random8   <- clmm2(GROUP.C ~ Mean_2003, random = SITE_ID, data = df6_10, Hess = T)
m_1_random9   <- clmm2(GROUP.C ~ Mean_2007, random = SITE_ID, data = df6_10, Hess = T)
m_1_random10  <- clmm2(GROUP.C ~ Mean_2008, random = SITE_ID, data = df6_10, Hess = T)
m_1_random11  <- clmm2(GROUP.C ~ Mean_2009, random = SITE_ID, data = df6_10, Hess = T)
m_1_random12  <- clmm2(GROUP.C ~ Mean_2015, random = SITE_ID, data = df6_10, Hess = T)
m_1_random13  <- clmm2(GROUP.C ~ Mean_2017, random = SITE_ID, data = df6_10, Hess = T)
m_1_random14  <- clmm2(GROUP.C ~ Mean_2024, random = SITE_ID, data = df6_10, Hess = T)
m_1_random15  <- clmm2(GROUP.C ~ Mean_2028, random = SITE_ID, data = df6_10, Hess = T)
  
# Summary table: Odds Ratio with 95% CI, p values and stars for significance
results_m_1_random[1,]  <- as_tibble(cbind(sig_m_1.n[1],exp(coef(m_1_random1)[3]),
                                           exp(broom.helpers::tidy_parameters(m_1_random1)[3,5:6]),
                                           lmtest::coeftest(m_1_random1)[3,4],
                                           stars.pval(lmtest::coeftest(m_1_random1)[3,4])
                                           ))
results_m_1_random[2,]  <- as_tibble(cbind(sig_m_1.n[2],exp(coef(m_1_random2)[3]),
                                           exp(broom.helpers::tidy_parameters(m_1_random2)[3,5:6]),
                                           lmtest::coeftest(m_1_random2)[3,4],
                                           stars.pval(lmtest::coeftest(m_1_random2)[3,4])
                                           ))
results_m_1_random[3,]  <- as_tibble(cbind(sig_m_1.n[3],exp(coef(m_1_random3)[3]),
                                           exp(broom.helpers::tidy_parameters(m_1_random3)[3,5:6]),
                                           lmtest::coeftest(m_1_random3)[3,4],
                                           stars.pval(lmtest::coeftest(m_1_random3)[3,4])
                                           ))
results_m_1_random[4,]  <- as_tibble(cbind(sig_m_1.n[4],exp(coef(m_1_random4)[3]),
                                           exp(broom.helpers::tidy_parameters(m_1_random4)[3,5:6]),
                                           lmtest::coeftest(m_1_random4)[3,4],
                                           stars.pval(lmtest::coeftest(m_1_random4)[3,4])
                                           ))
results_m_1_random[5,]  <- as_tibble(cbind(sig_m_1.n[5],exp(coef(m_1_random5)[3]),
                                           exp(broom.helpers::tidy_parameters(m_1_random5)[3,5:6]),
                                           lmtest::coeftest(m_1_random5)[3,4],
                                           stars.pval(lmtest::coeftest(m_1_random5)[3,4])
                                           ))
results_m_1_random[6,]  <- as_tibble(cbind(sig_m_1.n[6],exp(coef(m_1_random6)[3]),
                                           exp(broom.helpers::tidy_parameters(m_1_random6)[3,5:6]),
                                           lmtest::coeftest(m_1_random6)[3,4],
                                           stars.pval(lmtest::coeftest(m_1_random6)[3,4])
                                           ))
results_m_1_random[7,]  <- as_tibble(cbind(sig_m_1.n[7],exp(coef(m_1_random7)[3]),
                                           exp(broom.helpers::tidy_parameters(m_1_random7)[3,5:6]),
                                           lmtest::coeftest(m_1_random7)[3,4],
                                           stars.pval(lmtest::coeftest(m_1_random7)[3,4])
                                           ))
results_m_1_random[8,]  <- as_tibble(cbind(sig_m_1.n[8],exp(coef(m_1_random8)[3]),
                                           exp(broom.helpers::tidy_parameters(m_1_random8)[3,5:6]),
                                           lmtest::coeftest(m_1_random8)[3,4],
                                           stars.pval(lmtest::coeftest(m_1_random8)[3,4])
                                           ))
results_m_1_random[9,]  <- as_tibble(cbind(sig_m_1.n[9],exp(coef(m_1_random9)[3]),
                                           exp(broom.helpers::tidy_parameters(m_1_random9)[3,5:6]),
                                           lmtest::coeftest(m_1_random9)[3,4],
                                           stars.pval(lmtest::coeftest(m_1_random9)[3,4])
                                           ))
results_m_1_random[10,]  <- as_tibble(cbind(sig_m_1.n[10],exp(coef(m_1_random10)[3]),
                                            exp(broom.helpers::tidy_parameters(m_1_random10)[3,5:6]),
                                            lmtest::coeftest(m_1_random10)[3,4],
                                            stars.pval(lmtest::coeftest(m_1_random10)[3,4])
                                            ))
results_m_1_random[11,]  <- as_tibble(cbind(sig_m_1.n[11],exp(coef(m_1_random11)[3]),
                                            exp(broom.helpers::tidy_parameters(m_1_random11)[3,5:6]),
                                            lmtest::coeftest(m_1_random11)[3,4],
                                            stars.pval(lmtest::coeftest(m_1_random11)[3,4])
                                            ))
results_m_1_random[12,]  <- as_tibble(cbind(sig_m_1.n[12],exp(coef(m_1_random12)[3]),
                                            exp(broom.helpers::tidy_parameters(m_1_random12)[3,5:6]),
                                            lmtest::coeftest(m_1_random12)[3,4],
                                            stars.pval(lmtest::coeftest(m_1_random12)[3,4])
                                            ))
results_m_1_random[13,]  <- as_tibble(cbind(sig_m_1.n[13],exp(coef(m_1_random13)[3]),
                                            exp(broom.helpers::tidy_parameters(m_1_random13)[3,5:6]),
                                            lmtest::coeftest(m_1_random13)[3,4],
                                            stars.pval(lmtest::coeftest(m_1_random13)[3,4])
                                            ))
results_m_1_random[14,]  <- as_tibble(cbind(sig_m_1.n[14],exp(coef(m_1_random14)[3]),
                                            exp(broom.helpers::tidy_parameters(m_1_random14)[3,5:6]),
                                            lmtest::coeftest(m_1_random14)[3,4],
                                            stars.pval(lmtest::coeftest(m_1_random14)[3,4])
                                            ))
results_m_1_random[15,]  <- as_tibble(cbind(sig_m_1.n[15],exp(coef(m_1_random15)[3]),
                                            exp(broom.helpers::tidy_parameters(m_1_random15)[3,5:6]),
                                            lmtest::coeftest(m_1_random15)[3,4],
                                            stars.pval(lmtest::coeftest(m_1_random15)[3,4])
                                            ))

gt::gtsave(gt::gt(results_m_1_random) %>% gt::fmt_number(decimals = 4) %>% 
             gt::tab_options(table.font.names = "Times New Roman") %>%
             tab_options(column_labels.font.weight = "bold"), filename = "6_10ModelG.png")

df6_10.m <- df6_10 %>%
  select(GROUP.C, sig_m_1[c(1,7,15)])
df6_10.m <- melt(df6_10.m, id.vars = 'GROUP.C') #redefinir pra pegar só colunas que foram sig
levels(df6_10.m$variable) <- sig_m_1.n[c(1,7,15)]
sizes1a <- ggplot(df6_10.m, aes(1, y = value, fill = GROUP.C)) +
  geom_rain(alpha = .5, cov = "GROUP.C",
            point.args.pos = list(position = position_dodge2(.1)),
            boxplot.args = list(width = .1, outlier.shape = NA),
            boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1)),
            violin.args = list(width = -.2, alpha = .5),
            violin.args.pos = list(side = "l", position = position_nudge(.17))
  )+
  theme(aspect.ratio = 1, legend.position = "none") +
  facet_wrap( ~ variable) +
  ylab("Thickness (mm)")+ xlab("Group")
sizes1a
ggsave("roimeans6_10a.png")

df6_10.m <- df6_10 %>%
  select(GROUP.C, sig_m_1[c(4,5,8,13,14)])
df6_10.m <- melt(df6_10.m, id.vars = 'GROUP.C') #redefinir pra pegar só colunas que foram sig
levels(df6_10.m$variable) <- sig_m_1.n[c(4,5,8,13,14)]
sizes1b <- ggplot(df6_10.m, aes(1, y = value, fill = GROUP.C)) +
  geom_rain(alpha = .5, cov = "GROUP.C",
            point.args.pos = list(position = position_dodge2(.1)),
            boxplot.args = list(width = .1, outlier.shape = NA),
            boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1)),
            violin.args = list(width = -.2, alpha = .5),
            violin.args.pos = list(side = "l", position = position_nudge(.17))
  )+
  theme(aspect.ratio = 1, legend.position = "none") +
  facet_wrap( ~ variable) +
  ylab("Thickness (mm)")+ xlab("Group")
sizes1b
ggsave("roimeans6_10b.png")

df6_10.m <- df6_10 %>%
  select(GROUP.C, sig_m_1[c(10,11)])
df6_10.m <- melt(df6_10.m, id.vars = 'GROUP.C') #redefinir pra pegar só colunas que foram sig
levels(df6_10.m$variable) <- sig_m_1.n[c(10,11)]
sizes1c <- ggplot(df6_10.m, aes(1, y = value, fill = GROUP.C)) +
  geom_rain(alpha = .5, cov = "GROUP.C",
            point.args.pos = list(position = position_dodge2(.1)),
            boxplot.args = list(width = .1, outlier.shape = NA),
            boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1)),
            violin.args = list(width = -.2, alpha = .5),
            violin.args.pos = list(side = "l", position = position_nudge(.17))
  )+
  theme(aspect.ratio = 1, legend.position = "none") +
  facet_wrap( ~ variable) +
  ylab("Thickness (mm)")+ xlab("Group")
sizes1c
ggsave("roimeans6_10c.png")

df6_10.m <- df6_10 %>%
  select(GROUP.C, sig_m_1[c(1,4,5,7,8,10,11,13,14,15)])
df6_10.m <- melt(df6_10.m, id.vars = 'GROUP.C') #redefinir pra pegar só colunas que foram sig
levels(df6_10.m$variable) <- sig_m_1.n[c(1,4,5,7,8,10,11,13,14,15)]
sizes1 <- ggplot(df6_10.m, aes(1, y = value, fill = GROUP.C)) +
  geom_rain(alpha = .5, cov = "GROUP.C",
            point.args.pos = list(position = position_dodge2(.1)),
            boxplot.args = list(width = .1, outlier.shape = NA),
            boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1)),
            violin.args = list(width = -.2, alpha = .5),
            violin.args.pos = list(side = "l", position = position_nudge(.17))
  )+
  theme(aspect.ratio = 1, legend.position = "none") +
  facet_wrap( ~ variable) +
  ylab("Thickness (mm)")+ xlab("Group")
sizes1
ggsave("roimeans6_10.png")


############ AGE RANGE 2 -> 11 to 14 yo ############
# Defines age range
df11_14 <- subset(df, AGE>=11 & AGE<15)
df11_14$SITE_ID <- as.factor(df11_14$SITE_ID)
var_label(df11_14[,33:94]) <- roi_names
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

###### ORDINAL MODELS
# Defining blank string to register significant results
sig_m_2     <- rep(NA,62)
# Defining blank list for model results
results_m_X2 <- as.list(rep(NA,62))
results_m_Y2 <- as.list(rep(NA,62))
results_m_Z2 <- as.list(rep(NA,62))
# Defining blank list for the proportional odds test
pr_odds2    <- array(NA,c(62,3))
deviance2   <- matrix(NA, 62, 3)

## MODEL F: Ordinal model for each ROI with fixed effects
# Proportional odds test
# Summary table: Odds Ratio with 95% CI, p values and stars for significance
# Registers which models were significant to run Model G
for (i in 1:62){
  try(m_2.X <- MASS::polr(GROUP.A ~ df11_14[,i+32] + SITE_ID, data=df11_14, Hess = T))
  try(m_2.Y <- MASS::polr(GROUP.B ~ df11_14[,i+32] + SITE_ID, data=df11_14, Hess = T))
  try(m_2.Z <- MASS::polr(GROUP.C ~ df11_14[,i+32] + SITE_ID, data=df11_14, Hess = T))
  # try(pr_odds2[i,1]  <- brant(m_2.X)[1,3])
  # try(pr_odds2[i,2]  <- brant(m_2.Y)[1,3])
  # try(pr_odds2[i,3]  <- brant(m_2.Z)[1,3])
  # deviance2[i,]      <- c(summary(m_2.X)$deviance, summary(m_2.Y)$deviance, summary(m_2.Z)$deviance)
  # results_m_X2[[i]]  <- gtsummary::tbl_regression(m_2.X, exponentiate = T
  #                                                 , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                                 , include = !SITE_ID
  #                                                 ) %>% gtsummary::add_global_p() %>%
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  # results_m_Y2[[i]]  <- gtsummary::tbl_regression(m_2.Y, exponentiate = T
  #                                                 , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                                 , include = !SITE_ID
  #                                                 ) %>% gtsummary::add_global_p() %>%
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  # results_m_Z2[[i]]  <- gtsummary::tbl_regression(m_2.Z, exponentiate = T
  #                                                 , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                                 , include = !SITE_ID
  #                                                 ) %>% gtsummary::add_global_p() %>%
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  if(lmtest::coeftest(m_2.X)[1,4] < 0.05 | lmtest::coeftest(m_2.Y)[1,4] < 0.05 | lmtest::coeftest(m_2.Z)[1,4] < 0.05) {sig_m_2[i] <- colnames(df)[i+32]}
}
gt::gtsave(gtsummary::tbl_merge(list(tbl_stack(results_m_X2[1:31]), tbl_stack(results_m_X2[32:62])),
                                      tab_spanner = c("Left", "Right")) %>% as_gt() %>%
                   gt::tab_options(table.font.names = "Times New Roman"), filename = "11_14ModelF_X.png")
gt::gtsave(gtsummary::tbl_merge(list(tbl_stack(results_m_Y2[1:31]), tbl_stack(results_m_Y2[32:62])),
                                      tab_spanner = c("Left", "Right")) %>% as_gt() %>%
                   gt::tab_options(table.font.names = "Times New Roman"), filename = "11_14ModelF_Y.png")
gt::gtsave(gtsummary::tbl_merge(list(tbl_stack(results_m_Z2[1:31]), tbl_stack(results_m_Z1[32:62])),
                                      tab_spanner = c("Left", "Right")) %>% as_gt() %>%
                   gt::tab_options(table.font.names = "Times New Roman"), filename = "11_14ModelF_Z.png")
system("paplay /usr/share/sounds/freedesktop/stereo/complete.oga")
pr_odds2
deviance2
write.table(deviance2, "11_14Deviance.csv", sep = ",", row.names = F, quote = F, col.names = c("X", "Y", "Z"))
write.table(pr_odds2, "11_14POdds.csv", sep = ",", row.names = F, quote = F, col.names = c("X", "Y", "Z"))
sig_m_2 <- sig_m_2[!is.na(sig_m_2)]
sig_m_2



## MODEL G: Ordinal model with random effects for SITE_ID
# Defining blank string to register significant results
sig_m_2_random      <- rep(NA,length(sig_m_2))
# Defining blank list for model results
sig_m_2
sig_m_2.n <- c(paste("Left",var_label(df11_14$Mean_1002)),paste("Left",var_label(df11_14$Mean_1018)),paste("Left",var_label(df11_14$Mean_1022)),paste("Left",var_label(df11_14$Mean_1024)),
               paste("Left",var_label(df11_14$Mean_1029)),paste("Left",var_label(df11_14$Mean_1035)),paste("Right",var_label(df11_14$Mean_2002)),paste("Right",var_label(df11_14$Mean_2016)),
               paste("Right",var_label(df11_14$Mean_2017)),paste("Right",var_label(df11_14$Mean_2022)),paste("Right",var_label(df11_14$Mean_2024)),paste("Right",var_label(df11_14$Mean_2026)),
               paste("Right",var_label(df11_14$Mean_2027)))
results_m_2_random  <- tibble(ROI = rep(NA, length(sig_m_2)), OR = rep(NA, length(sig_m_2)),
                              "Low CI" = rep(NA, length(sig_m_2)), "High CI" = rep(NA, length(sig_m_2)),
                              "p-value" = rep(NA, length(sig_m_2)), " " = rep(NA, length(sig_m_2)))
results_m_2_random


# Individual models. If it doesn't converge with default link functions, replace with the following:
# link = c("logistic", "probit", "cloglog", "loglog", "cauchit", "Aranda-Ordaz", "log-gamma")
m_2_random1   <- clmm2(GROUP.C ~ Mean_1002, random = SITE_ID, data = df11_14, Hess = T)
m_2_random2   <- clmm2(GROUP.C ~ Mean_1018, random = SITE_ID, data = df11_14, Hess = T)
m_2_random3   <- clmm2(GROUP.C ~ Mean_1022, random = SITE_ID, data = df11_14, Hess = T)
m_2_random4   <- clmm2(GROUP.C ~ Mean_1024, random = SITE_ID, data = df11_14, Hess = T)
m_2_random5   <- clmm2(GROUP.C ~ Mean_1029, random = SITE_ID, data = df11_14, Hess = T)
m_2_random6   <- clmm2(GROUP.C ~ Mean_1035, random = SITE_ID, data = df11_14, Hess = T)
m_2_random7   <- clmm2(GROUP.C ~ Mean_2002, random = SITE_ID, data = df11_14, Hess = T)
m_2_random8   <- clmm2(GROUP.C ~ Mean_2016, random = SITE_ID, data = df11_14, Hess = T)
m_2_random9   <- clmm2(GROUP.C ~ Mean_2017, random = SITE_ID, data = df11_14, Hess = T)
m_2_random10  <- clmm2(GROUP.C ~ Mean_2022, random = SITE_ID, data = df11_14, Hess = T)
m_2_random11  <- clmm2(GROUP.C ~ Mean_2024, random = SITE_ID, data = df11_14, Hess = T)
m_2_random12  <- clmm2(GROUP.C ~ Mean_2026, random = SITE_ID, data = df11_14, Hess = T)
m_2_random13  <- clmm2(GROUP.C ~ Mean_2027, random = SITE_ID, data = df11_14, Hess = T)

# Summary table: Odds Ratio with 95% CI, p values and stars for significance
results_m_2_random[1,]  <- as_tibble(cbind(sig_m_2.n[1],exp(coef(m_2_random1)[3]),
                                           exp(broom.helpers::tidy_parameters(m_2_random1)[3,5:6]),
                                           lmtest::coeftest(m_2_random1)[3,4],
                                           stars.pval(lmtest::coeftest(m_2_random1)[3,4])
))
results_m_2_random[2,]  <- as_tibble(cbind(sig_m_2.n[2],exp(coef(m_2_random2)[3]),
                                           exp(broom.helpers::tidy_parameters(m_2_random2)[3,5:6]),
                                           lmtest::coeftest(m_2_random2)[3,4],
                                           stars.pval(lmtest::coeftest(m_2_random2)[3,4])
))
results_m_2_random[3,]  <- as_tibble(cbind(sig_m_2.n[3],exp(coef(m_2_random3)[3]),
                                           exp(broom.helpers::tidy_parameters(m_2_random3)[3,5:6]),
                                           lmtest::coeftest(m_2_random3)[3,4],
                                           stars.pval(lmtest::coeftest(m_2_random3)[3,4])
))
results_m_2_random[4,]  <- as_tibble(cbind(sig_m_2.n[4],exp(coef(m_2_random4)[3]),
                                           exp(broom.helpers::tidy_parameters(m_2_random4)[3,5:6]),
                                           lmtest::coeftest(m_2_random4)[3,4],
                                           stars.pval(lmtest::coeftest(m_2_random4)[3,4])
))
results_m_2_random[5,]  <- as_tibble(cbind(sig_m_2.n[5],exp(coef(m_2_random5)[3]),
                                           exp(broom.helpers::tidy_parameters(m_2_random5)[3,5:6]),
                                           lmtest::coeftest(m_2_random5)[3,4],
                                           stars.pval(lmtest::coeftest(m_2_random5)[3,4])
))
results_m_2_random[6,]  <- as_tibble(cbind(sig_m_2.n[6],exp(coef(m_2_random6)[3]),
                                           exp(broom.helpers::tidy_parameters(m_2_random6)[3,5:6]),
                                           lmtest::coeftest(m_2_random6)[3,4],
                                           stars.pval(lmtest::coeftest(m_2_random6)[3,4])
))
results_m_2_random[7,]  <- as_tibble(cbind(sig_m_2.n[7],exp(coef(m_2_random7)[3]),
                                           exp(broom.helpers::tidy_parameters(m_2_random7)[3,5:6]),
                                           lmtest::coeftest(m_2_random7)[3,4],
                                           stars.pval(lmtest::coeftest(m_2_random7)[3,4])
))
results_m_2_random[8,]  <- as_tibble(cbind(sig_m_2.n[8],exp(coef(m_2_random8)[3]),
                                           exp(broom.helpers::tidy_parameters(m_2_random8)[3,5:6]),
                                           lmtest::coeftest(m_2_random8)[3,4],
                                           stars.pval(lmtest::coeftest(m_2_random8)[3,4])
))
results_m_2_random[9,]  <- as_tibble(cbind(sig_m_2.n[9],exp(coef(m_2_random9)[3]),
                                           exp(broom.helpers::tidy_parameters(m_2_random9)[3,5:6]),
                                           lmtest::coeftest(m_2_random9)[3,4],
                                           stars.pval(lmtest::coeftest(m_2_random9)[3,4])
))
results_m_2_random[10,]  <- as_tibble(cbind(sig_m_2.n[10],exp(coef(m_2_random10)[3]),
                                            exp(broom.helpers::tidy_parameters(m_2_random10)[3,5:6]),
                                            lmtest::coeftest(m_2_random10)[3,4],
                                            stars.pval(lmtest::coeftest(m_2_random10)[3,4])
))
results_m_2_random[11,]  <- as_tibble(cbind(sig_m_2.n[11],exp(coef(m_2_random11)[3]),
                                            exp(broom.helpers::tidy_parameters(m_2_random11)[3,5:6]),
                                            lmtest::coeftest(m_2_random11)[3,4],
                                            stars.pval(lmtest::coeftest(m_2_random11)[3,4])
))
results_m_2_random[12,]  <- as_tibble(cbind(sig_m_2.n[12],exp(coef(m_2_random12)[3]),
                                            exp(broom.helpers::tidy_parameters(m_2_random12)[3,5:6]),
                                            lmtest::coeftest(m_2_random12)[3,4],
                                            stars.pval(lmtest::coeftest(m_2_random12)[3,4])
))
results_m_2_random[13,]  <- as_tibble(cbind(sig_m_2.n[13],exp(coef(m_2_random13)[3]),
                                            exp(broom.helpers::tidy_parameters(m_2_random13)[3,5:6]),
                                            lmtest::coeftest(m_2_random13)[3,4],
                                            stars.pval(lmtest::coeftest(m_2_random13)[3,4])
))

gt::gtsave(gt::gt(results_m_2_random) %>%  gt::fmt_number(decimals = 4) %>%
             gt::tab_options(table.font.names = "Times New Roman") %>%
             tab_options(column_labels.font.weight = "bold"), filename = "11_14ModelG.png")

df11_14.m <- df11_14 %>%
  select(GROUP.C, all_of(sig_m_2[c(2,4,8,11)]))
df11_14.m <- melt(df11_14.m, id.vars = 'GROUP.C') #redefinir pra pegar só colunas que foram sig
levels(df11_14.m$variable) <- sig_m_2.n[c(2,4,8,11)]
sizes2 <- ggplot(df11_14.m, aes(1, y = value, fill = GROUP.C)) +
  geom_rain(alpha = .5, cov = "GROUP.C",
            point.args.pos = list(position = position_dodge2(.1)),
            boxplot.args = list(width = .1, outlier.shape = NA),
            boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1)),
            violin.args = list(width = -.2, alpha = .5),
            violin.args.pos = list(side = "l", position = position_nudge(.17))
  )+
  theme(aspect.ratio = 1) +
  facet_wrap( ~ variable) +
  ylab("Thickness (mm)")+ xlab("Group")
sizes2
ggsave("roimeans11_14.png")

############ AGE RANGE 3 -> 15 to 17 yo ############
# Defines age range
df15_17 <- subset(df, AGE>=15 & AGE<18)
df15_17$SITE_ID <- as.factor(df15_17$SITE_ID)
var_label(df15_17[,33:94]) <- roi_names
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

###### ORDINAL MODELS
# Defining blank string to register significant results
sig_m_3     <- rep(NA,62)
# Defining blank list for model results
results_m_X3 <- as.list(rep(NA,62))
results_m_Y3 <- as.list(rep(NA,62))
results_m_Z3 <- as.list(rep(NA,62))
# Defining blank list for the proportional odds test
pr_odds3    <- array(NA,c(62,3))
deviance3   <- matrix(NA, 62, 3)

## MODEL F: Ordinal model for each ROI with fixed effects
# Proportional odds test
# Summary table: Odds Ratio with 95% CI, p values and stars for significance
# Registers which models were significant to run Model G
for (i in 1:62){
  try(m_3.X <- MASS::polr(GROUP.A ~ df15_17[,i+32] + SITE_ID, data=df15_17, Hess = T))
  try(m_3.Y <- MASS::polr(GROUP.B ~ df15_17[,i+32] + SITE_ID, data=df15_17, Hess = T))
  try(m_3.Z <- MASS::polr(GROUP.C ~ df15_17[,i+32] + SITE_ID, data=df15_17, Hess = T))
  # try(pr_odds3[i,1]  <- brant(m_3.X)[1,3])
  # try(pr_odds3[i,2]  <- brant(m_3.Y)[1,3])
  # try(pr_odds3[i,3]  <- brant(m_3.Z)[1,3])
  # deviance3[i,]      <- c(summary(m_3.X)$deviance, summary(m_3.Y)$deviance, summary(m_3.Z)$deviance)
  # results_m_X3[[i]]  <- gtsummary::tbl_regression(m_3.X, exponentiate = T
  #                                                 , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                                 , include = !SITE_ID
  # ) %>% gtsummary::add_global_p() %>%
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  # results_m_Y3[[i]]  <- gtsummary::tbl_regression(m_3.Y, exponentiate = T
  #                                                 , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                                 , include = !SITE_ID
  # ) %>% gtsummary::add_global_p() %>%
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  # results_m_Z3[[i]]  <- gtsummary::tbl_regression(m_3.Z, exponentiate = T
  #                                                 , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                                 , include = !SITE_ID
  # ) %>% gtsummary::add_global_p() %>%
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  if(lmtest::coeftest(m_3.X)[1,4] < 0.05 | lmtest::coeftest(m_3.Y)[1,4] < 0.05 | lmtest::coeftest(m_3.Z)[1,4] < 0.05) {sig_m_3[i] <- colnames(df)[i+32]}
}
system("paplay /usr/share/sounds/freedesktop/stereo/complete.oga")
gt::gtsave(gtsummary::tbl_merge(list(tbl_stack(results_m_X3[1:31]), tbl_stack(results_m_X3[32:62])),
                                      tab_spanner = c("Left", "Right")) %>% as_gt() %>%
             gt::tab_options(table.font.names = "Times New Roman"), filename = "15_17ModelF_X.png")
gt::gtsave(gtsummary::tbl_merge(list(tbl_stack(results_m_Y3[1:31]), tbl_stack(results_m_Y3[32:62])),
                                      tab_spanner = c("Left", "Right")) %>% as_gt() %>%
                   gt::tab_options(table.font.names = "Times New Roman"), filename = "15_17ModelF_Y.png")
gt::gtsave(gtsummary::tbl_merge(list(tbl_stack(results_m_Z3[1:31]), tbl_stack(results_m_Z1[32:62])),
                                      tab_spanner = c("Left", "Right")) %>% as_gt() %>%
             gt::tab_options(table.font.names = "Times New Roman"), filename = "15_17ModelF_Z.png")
pr_odds3
deviance3
write.table(deviance3, "15_17Deviance.csv", sep = ",", row.names = F, quote = F, col.names = c("X", "Y", "Z"))
write.table(pr_odds3, "15_17POdds.csv", sep = ",", row.names = F, quote = F, col.names = c("X", "Y", "Z"))
sig_m_3 <- sig_m_3[!is.na(sig_m_3)]
sig_m_3



## MODEL G: Ordinal model with random effects for SITE_ID
# Defining blank string to register significant results
sig_m_3_random      <- rep(NA,length(sig_m_3))
# Defining blank list for model results
sig_m_3
sig_m_3.n <- c(paste("L",var_label(df15_17$Mean_1017)),paste("L",var_label(df15_17$Mean_1022)),paste("L",var_label(df15_17$Mean_1024)),paste("L",var_label(df15_17$Mean_1029)),
               paste("L",var_label(df15_17$Mean_1031)),paste("R",var_label(df15_17$Mean_2002)),paste("R",var_label(df15_17$Mean_2003)),paste("R",var_label(df15_17$Mean_2005)),
               paste("R",var_label(df15_17$Mean_2011)),paste("R",var_label(df15_17$Mean_2013)),paste("R",var_label(df15_17$Mean_2017)),paste("R",var_label(df15_17$Mean_2021)),
               paste("R",var_label(df15_17$Mean_2022)),paste("R",var_label(df15_17$Mean_2023)),paste("R",var_label(df15_17$Mean_2024)),paste("R",var_label(df15_17$Mean_2029)),
               paste("R",var_label(df15_17$Mean_2031)))
results_m_3_random  <- tibble(ROI = rep(NA, length(sig_m_3)), OR = rep(NA, length(sig_m_3)),
                              "Low CI" = rep(NA, length(sig_m_3)), "High CI" = rep(NA, length(sig_m_3)),
                              "p-value" = rep(NA, length(sig_m_3)), " " = rep(NA, length(sig_m_3)))
results_m_3_random


# Individual models. If it doesn't converge with default link functions, replace with the following:
# link = c("logistic", "probit", "cloglog", "loglog", "cauchit", "Aranda-Ordaz", "log-gamma")
m_3_random1   <- clmm2(GROUP.C ~ Mean_1017, random = SITE_ID, data = df15_17, Hess = T)
m_3_random2   <- clmm2(GROUP.C ~ Mean_1022, random = SITE_ID, data = df15_17, Hess = T)
m_3_random3   <- clmm2(GROUP.C ~ Mean_1024, random = SITE_ID, data = df15_17, Hess = T)
m_3_random4   <- clmm2(GROUP.C ~ Mean_1029, random = SITE_ID, data = df15_17, Hess = T)
m_3_random5   <- clmm2(GROUP.C ~ Mean_1031, random = SITE_ID, data = df15_17, Hess = T)
m_3_random6   <- clmm2(GROUP.C ~ Mean_2002, random = SITE_ID, data = df15_17, Hess = T)
m_3_random7   <- clmm2(GROUP.C ~ Mean_2003, random = SITE_ID, data = df15_17, Hess = T)
m_3_random8   <- clmm2(GROUP.C ~ Mean_2005, random = SITE_ID, data = df15_17, Hess = T)
m_3_random9   <- clmm2(GROUP.C ~ Mean_2011, random = SITE_ID, data = df15_17, Hess = T)
m_3_random10  <- clmm2(GROUP.C ~ Mean_2013, random = SITE_ID, data = df15_17, Hess = T)
m_3_random11  <- clmm2(GROUP.C ~ Mean_2017, random = SITE_ID, data = df15_17, Hess = T)
m_3_random12  <- clmm2(GROUP.C ~ Mean_2021, random = SITE_ID, data = df15_17, Hess = T)
m_3_random13  <- clmm2(GROUP.C ~ Mean_2022, random = SITE_ID, data = df15_17, Hess = T)
m_3_random14  <- clmm2(GROUP.C ~ Mean_2023, random = SITE_ID, data = df15_17, Hess = T)
m_3_random15  <- clmm2(GROUP.C ~ Mean_2024, random = SITE_ID, data = df15_17, Hess = T)
m_3_random16  <- clmm2(GROUP.C ~ Mean_2029, random = SITE_ID, data = df15_17, Hess = T)
m_3_random17  <- clmm2(GROUP.C ~ Mean_2031, random = SITE_ID, data = df15_17, Hess = T)

# Summary table: Odds Ratio with 95% CI, p values and stars for significance
results_m_3_random[1,]  <- as_tibble(cbind(sig_m_3.n[1],exp(coef(m_3_random1)[3]),
                                           exp(broom.helpers::tidy_parameters(m_3_random1)[3,5:6]),
                                           lmtest::coeftest(m_3_random1)[3,4],
                                           stars.pval(lmtest::coeftest(m_3_random1)[3,4])
))
results_m_3_random[2,]  <- as_tibble(cbind(sig_m_3.n[2],exp(coef(m_3_random2)[3]),
                                           exp(broom.helpers::tidy_parameters(m_3_random2)[3,5:6]),
                                           lmtest::coeftest(m_3_random2)[3,4],
                                           stars.pval(lmtest::coeftest(m_3_random2)[3,4])
))
results_m_3_random[3,]  <- as_tibble(cbind(sig_m_3.n[3],exp(coef(m_3_random3)[3]),
                                           exp(broom.helpers::tidy_parameters(m_3_random3)[3,5:6]),
                                           lmtest::coeftest(m_3_random3)[3,4],
                                           stars.pval(lmtest::coeftest(m_3_random3)[3,4])
))
results_m_3_random[4,]  <- as_tibble(cbind(sig_m_3.n[4],exp(coef(m_3_random4)[3]),
                                           exp(broom.helpers::tidy_parameters(m_3_random4)[3,5:6]),
                                           lmtest::coeftest(m_3_random4)[3,4],
                                           stars.pval(lmtest::coeftest(m_3_random4)[3,4])
))
results_m_3_random[5,]  <- as_tibble(cbind(sig_m_3.n[5],exp(coef(m_3_random5)[3]),
                                           exp(broom.helpers::tidy_parameters(m_3_random5)[3,5:6]),
                                           lmtest::coeftest(m_3_random5)[3,4],
                                           stars.pval(lmtest::coeftest(m_3_random5)[3,4])
))
results_m_3_random[6,]  <- as_tibble(cbind(sig_m_3.n[6],exp(coef(m_3_random6)[3]),
                                           exp(broom.helpers::tidy_parameters(m_3_random6)[3,5:6]),
                                           lmtest::coeftest(m_3_random6)[3,4],
                                           stars.pval(lmtest::coeftest(m_3_random6)[3,4])
))
results_m_3_random[7,]  <- as_tibble(cbind(sig_m_3.n[7],exp(coef(m_3_random7)[3]),
                                           exp(broom.helpers::tidy_parameters(m_3_random7)[3,5:6]),
                                           lmtest::coeftest(m_3_random7)[3,4],
                                           stars.pval(lmtest::coeftest(m_3_random7)[3,4])
))
results_m_3_random[8,]  <- as_tibble(cbind(sig_m_3.n[8],exp(coef(m_3_random8)[3]),
                                           exp(broom.helpers::tidy_parameters(m_3_random8)[3,5:6]),
                                           lmtest::coeftest(m_3_random8)[3,4],
                                           stars.pval(lmtest::coeftest(m_3_random8)[3,4])
))
results_m_3_random[9,]  <- as_tibble(cbind(sig_m_3.n[9],exp(coef(m_3_random9)[3]),
                                           exp(broom.helpers::tidy_parameters(m_3_random9)[3,5:6]),
                                           lmtest::coeftest(m_3_random9)[3,4],
                                           stars.pval(lmtest::coeftest(m_3_random9)[3,4])
))
results_m_3_random[10,]  <- as_tibble(cbind(sig_m_3.n[10],exp(coef(m_3_random10)[3]),
                                            exp(broom.helpers::tidy_parameters(m_3_random10)[3,5:6]),
                                            lmtest::coeftest(m_3_random10)[3,4],
                                            stars.pval(lmtest::coeftest(m_3_random10)[3,4])
))
results_m_3_random[11,]  <- as_tibble(cbind(sig_m_3.n[11],exp(coef(m_3_random11)[3]),
                                            exp(broom.helpers::tidy_parameters(m_3_random11)[3,5:6]),
                                            lmtest::coeftest(m_3_random11)[3,4],
                                            stars.pval(lmtest::coeftest(m_3_random11)[3,4])
))
results_m_3_random[12,]  <- as_tibble(cbind(sig_m_3.n[12],exp(coef(m_3_random12)[3]),
                                            exp(broom.helpers::tidy_parameters(m_3_random12)[3,5:6]),
                                            lmtest::coeftest(m_3_random12)[3,4],
                                            stars.pval(lmtest::coeftest(m_3_random12)[3,4])
))
results_m_3_random[13,]  <- as_tibble(cbind(sig_m_3.n[13],exp(coef(m_3_random13)[3]),
                                            exp(broom.helpers::tidy_parameters(m_3_random13)[3,5:6]),
                                            lmtest::coeftest(m_3_random13)[3,4],
                                            stars.pval(lmtest::coeftest(m_3_random13)[3,4])
))
results_m_3_random[14,]  <- as_tibble(cbind(sig_m_3.n[14],exp(coef(m_3_random14)[3]),
                                            exp(broom.helpers::tidy_parameters(m_3_random14)[3,5:6]),
                                            lmtest::coeftest(m_3_random14)[3,4],
                                            stars.pval(lmtest::coeftest(m_3_random14)[3,4])
))
results_m_3_random[15,]  <- as_tibble(cbind(sig_m_3.n[15],exp(coef(m_3_random15)[3]),
                                            exp(broom.helpers::tidy_parameters(m_3_random15)[3,5:6]),
                                            lmtest::coeftest(m_3_random15)[3,4],
                                            stars.pval(lmtest::coeftest(m_3_random15)[3,4])
))

results_m_3_random[16,]  <- as_tibble(cbind(sig_m_3.n[16],exp(coef(m_3_random16)[3]),
                                            exp(broom.helpers::tidy_parameters(m_3_random16)[3,5:6]),
                                            lmtest::coeftest(m_3_random16)[3,4],
                                            stars.pval(lmtest::coeftest(m_3_random16)[3,4])
))

results_m_3_random[17,]  <- as_tibble(cbind(sig_m_3.n[17],exp(coef(m_3_random17)[3]),
                                            exp(broom.helpers::tidy_parameters(m_3_random17)[3,5:6]),
                                            lmtest::coeftest(m_3_random17)[3,4],
                                            stars.pval(lmtest::coeftest(m_3_random17)[3,4])
))

gt::gtsave(gt::gt(results_m_3_random) %>% gt::fmt_number(decimals = 4) %>%
             gt::tab_options(table.font.names = "Times New Roman") %>%
             tab_options(column_labels.font.weight = "bold"), filename = "15_17ModelG.png")

df15_17.m <- df15_17 %>%
  select(GROUP.C, all_of(sig_m_3))
df15_17.m <- melt(df15_17.m, id.vars = 'GROUP.C') #redefinir pra pegar só colunas que foram sig
levels(df15_17.m$variable) <- sig_m_3.n
sizes <- ggplot(df15_17.m, aes(y = value)) +
  geom_boxplot(aes(x=GROUP.C, y = value))+
  scale_x_discrete(labels = c("Ctrl F", "Ctrl M", "ASD"))+
  facet_wrap( ~ variable)+
  ylab("Thickness (mm)")+ xlab("Group")
sizes
ggsave("roimeans15_17.png")



############ AGE RANGE 4 -> 18 to 24 yo ############
# Defines age range
df18_24 <- subset(df, AGE>17)
df18_24$SITE_ID <- as.factor(df18_24$SITE_ID)
var_label(df18_24[,33:94]) <- roi_names
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

###### ORDINAL MODELS
# Defining blank string to register significant results
sig_m_4     <- rep(NA,62)
# Defining blank list for model results
results_m_X4 <- as.list(rep(NA,62))
results_m_Y4 <- as.list(rep(NA,62))
results_m_Z4 <- as.list(rep(NA,62))
# Defining blank list for the proportional odds test
pr_odds4    <- array(NA,c(62,3))
deviance4   <- matrix(NA, 62, 3)

## MODEL F: Ordinal model for each ROI with fixed effects
# Proportional odds test
# Summary table: Odds Ratio with 95% CI, p values and stars for significance
# Registers which models were significant to run Model G
for (i in 1:62){
  try(m_4.X <- MASS::polr(GROUP.A ~ df18_24[,i+32] + SITE_ID, data=df18_24, Hess = T))
  try(m_4.Y <- MASS::polr(GROUP.B ~ df18_24[,i+32] + SITE_ID, data=df18_24, Hess = T))
  try(m_4.Z <- MASS::polr(GROUP.C ~ df18_24[,i+32] + SITE_ID, data=df18_24, Hess = T))
  # try(pr_odds4[i,1]  <- brant(m_4.X)[1,3])
  # try(pr_odds4[i,2]  <- brant(m_4.Y)[1,3])
  # try(pr_odds4[i,3]  <- brant(m_4.Z)[1,3])
  # deviance4[i,]      <- c(summary(m_4.X)$deviance, summary(m_4.Y)$deviance, summary(m_4.Z)$deviance)
  # results_m_X4[[i]]  <- gtsummary::tbl_regression(m_4.X, exponentiate = T
  #                                                 , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                                 , include = !SITE_ID
  # ) %>% gtsummary::add_global_p() %>%
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  # results_m_Y4[[i]]  <- gtsummary::tbl_regression(m_4.Y, exponentiate = T
  #                                                 , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                                 , include = !SITE_ID
  # ) %>% gtsummary::add_global_p() %>%
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  # results_m_Z4[[i]]  <- gtsummary::tbl_regression(m_4.Z, exponentiate = T
  #                                                 , estimate_fun = purrr::partial(style_ratio, digits = 3)
  #                                                 , include = !SITE_ID
  # ) %>% gtsummary::add_global_p() %>%
  #   gtsummary::add_significance_stars(hide_p = F, hide_se = T, hide_ci = F, pattern = "{p.value}{stars}")
  if(lmtest::coeftest(m_4.X)[1,4] < 0.05 | lmtest::coeftest(m_4.Y)[1,4] < 0.05 | lmtest::coeftest(m_4.Z)[1,4] < 0.05) {sig_m_4[i] <- colnames(df)[i+32]}
}
sig_m_4 <- sig_m_4[!is.na(sig_m_4)]
sig_m_4
gt::gtsave(gtsummary::tbl_merge(list(tbl_stack(results_m_X4[1:31]), tbl_stack(results_m_X4[32:62])),
                                      tab_spanner = c("Left", "Right")) %>% as_gt() %>%
             gt::tab_options(table.font.names = "Times New Roman"), filename = "18_24ModelF_X.png")
gt::gtsave(gtsummary::tbl_merge(list(tbl_stack(results_m_Y4[1:31]), tbl_stack(results_m_Y4[32:62])),
                                      tab_spanner = c("Left", "Right")) %>% as_gt() %>%
             gt::tab_options(table.font.names = "Times New Roman"), filename = "18_24ModelF_Y.png")
gt::gtsave(gtsummary::tbl_merge(list(tbl_stack(results_m_Z4[1:31]), tbl_stack(results_m_Z4[32:62])),
                                      tab_spanner = c("Left", "Right")) %>% as_gt() %>%
             gt::tab_options(table.font.names = "Times New Roman"), filename = "18_24ModelF_Z.png")
pr_odds4
deviance4
write.table(deviance4, "18_24Deviance.csv", sep = ",", row.names = F, quote = F, col.names = c("X", "Y", "Z"))
write.table(pr_odds4, "18_24POdds.csv", sep = ",", row.names = F, quote = F, col.names = c("X", "Y", "Z"))
system("paplay /usr/share/sounds/freedesktop/stereo/complete.oga")



## MODEL G: Ordinal model with random effects for SITE_ID
# Defining blank string to register significant results
sig_m_4_random      <- rep(NA,length(sig_m_4))
# Defining blank list for model results
sig_m_4
sig_m_4.n <- c(paste("L",var_label(df18_24$Mean_1009)),paste("L",var_label(df18_24$Mean_1010)),paste("L",var_label(df18_24$Mean_1015)),paste("L",var_label(df18_24$Mean_1020)),
               paste("L",var_label(df18_24$Mean_1023)),paste("L",var_label(df18_24$Mean_1026)),paste("L",var_label(df18_24$Mean_1027)),paste("L",var_label(df18_24$Mean_1028)),
               paste("L",var_label(df18_24$Mean_1030)),paste("L",var_label(df18_24$Mean_1031)),paste("L",var_label(df18_24$Mean_1034)),paste("L",var_label(df18_24$Mean_1035)),
               paste("R",var_label(df18_24$Mean_2003)),paste("R",var_label(df18_24$Mean_2006)),paste("R",var_label(df18_24$Mean_2009)),paste("R",var_label(df18_24$Mean_2010)),
               paste("R",var_label(df18_24$Mean_2015)),paste("R",var_label(df18_24$Mean_2020)),paste("R",var_label(df18_24$Mean_2022)),paste("R",var_label(df18_24$Mean_2023)),
               paste("R",var_label(df18_24$Mean_2024)),paste("R",var_label(df18_24$Mean_2025)),paste("R",var_label(df18_24$Mean_2027)),paste("R",var_label(df18_24$Mean_2028)),
               paste("R",var_label(df18_24$Mean_2030)),paste("R",var_label(df18_24$Mean_2034)))
results_m_4_random  <- tibble(ROI = rep(NA, length(sig_m_4)), OR = rep(NA, length(sig_m_4)),
                              "Low CI" = rep(NA, length(sig_m_4)), "High CI" = rep(NA, length(sig_m_4)),
                              "p-value" = rep(NA, length(sig_m_4)), " " = rep(NA, length(sig_m_4)))
results_m_4_random



# Individual models. If it doesn't converge with default link functions, replace with the following:
# link = c("logistic", "probit", "cloglog", "loglog", "cauchit", "Aranda-Ordaz", "log-gamma")
m_4_random1   <- clmm2(GROUP.C ~ Mean_1009, random = SITE_ID, data = df18_24, Hess = T)
m_4_random2   <- clmm2(GROUP.C ~ Mean_1010, random = SITE_ID, data = df18_24, Hess = T)
m_4_random3   <- clmm2(GROUP.C ~ Mean_1015, random = SITE_ID, data = df18_24, Hess = T)
m_4_random4   <- clmm2(GROUP.C ~ Mean_1020, random = SITE_ID, data = df18_24, Hess = T)
m_4_random5   <- clmm2(GROUP.C ~ Mean_1023, random = SITE_ID, data = df18_24, Hess = T, link = "probit")
m_4_random6   <- clmm2(GROUP.C ~ Mean_1026, random = SITE_ID, data = df18_24, Hess = T)
m_4_random7   <- clmm2(GROUP.C ~ Mean_1027, random = SITE_ID, data = df18_24, Hess = T)
m_4_random8   <- clmm2(GROUP.C ~ Mean_1028, random = SITE_ID, data = df18_24, Hess = T)
m_4_random9   <- clmm2(GROUP.C ~ Mean_1030, random = SITE_ID, data = df18_24, Hess = T, link = "probit")
m_4_random10  <- clmm2(GROUP.C ~ Mean_1031, random = SITE_ID, data = df18_24, Hess = T)
m_4_random11  <- clmm2(GROUP.C ~ Mean_1034, random = SITE_ID, data = df18_24, Hess = T)
m_4_random12  <- clmm2(GROUP.C ~ Mean_1035, random = SITE_ID, data = df18_24, Hess = T, link = "probit")
m_4_random13  <- clmm2(GROUP.C ~ Mean_2003, random = SITE_ID, data = df18_24, Hess = T)
m_4_random14  <- clmm2(GROUP.C ~ Mean_2006, random = SITE_ID, data = df18_24, Hess = T)
m_4_random15  <- clmm2(GROUP.C ~ Mean_2009, random = SITE_ID, data = df18_24, Hess = T)
m_4_random16  <- clmm2(GROUP.C ~ Mean_2010, random = SITE_ID, data = df18_24, Hess = T, link = "probit")
m_4_random17  <- clmm2(GROUP.C ~ Mean_2015, random = SITE_ID, data = df18_24, Hess = T)
m_4_random18  <- clmm2(GROUP.C ~ Mean_2020, random = SITE_ID, data = df18_24, Hess = T)
m_4_random19  <- clmm2(GROUP.C ~ Mean_2022, random = SITE_ID, data = df18_24, Hess = T)
m_4_random20  <- clmm2(GROUP.C ~ Mean_2023, random = SITE_ID, data = df18_24, Hess = T)
m_4_random21  <- clmm2(GROUP.C ~ Mean_2024, random = SITE_ID, data = df18_24, Hess = T)
m_4_random22  <- clmm2(GROUP.C ~ Mean_2025, random = SITE_ID, data = df18_24, Hess = T, link = "probit")
m_4_random23  <- clmm2(GROUP.C ~ Mean_2027, random = SITE_ID, data = df18_24, Hess = T)
m_4_random24  <- clmm2(GROUP.C ~ Mean_2028, random = SITE_ID, data = df18_24, Hess = T)
m_4_random25  <- clmm2(GROUP.C ~ Mean_2030, random = SITE_ID, data = df18_24, Hess = T, link = "probit")
m_4_random26  <- clmm2(GROUP.C ~ Mean_2034, random = SITE_ID, data = df18_24, Hess = T)

# Summary table: Odds Ratio with 95% CI, p values and stars for significance
results_m_4_random[1,]  <- as_tibble(cbind(sig_m_4.n[1],exp(coef(m_4_random1)[3]),
                                           exp(broom.helpers::tidy_parameters(m_4_random1)[3,5:6]),
                                           lmtest::coeftest(m_4_random1)[3,4],
                                           stars.pval(lmtest::coeftest(m_4_random1)[3,4])
))
results_m_4_random[2,]  <- as_tibble(cbind(sig_m_4.n[2],exp(coef(m_4_random2)[3]),
                                           exp(broom.helpers::tidy_parameters(m_4_random2)[3,5:6]),
                                           lmtest::coeftest(m_4_random2)[3,4],
                                           stars.pval(lmtest::coeftest(m_4_random2)[3,4])
))
results_m_4_random[3,]  <- as_tibble(cbind(sig_m_4.n[3],exp(coef(m_4_random3)[3]),
                                           exp(broom.helpers::tidy_parameters(m_4_random3)[3,5:6]),
                                           lmtest::coeftest(m_4_random3)[3,4],
                                           stars.pval(lmtest::coeftest(m_4_random3)[3,4])
))
results_m_4_random[4,]  <- as_tibble(cbind(sig_m_4.n[4],exp(coef(m_4_random4)[3]),
                                           exp(broom.helpers::tidy_parameters(m_4_random4)[3,5:6]),
                                           lmtest::coeftest(m_4_random4)[3,4],
                                           stars.pval(lmtest::coeftest(m_4_random4)[3,4])
))
results_m_4_random[5,]  <- as_tibble(cbind(sig_m_4.n[5],exp(coef(m_4_random5)[3]),
                                           exp(broom.helpers::tidy_parameters(m_4_random5)[3,5:6]),
                                           lmtest::coeftest(m_4_random5)[3,4],
                                           stars.pval(lmtest::coeftest(m_4_random5)[3,4])
))
results_m_4_random[6,]  <- as_tibble(cbind(sig_m_4.n[6],exp(coef(m_4_random6)[3]),
                                           exp(broom.helpers::tidy_parameters(m_4_random6)[3,5:6]),
                                           lmtest::coeftest(m_4_random6)[3,4],
                                           stars.pval(lmtest::coeftest(m_4_random6)[3,4])
))
results_m_4_random[7,]  <- as_tibble(cbind(sig_m_4.n[7],exp(coef(m_4_random7)[3]),
                                           exp(broom.helpers::tidy_parameters(m_4_random7)[3,5:6]),
                                           lmtest::coeftest(m_4_random7)[3,4],
                                           stars.pval(lmtest::coeftest(m_4_random7)[3,4])
))
results_m_4_random[8,]  <- as_tibble(cbind(sig_m_4.n[8],exp(coef(m_4_random8)[3]),
                                           exp(broom.helpers::tidy_parameters(m_4_random8)[3,5:6]),
                                           lmtest::coeftest(m_4_random8)[3,4],
                                           stars.pval(lmtest::coeftest(m_4_random8)[3,4])
))
results_m_4_random[9,]  <- as_tibble(cbind(sig_m_4.n[9],exp(coef(m_4_random9)[3]),
                                           exp(broom.helpers::tidy_parameters(m_4_random9)[3,5:6]),
                                           lmtest::coeftest(m_4_random9)[3,4],
                                           stars.pval(lmtest::coeftest(m_4_random9)[3,4])
))
results_m_4_random[10,]  <- as_tibble(cbind(sig_m_4.n[10],exp(coef(m_4_random10)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random10)[3,5:6]),
                                            lmtest::coeftest(m_4_random10)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random10)[3,4])
))
results_m_4_random[11,]  <- as_tibble(cbind(sig_m_4.n[11],exp(coef(m_4_random11)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random11)[3,5:6]),
                                            lmtest::coeftest(m_4_random11)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random11)[3,4])
))
results_m_4_random[12,]  <- as_tibble(cbind(sig_m_4.n[12],exp(coef(m_4_random12)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random12)[3,5:6]),
                                            lmtest::coeftest(m_4_random12)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random12)[3,4])
))
results_m_4_random[13,]  <- as_tibble(cbind(sig_m_4.n[13],exp(coef(m_4_random13)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random13)[3,5:6]),
                                            lmtest::coeftest(m_4_random13)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random13)[3,4])
))
results_m_4_random[14,]  <- as_tibble(cbind(sig_m_4.n[14],exp(coef(m_4_random14)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random14)[3,5:6]),
                                            lmtest::coeftest(m_4_random14)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random14)[3,4])
))
results_m_4_random[15,]  <- as_tibble(cbind(sig_m_4.n[15],exp(coef(m_4_random15)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random15)[3,5:6]),
                                            lmtest::coeftest(m_4_random15)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random15)[3,4])
))
results_m_4_random[16,]  <- as_tibble(cbind(sig_m_4.n[16],exp(coef(m_4_random16)[3]),
                                             exp(broom.helpers::tidy_parameters(m_4_random16)[3,5:6]),
                                             lmtest::coeftest(m_4_random16)[3,4],
                                             stars.pval(lmtest::coeftest(m_4_random16)[3,4])
))
results_m_4_random[17,]  <- as_tibble(cbind(sig_m_4.n[17],exp(coef(m_4_random17)[3]),
                                           exp(broom.helpers::tidy_parameters(m_4_random17)[3,5:6]),
                                           lmtest::coeftest(m_4_random17)[3,4],
                                           stars.pval(lmtest::coeftest(m_4_random17)[3,4])
))
results_m_4_random[18,]  <- as_tibble(cbind(sig_m_4.n[18],exp(coef(m_4_random18)[3]),
                                           exp(broom.helpers::tidy_parameters(m_4_random18)[3,5:6]),
                                           lmtest::coeftest(m_4_random18)[3,4],
                                           stars.pval(lmtest::coeftest(m_4_random18)[3,4])
))
results_m_4_random[19,]  <- as_tibble(cbind(sig_m_4.n[19],exp(coef(m_4_random19)[3]),
                                           exp(broom.helpers::tidy_parameters(m_4_random19)[3,5:6]),
                                           lmtest::coeftest(m_4_random19)[3,4],
                                           stars.pval(lmtest::coeftest(m_4_random19)[3,4])
))
results_m_4_random[20,]  <- as_tibble(cbind(sig_m_4.n[20],exp(coef(m_4_random20)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random20)[3,5:6]),
                                            lmtest::coeftest(m_4_random20)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random20)[3,4])
))
results_m_4_random[21,]  <- as_tibble(cbind(sig_m_4.n[21],exp(coef(m_4_random21)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random21)[3,5:6]),
                                            lmtest::coeftest(m_4_random21)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random21)[3,4])
))
results_m_4_random[22,]  <- as_tibble(cbind(sig_m_4.n[22],exp(coef(m_4_random22)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random22)[3,5:6]),
                                            lmtest::coeftest(m_4_random22)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random22)[3,4])
))
results_m_4_random[23,]  <- as_tibble(cbind(sig_m_4.n[23],exp(coef(m_4_random23)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random23)[3,5:6]),
                                            lmtest::coeftest(m_4_random23)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random23)[3,4])
))
results_m_4_random[24,]  <- as_tibble(cbind(sig_m_4.n[24],exp(coef(m_4_random24)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random24)[3,5:6]),
                                            lmtest::coeftest(m_4_random24)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random24)[3,4])
))
results_m_4_random[25,]  <- as_tibble(cbind(sig_m_4.n[25],exp(coef(m_4_random25)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random25)[3,5:6]),
                                            lmtest::coeftest(m_4_random25)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random25)[3,4])
))
results_m_4_random[26,]  <- as_tibble(cbind(sig_m_4.n[26],exp(coef(m_4_random26)[3]),
                                            exp(broom.helpers::tidy_parameters(m_4_random26)[3,5:6]),
                                            lmtest::coeftest(m_4_random26)[3,4],
                                            stars.pval(lmtest::coeftest(m_4_random26)[3,4])
))
gt::gtsave(gt::gt(results_m_4_random) %>% gt::fmt_number(decimals = 4) %>%
             gt::tab_options(table.font.names = "Times New Roman") %>%
             tab_options(column_labels.font.weight = "bold"), filename = "18_24ModelG.png")

df18_24.m <- df18_24 %>%
  select(GROUP.C, all_of(sig_m_4[c(1,2,3,5,16,17,20)]))
df18_24.m <- melt(df18_24.m, id.vars = 'GROUP.C') #redefinir pra pegar só colunas que foram sig
levels(df18_24.m$variable) <- sig_m_4.n[c(1,2,3,5,16,17,20)]
sizes4a <-  ggplot(df18_24.m, aes(1, y = value, fill = GROUP.C)) +
  geom_rain(alpha = .5, cov = "GROUP.C",
            point.args.pos = list(position = position_dodge2(.1)),
            boxplot.args = list(width = .1, outlier.shape = NA),
            boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1)),
            violin.args = list(width = -.2, alpha = .5),
            violin.args.pos = list(side = "l", position = position_nudge(.17))
  )+
  theme(aspect.ratio = 1) +
  facet_wrap( ~ variable, nr = 2) +
  ylab("Thickness (mm)")+ xlab("Group")
sizes4a
ggsave("roimeans18_24a.png")

df18_24.m <- df18_24 %>%
  select(GROUP.C, all_of(sig_m_4[c(6,12,14,15)]))
df18_24.m <- melt(df18_24.m, id.vars = 'GROUP.C') #redefinir pra pegar só colunas que foram sig
levels(df18_24.m$variable) <- sig_m_4.n[c(6,12,14,15)]
sizes4b <-  ggplot(df18_24.m, aes(1, y = value, fill = GROUP.C)) +
  geom_rain(alpha = .5, cov = "GROUP.C",
            point.args.pos = list(position = position_dodge2(.1)),
            boxplot.args = list(width = .1, outlier.shape = NA),
            boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1)),
            violin.args = list(width = -.2, alpha = .5),
            violin.args.pos = list(side = "l", position = position_nudge(.17))
  )+
  theme(aspect.ratio = 1) +
  facet_wrap( ~ variable) +
  ylab("Thickness (mm)")+ xlab("Group")
sizes4b
ggsave("roimeans18_24b.png")


df18_24.m <- df18_24 %>%
  select(GROUP.C, all_of(sig_m_4[c(8,9,10,11,22,24,25,26)]))
df18_24.m <- melt(df18_24.m, id.vars = 'GROUP.C') #redefinir pra pegar só colunas que foram sig
levels(df18_24.m$variable) <- sig_m_4.n[c(8,9,10,11,22,24,25,26)]
sizes4c <-  ggplot(df18_24.m, aes(1, y = value, fill = GROUP.C)) +
  geom_rain(alpha = .5, cov = "GROUP.C",
            point.args.pos = list(position = position_dodge2(.1)),
            boxplot.args = list(width = .1, outlier.shape = NA),
            boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1)),
            violin.args = list(width = -.2, alpha = .5),
            violin.args.pos = list(side = "l", position = position_nudge(.17))
  )+
  theme(aspect.ratio = 1) +
  facet_wrap( ~ variable, nr = 2) +
  ylab("Thickness (mm)")+ xlab("Group")
sizes4c
ggsave("roimeans18_24c.png")

df18_24.m <- df18_24 %>%
  select(GROUP.C, all_of(sig_m_4[c(19,21)]))
df18_24.m <- melt(df18_24.m, id.vars = 'GROUP.C') #redefinir pra pegar só colunas que foram sig
levels(df18_24.m$variable) <- sig_m_4.n[c(19,21)]
sizes4d <-  ggplot(df18_24.m, aes(1, y = value, fill = GROUP.C)) +
  geom_rain(alpha = .5, cov = "GROUP.C",
            point.args.pos = list(position = position_dodge2(.1)),
            boxplot.args = list(width = .1, outlier.shape = NA),
            boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1)),
            violin.args = list(width = -.2, alpha = .5),
            violin.args.pos = list(side = "l", position = position_nudge(.17))
  )+
  theme(aspect.ratio = 1) +
  facet_wrap( ~ variable, nr = 1) +
  ylab("Thickness (mm)")+ xlab("Group")
sizes4d
ggsave("roimeans18_24d.png")


df18_24.m <- df18_24 %>%
  select(GROUP.C, all_of(sig_m_4[-c(4,7,13,18,23)]))
df18_24.m <- melt(df18_24.m, id.vars = 'GROUP.C') #redefinir pra pegar só colunas que foram sig
levels(df18_24.m$variable) <- sig_m_4.n[-c(4,7,13,18,23)]
sizes4 <-  ggplot(df18_24.m, aes(1, y = value, fill = GROUP.C)) +
  geom_rain(alpha = .5, cov = "GROUP.C",
            point.args.pos = list(position = position_dodge2(.1)),
            boxplot.args = list(width = .1, outlier.shape = NA),
            boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1)),
            violin.args = list(width = -.2, alpha = .5),
            violin.args.pos = list(side = "l", position = position_nudge(.17))
  )+
  theme(aspect.ratio = 1) +
  facet_wrap( ~ variable) +
  ylab("Thickness (mm)")+ xlab("Group")
sizes4
ggsave("roimeans18_24.png")
