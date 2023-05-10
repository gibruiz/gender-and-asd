load("./data/df.RData")
# setwd("./results")
attach(df)
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, gtsummary, gt, webshot2, reshape2, ggplot2, gtools)

### DEMOGRAPHICS ###
# Summary tables for age, in general and divided by SEX, DIAGNOSTIC GROUP and SEXxDIAGNOSIS
# Includes median, mean, standard deviation (sd) and number of individuals
# Saves in a .png file

# Age in general
age_general <- df %>%
  select(AGE) %>%
  tbl_summary(statistic = list(AGE ~ c("{mean} ({sd})", "{median}")),
              type = list(AGE ~ 'continuous2'))
gt::gtsave(as_gt(age_general), filename = "./results/Age_general.png")

# Age by diagnostic
age_by_diag <- df %>%
  select(AGE, DX_GROUP) %>%
  tbl_summary(by=DX_GROUP,
              statistic = list(AGE ~ c("{mean} ({sd})", "{median}")),
              type = list(AGE ~ 'continuous2'))
gt::gtsave(as_gt(age_by_diag), filename = "./results/Age_by_diag.png")

# Age by sex
age_by_sex <- df %>%
  select(AGE, SEX) %>%
  tbl_summary(by=SEX,
              statistic = list(AGE ~ c("{mean} ({sd})", "{median}")),
              type = list(AGE ~ 'continuous2'))
gt::gtsave(as_gt(age_by_sex), filename = "./results/Age_by_sex.png")

# Age by groups (sex and diagnostic combined)
age_by_groups <- df %>%
  select(AGE, GROUP) %>%
  tbl_summary(by=GROUP,
              statistic = list(AGE ~ c("{mean} ({sd})", "{median}")),
              type = list(AGE ~ 'continuous2'))
gt::gtsave(as_gt(age_by_groups), filename = "./results/Age_by_groups.png")

## Histogram plot for AGE by GROUPS (SEX and DIAGNOSIS)
# Whole sample
histog <- ggplot(df, aes(x = AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df$SEX ~ df$DX_GROUP)
histog
ggsave("./results/histogram_age.png")

# 6 to 10 years old
df6_10 <- subset(df, AGE<11) #faixa etaria
attach(df6_10)
histog_6to10 <- ggplot(df6_10, aes(x = df6_10$AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df6_10$SEX ~ df6_10$DX_GROUP)
histog_6to10
ggsave("./results/histogram_age_6to10.png")

# 11 to 14 years old
df11_14 <- subset(df, AGE>=11 & AGE<15) #faixa etaria
attach(df11_14)
histog_11to14 <- ggplot(df11_14, aes(x = df11_14$AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df11_14$SEX ~ df11_14$DX_GROUP)
histog_11to14
ggsave("./results/histogram_age_11to14.png")

# 15 to 17 years old
df15_17 <- subset(df, AGE<11) #faixa etaria
attach(df15_17)
histog_15to17 <- ggplot(df15_17, aes(x = df15_17$AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df15_17$SEX ~ df15_17$DX_GROUP)
histog_15to17
ggsave("./results/histogram_age_15to17.png")

# 18 to 24 years old
df18_24 <- subset(df, AGE<11) #faixa etaria
attach(df18_24)
histog_18to24 <- ggplot(df18_24, aes(x = df18_24$AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df18_24$SEX ~ df18_24$DX_GROUP)
histog_18to24
ggsave("./results/histogram_age_18to24.png")



### DEMOGRAPHICS FOR AGE GROUPS
# Same tables and plots

# 6 TO 10 YEARS OLD
#Defining age range
df <- subset(df, AGE>=6 & AGE<11)
attach(df)

# Summary tables
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

write.table(format(as.data.frame(summ_geral),digits = 4), "6a10.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(summ_groups),digits = 4), "6a10grupos.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(summ_sex),digits = 4), "6a10sex.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(summ_dx),digits = 4), "6a10dx.csv", sep = ",", quote = F, row.names = F)



#11 TO 14 YEARS OLD
# Defining age range
load("./data/df.RData")
df <- subset(df, AGE>=11 & AGE<15)
attach(df)

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


#15 TO 17 YEARS OLD
# Defining age range
load("./data/df.RData")
df <- subset(df, AGE>=15 & AGE<18)
attach(df)

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

write.table(format(as.data.frame(summ_geral),digits = 4), "15a17.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(summ_groups),digits = 4), "15a17grupos.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(summ_sex),digits = 4), "15a17sex.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(summ_dx),digits = 4), "15a17dx.csv", sep = ",", quote = F, row.names = F)


#18 TO 24 YEARS OLD
# Defining age range
load("./data/df.RData")
df <- subset(df, AGE>=18)
attach(df)

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

write.table(format(as.data.frame(summ_geral),digits = 4), "18a24.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(summ_groups),digits = 4), "18a24grupos.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(summ_sex),digits = 4), "18a24sex.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(summ_dx),digits = 4), "18a24dx.csv", sep = ",", quote = F, row.names = F)
