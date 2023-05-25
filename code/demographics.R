load("./data/df.RData")
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
  select(AGE, GROUP.A) %>%
  tbl_summary(by=GROUP.A,
              statistic = list(AGE ~ c("{mean} ({sd})", "{median}")),
              type = list(AGE ~ 'continuous2'),
              label = AGE ~ "Age") %>%
  modify_header(all_stat_cols() ~  "**{level}**<br>N = {n}")
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

n_6_10 <- tbl_summary(df6_10,
                      statistic = list(GROUP.A ~ "{n}"),
                      include = GROUP.A,
                      label = list(GROUP.A ~ "Group"))%>%
  modify_footnote(all_stat_cols() ~ NA)
adir_6_10 <- df6_10 %>%
  select(ADI_R_SOCIAL_TOTAL_A, ADI_R_VERBAL_TOTAL_BV, ADI_RRB_TOTAL_C, SEX) %>%
  filter(DX_GROUP=="ASD") %>% 
  tbl_summary(by = SEX,
              statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median}", "{N_obs}")),
              type = list(all_continuous() ~ 'continuous2'),
              missing_text = "Missing",
              label = list(ADI_R_SOCIAL_TOTAL_A ~ "ADI-R Social",
                           ADI_R_VERBAL_TOTAL_BV ~ "ADI-R Verbal",
                           ADI_RRB_TOTAL_C ~ "ADI-R RRB")) %>%
  modify_header(all_stat_cols() ~  "**{level}**") %>% 
  bold_labels()
tvolume_6_10 <- tbl_summary(df6_10, by = GROUP.A,
                            statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median}")),
                            type = list(all_continuous() ~ 'continuous2'),
                            label = TOTAL_VOLUME ~ "6 to 10 years old",
                            include = TOTAL_VOLUME) %>% bold_labels() %>%
  modify_header(all_stat_cols() ~  "**{level}**<br>N = {n}")
tvolume_6_10


# 11 to 14 years old
df11_14 <- subset(df, AGE>=11 & AGE<15) #faixa etaria
attach(df11_14)
histog_11to14 <- ggplot(df11_14, aes(x = df11_14$AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df11_14$SEX ~ df11_14$DX_GROUP)
histog_11to14
ggsave("./results/histogram_age_11to14.png")

n_11_14 <- tbl_summary(df11_14,
                      statistic = list(GROUP.A ~ "{n}"),
                      include = GROUP.A,
                      label = list(GROUP.A ~ "Group"))%>%
  modify_footnote(all_stat_cols() ~ NA)
adir_11_14 <- df11_14 %>%
  select(ADI_R_SOCIAL_TOTAL_A, ADI_R_VERBAL_TOTAL_BV, ADI_RRB_TOTAL_C, SEX) %>%
  filter(DX_GROUP=="ASD") %>% 
  tbl_summary(by = SEX,
              statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median}", "{N_obs}")),
              type = list(all_continuous() ~ 'continuous2'),
              missing_text = "Missing",
              label = list(ADI_R_SOCIAL_TOTAL_A ~ "ADI-R Social",
                           ADI_R_VERBAL_TOTAL_BV ~ "ADI-R Verbal",
                           ADI_RRB_TOTAL_C ~ "ADI-R RRB")) %>%
  modify_header(all_stat_cols() ~  "**{level}**") %>% 
  bold_labels()

tvolume_11_14 <- tbl_summary(df11_14, by = GROUP.A,
                             statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median}")),
                             type = list(all_continuous() ~ 'continuous2'),
                             label = TOTAL_VOLUME ~ "11 to 14 years old",
                             include = TOTAL_VOLUME) %>% bold_labels() %>%
  modify_header(all_stat_cols() ~  "**{level}**<br>N = {n}")

# 15 to 17 years old
df15_17 <- subset(df, AGE>=15 & AGE<18) #faixa etaria
attach(df15_17)
histog_15to17 <- ggplot(df15_17, aes(x = df15_17$AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df15_17$SEX ~ df15_17$DX_GROUP)
histog_15to17
ggsave("./results/histogram_age_15to17.png")

n_15_17 <- tbl_summary(df15_17,
                      statistic = list(GROUP.A ~ "{n}"),
                      include = GROUP.A,
                      label = list(GROUP.A ~ "Group"))%>%
  modify_footnote(all_stat_cols() ~ NA)
adir_15_17 <- df15_17 %>%
  select(ADI_R_SOCIAL_TOTAL_A, ADI_R_VERBAL_TOTAL_BV, ADI_RRB_TOTAL_C, SEX) %>%
  filter(DX_GROUP=="ASD") %>% 
  tbl_summary(by = SEX,
              statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median}", "{N_obs}")),
              type = list(all_continuous() ~ 'continuous2'),
              missing_text = "Missing",
              label = list(ADI_R_SOCIAL_TOTAL_A ~ "ADI-R Social",
                           ADI_R_VERBAL_TOTAL_BV ~ "ADI-R Verbal",
                           ADI_RRB_TOTAL_C ~ "ADI-R RRB")) %>%
  modify_header(all_stat_cols() ~  "**{level}**") %>% 
  bold_labels()

tvolume_15_17 <- tbl_summary(df15_17, by = GROUP.A,
                             statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median}")),
                             type = list(all_continuous() ~ 'continuous2'),
                             label = TOTAL_VOLUME ~ "15 to 17 years old",
                             include = TOTAL_VOLUME) %>% bold_labels() %>%
  modify_header(all_stat_cols() ~  "**{level}**<br>N = {n}")

# 18 to 24 years old
df18_24 <- subset(df, AGE>=18)
attach(df18_24)
histog_18to24 <- ggplot(df18_24, aes(x = df18_24$AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df18_24$SEX ~ df18_24$DX_GROUP)
histog_18to24
ggsave("./results/histogram_age_18to24.png")

n_18_24 <- tbl_summary(df18_24,
                      statistic = list(GROUP.A ~ "{n}"),
                      include = GROUP.A,
                      label = list(GROUP.A ~ "Group"))%>%
  modify_footnote(all_stat_cols() ~ NA)
adir_18_24 <- df18_24 %>%
  select(ADI_R_SOCIAL_TOTAL_A, ADI_R_VERBAL_TOTAL_BV, ADI_RRB_TOTAL_C, SEX) %>%
  filter(DX_GROUP=="ASD") %>% 
  tbl_summary(by = SEX,
              statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median}", "{N_obs}")),
              type = list(all_continuous() ~ 'continuous2'),
              missing_text = "Missing",
              label = list(ADI_R_SOCIAL_TOTAL_A ~ "ADI-R Social",
                           ADI_R_VERBAL_TOTAL_BV ~ "ADI-R Verbal",
                           ADI_RRB_TOTAL_C ~ "ADI-R RRB")) %>%
  modify_header(all_stat_cols() ~  "**{level}**") %>% 
  bold_labels()

tvolume_18_24 <- tbl_summary(df18_24, by = GROUP.A,
                             statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median}")),
                             type = list(all_continuous() ~ 'continuous2'),
                             label = TOTAL_VOLUME ~ "18 to 24 years old",
                             include = TOTAL_VOLUME) %>% bold_labels() %>%
  modify_header(all_stat_cols() ~  "**{level}**<br>N = {n}")

gt::gtsave(as_gt(gtsummary::tbl_merge(list(n_6_10, n_11_14, n_15_17, n_18_24),
                                      tab_spanner = c("6 to 10", "11 to 14", "15 to 17", "18 to 24"))),
           filename = "./results/n_agegroups.png")
gt::gtsave(as_gt(gtsummary::tbl_merge(list(adir_6_10, adir_11_14, adir_15_17, adir_18_24),
                                      tab_spanner = c("**6 to 10**", "**11 to 14**", "**15 to 17**", "**18 to 24**"))),
           filename = "./results/adir_groups.png")
gt::gtsave(as_gt(gtsummary::tbl_stack(list(tvolume_6_10, tvolume_11_14, tvolume_15_17, tvolume_18_24))),
           filename = "./results/tvolume_groups.png")

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
