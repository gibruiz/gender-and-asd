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
  modify_header(all_stat_cols() ~  "**{level}**") %>% as_gt() %>% 
  gt::tab_options(table.font.names = "Times New Roman")
gt::gtsave(age_by_groups, filename = "./results/Age_by_groups.png")

## Histogram plot for AGE by GROUPS (SEX and DIAGNOSIS)
# Whole sample
histog <- ggplot(df, aes(x = AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df$SEX ~ df$DX_GROUP) +
  xlab("Age")
histog
ggsave("./results/histogram_age.png")

# 6 to 10 years old
df6_10 <- subset(df, AGE<11) #faixa etaria
attach(df6_10)
histog_6to10 <- ggplot(df6_10, aes(x = df6_10$AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df6_10$SEX ~ df6_10$DX_GROUP) +
  xlab("Age")
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
  modify_header(all_stat_cols() ~  "**{level}**")
tvolume_6_10


# 11 to 14 years old
df11_14 <- subset(df, AGE>=11 & AGE<15) #faixa etaria
attach(df11_14)
histog_11to14 <- ggplot(df11_14, aes(x = df11_14$AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df11_14$SEX ~ df11_14$DX_GROUP) +
  xlab("Age")
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
  modify_header(all_stat_cols() ~  "**{level}**")

# 15 to 17 years old
df15_17 <- subset(df, AGE>=15 & AGE<18) #faixa etaria
attach(df15_17)
histog_15to17 <- ggplot(df15_17, aes(x = df15_17$AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df15_17$SEX ~ df15_17$DX_GROUP) +
  xlab("Age")
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
  modify_header(all_stat_cols() ~  "**{level}**")

# 18 to 24 years old
df18_24 <- subset(df, AGE>=18)
attach(df18_24)
histog_18to24 <- ggplot(df18_24, aes(x = df18_24$AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df18_24$SEX ~ df18_24$DX_GROUP) +
  xlab("Age")
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
  modify_header(all_stat_cols() ~  "**{level}**")

gt::gtsave(gtsummary::tbl_merge(list(n_6_10, n_11_14, n_15_17, n_18_24),
                                      tab_spanner = c("**6 to 10**", "**11 to 14**", "**15 to 17**", "**18 to 24**")) %>% as_gt() %>% 
                   gt::tab_options(table.font.names = "Times New Roman"),
           filename = "./results/n_agegroups.png")
gt::gtsave(gtsummary::tbl_merge(list(adir_6_10, adir_11_14, adir_15_17, adir_18_24),
                                      tab_spanner = c("**6 to 10**", "**11 to 14**", "**15 to 17**", "**18 to 24**")) %>% as_gt() %>%
                   gt::tab_options(table.font.names = "Times New Roman"),
           filename = "./results/adir_groups.png")
gt::gtsave(gtsummary::tbl_stack(list(tvolume_6_10, tvolume_11_14, tvolume_15_17, tvolume_18_24)) %>% as_gt() %>% 
                   gt::tab_options(table.font.names = "Times New Roman"),
           filename = "./results/tvolume_groups.png")
