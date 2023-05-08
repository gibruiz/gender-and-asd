load("./data/df.RData")
setwd("./results")
attach(df)

### DEMOGRAPHICS FOR THE WHOLE SAMPLE ###
# Summary tables for age, divided by SEX, DIAGNOSTIC GROUP and SEXxDIAGNOSIS
# Includes median, mean, standard deviation (sd) and number of individuals
age_geral <- df %>%
  select(AGE, SEX, DX_GROUP) %>%
  tbl_summary(by=SEX)
  # summarise(across(AGE, list(median = median, mean = mean, sd = sd, n = length)))
age_geral
age_groups <- df %>% group_by(SEX, DX_GROUP) %>%
  summarise(across(AGE, list(median = median, mean = mean, sd = sd, n = length)))
age_groups
age_sex <- df %>% group_by(SEX) %>%
  summarise(across(AGE, list(median = median, mean = mean, sd = sd, n = length)))
age_sex
age_dx <- df %>% group_by(DX_GROUP) %>%
  summarise(across(AGE, list(median = median, mean = mean, sd = sd, n = length)))
age_dx

write.table(format(as.data.frame(age_geral),digits = 4), "idade.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(age_groups),digits = 4), "idadegrupos.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(age_sex),digits = 4), "idadesex.csv", sep = ",", quote = F, row.names = F)
write.table(format(as.data.frame(age_dx),digits = 4), "idadedx.csv", sep = ",", quote = F, row.names = F)

# Histogram plot for AGE by GROUPS (SEX and DIAGNOSIS)
histog <- ggplot(df, aes(x = AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df$SEX ~ df$DX_GROUP)
histog
ggsave("idadegrupos.png")


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

histog <- ggplot(df, aes(x = AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df$SEX ~ df$DX_GROUP)
histog
ggsave("6a10.png")


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

histog <- ggplot(df, aes(x = AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df$SEX ~ df$DX_GROUP)
histog
ggsave("11a14.png")



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

histog <- ggplot(df, aes(x = AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df$SEX ~ df$DX_GROUP)
histog
ggsave("15a17.png")

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

histog <- ggplot(df, aes(x = AGE)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(df$SEX ~ df$DX_GROUP)
histog
ggsave("18a24.png")
