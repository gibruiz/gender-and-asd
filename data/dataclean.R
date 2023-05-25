library(ggplot2)
library(dplyr)
library(forcats)


banco <- read.csv("./data/clean_Phenotypic_V1_0b_preprocessed1.csv")
attach(banco)
head(banco)
summary(banco)
banco$DX_GROUP <- as.factor(banco$DX_GROUP)
levels(banco$DX_GROUP) <- c("ASD","Ctrl")

head(banco$AGE)
banco$AGE <- floor(banco$AGE) # Rounds age down
summary(banco$AGE)
hist(banco$AGE)

# There are subjects with no correspondent file, I'm deleting their rows
df1 <- subset(banco, FILE_ID!="no_filename" & AGE<=24)
summary(df1)
attach(df1)
hist(df1$AGE)
hist(AGE_AT_SCAN)
df1[,1]
names(df1)

# Selecting columns that will be used
df2 <- df1[,5:35]
names(df2)
head(df2[,1])
# Creates column with names of files for later
df2$filenames <- paste(FILE_ID,"_roi_thickness.txt",sep="")
attach(df2)
head(filenames) # String with filenames in the same order as the dataframe
head(FILE_ID)

setwd("/home/gisele/Documentos/ABIDE/data/ANTS") # This is a relative path as I cannot upload these files

# Imports files in the same order as filenames
ants <- do.call(rbind, lapply(filenames, read.delim))
# Deleting subcortical volumes
ants <- ants[,38:99]
df <- cbind(df2, ants)
somavol <- rowSums(ants)
df$TOTAL_VOLUME <- somavol

head(df$TOTAL_VOLUME)
head(df)
df$SEX <- as.factor(df$SEX)
levels(df$SEX) <- c("Male", "Female")
df$GROUP.A <- interaction(df$SEX, df$DX_GROUP)
df$GROUP.A <- factor(df$GROUP.A, levels = c("Female.Ctrl", "Male.Ctrl", "Female.ASD", "Male.ASD"),
                     labels = c("Female Ctrl", "Male Ctrl", "Female ASD", "Male ASD"),
                     ordered = T)
df$GROUP.B <- factor(df$GROUP.A, levels = c("Female Ctrl", "Male Ctrl", "Male ASD", "Female ASD"),
                     ordered = T)
df$GROUP.C <- df$GROUP.B %>% fct_collapse(ASD = c("Male ASD", "Female ASD"))
# df$SITE_ID <- as.factor(df$SITE_ID)
attach(df)
head(df)


df$SRS_MANNERISMS[df$SRS_MANNERISMS==-9999] <- NA
df$ADI_R_SOCIAL_TOTAL_A[df$ADI_R_SOCIAL_TOTAL_A==-9999] <- NA
df$ADI_R_VERBAL_TOTAL_BV[df$ADI_R_VERBAL_TOTAL_BV==-9999] <- NA
df$ADI_RRB_TOTAL_C[df$ADI_RRB_TOTAL_C==-9999] <- NA
df$ADI_R_ONSET_TOTAL_D[df$ADI_R_ONSET_TOTAL_D==-9999] <- NA
df$ADI_R_RSRCH_RELIABLE[df$ADI_R_RSRCH_RELIABLE==-9999] <- NA
df$ADOS_MODULE[df$ADOS_MODULE==-9999] <- NA
df$ADOS_TOTAL[df$ADOS_TOTAL==-9999] <- NA
df$ADOS_COMM[df$ADOS_COMM==-9999] <- NA
df$ADOS_SOCIAL[df$ADOS_SOCIAL==-9999] <- NA
df$ADOS_STEREO_BEHAV[df$ADOS_STEREO_BEHAV==-9999] <- NA
df$ADOS_RSRCH_RELIABLE[df$ADOS_RSRCH_RELIABLE==-9999] <- NA
df$ADOS_GOTHAM_SOCAFFECT[df$ADOS_GOTHAM_SOCAFFECT==-9999] <- NA
df$ADOS_GOTHAM_RRB[df$ADOS_GOTHAM_RRB==-9999] <- NA
df$ADOS_GOTHAM_TOTAL[df$ADOS_GOTHAM_TOTAL==-9999] <- NA
df$ADOS_GOTHAM_SEVERITY[df$ADOS_GOTHAM_SEVERITY==-9999] <- NA
df$SRS_VERSION[df$SRS_VERSION==-9999] <- NA
df$SRS_RAW_TOTAL[df$SRS_RAW_TOTAL==-9999] <- NA
df$SRS_AWARENESS[df$SRS_AWARENESS==-9999] <- NA
df$SRS_COGNITION[df$SRS_COGNITION==-9999] <- NA
df$SRS_COMMUNICATION[df$SRS_COMMUNICATION==-9999] <- NA
df$SRS_MOTIVATION[df$SRS_MOTIVATION==-9999] <- NA
df$SRS_MANNERISMS[df$SRS_MANNERISMS==-9999] <- NA
df$SCQ_TOTAL[df$SCQ_TOTAL==-9999] <- NA


save("df", file="/home/gisele/Documentos/Mestrado/Pesquisa/ABIDE/gender-and-asd/data/df.RData")




