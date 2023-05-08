library(ggplot2)
setwd("/home/gisele/Documentos/ABIDE")
banco <- read.csv("clean_Phenotypic_V1_0b_preprocessed1.csv")
attach(banco)
head(banco)
summary(banco)
banco$DX_GROUP <- as.factor(banco$DX_GROUP)
levels(banco$DX_GROUP) <- c("ASD","Ctrl")

head(banco$AGE)
banco$AGE <- floor(banco$AGE) #arredonda idade p baixo
summary(banco$AGE)
hist(banco$AGE)

df1 <- subset(banco, FILE_ID!="no_filename" & AGE<=24) #deleta sujeitos sem arquivo
summary(df1)
attach(df1)
hist(df1$AGE)
hist(AGE_AT_SCAN)
df1[,1]
names(df1)

df2 <- df1[,5:35]
names(df2)
head(df2[,1])
df2$filenames <- paste(FILE_ID,"_roi_thickness.txt",sep="") #criando coluna c nomes dos arquivos p puxar dps
attach(df2)
head(filenames) #filenames eh uma string com os nomes dos arquivos na ordem certa
head(FILE_ID)

setwd("/home/gisele/Documentos/ABIDE/data/ANTS")
ants <- do.call(rbind, lapply(filenames, read.delim)) #importa os arquivos na ordem do filenames
ants <- ants[,38:99] #deleta volumes subcorticais
df <- cbind(df2, ants) #juntando dataframe inicial com o criado importando ants
somavol <- rowSums(ants) #soma volumes
df$TOTAL_VOLUME <- somavol

head(df$TOTAL_VOLUME)
head(df)
attach(df)
df$SEX <- as.factor(df$SEX)
levels(df$SEX) <- c("Male", "Female")
df$GROUP <- interaction(df$SEX, df$DX_GROUP)
df$GROUP <- factor(df$GROUP, levels = c("Female.Ctrl", "Male.Ctrl", "Female.ASD", "Male.ASD"),
                        ordered = T)


df$SRS_MANNERISMS[df$SRS_MANNERISMS==-9999] <- NA
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


save("df", file="/home/gisele/Documentos/ABIDE/df.RData")




