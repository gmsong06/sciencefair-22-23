############################# File #3 ##################################################
#### This file is to get patient information, including:
# Age
# gender
# education
# years to follow up
# patients that have progress to demented and AD
# Time takes to progress to each stage

#########################################################################################
#################### General Stats on Each individual files  ############################
#########################################################################################

################################################################
########## Clinial Diagnosis & Demographic of the patients
#################################################################
load("ClinDia_Patient.RData")
str(ClinDia_Patient)                   # 7653 obs. of  6 variables
n_distinct(ClinDia_Patient$OASISID)    # 1336 patients
summary(ClinDia_Patient$DEMENTED)

Demographics_raw <- read.csv(file = 'OASIS3_demographics.csv')
str(Demographics_raw)
save(Demographics_raw, file = "Demographics_raw.RData")
load("Demographics_raw.RData")
Demo_raw2 <- Demographics_raw %>% dplyr::select(OASISID, GENDER, EDUC, HAND)
Demo_raw2$GENDER <- as.factor(Demo_raw2$GENDER)   # 2:female; 1: male
Demo_raw2$HAND <- as.factor(Demo_raw2$HAND)   # 1: left; 2: right; 3: both
str(Demo_raw2)
summary(Demo_raw2)

###########
### Stats
###########
ClinDia_Patient_stats <- merge(ClinDia_Patient, Demo_raw2, all.x = TRUE)
save(ClinDia_Patient_stats, file = "ClinDia_Patient_stats.RData")
load("ClinDia_Patient_stats.RData")
# GENDER
summary(ClinDia_Patient_stats$GENDER)
#    1    2 
# 3436 4217
tapply(ClinDia_Patient_stats$GENDER, ClinDia_Patient_stats$DEMENTED, summary) 
# $`0`
# 1   2 
# 278 207 
# 
# $`1`
# 1   2 
# 756 613 
# 
# $`3`
# 1    2 
# 2402 3397 

# Age
stat.desc(ClinDia_Patient_stats$age.at.visit)
tapply(ClinDia_Patient_stats$age.at.visit, ClinDia_Patient_stats$DEMENTED, summary) 
# $`0`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 54.73   72.42   77.08   77.32   82.60  100.55 
# 
# $`1`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 50.33   73.21   78.95   78.61   84.08   99.24 
# 
# $`3`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 42.50   68.89   73.64   73.54   78.64   98.34 
tapply(ClinDia_Patient_stats$age.at.visit, ClinDia_Patient_stats$DEMENTED, sd) 
#        0        1        3 
# 7.711747 7.921201 8.123024

# Education
stat.desc(ClinDia_Patient_stats$EDUC)
tapply(ClinDia_Patient_stats$EDUC, ClinDia_Patient_stats$DEMENTED, summary) 
# $`0`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.0    13.0    16.0    15.6    18.0    29.0 
# 
# $`1`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00   12.00   16.00   15.17   18.00   29.00 
# 
# $`3`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00   14.00   16.00   16.03   18.00   29.00 
tapply(ClinDia_Patient_stats$EDUC, ClinDia_Patient_stats$DEMENTED, sd) 
#         0        1        3 
# 2.716721 2.953913 2.511195

# HAND
summary(ClinDia_Patient_stats$HAND)
#       B    L    R 
# 38   32  734 6849 
tapply(ClinDia_Patient_stats$HAND, ClinDia_Patient_stats$DEMENTED, summary) 
# $`0`
#     B   L   R 
# 5   0  42 438 
# 
# $`1`
#       B    L    R 
# 14   17  116 1222 
# 
# $`3`
#       B    L    R 
# 19   15  576 5189
# tapply(ClinDia_Patient_stats$HAND, ClinDia_Patient_stats$DEMENTED, sd) # ERROR
#         0        1        3 
# 2.716721 2.953913 2.511195

##### t-test
Clin_2groups <- filter(ClinDia_Patient_stats, DEMENTED != "3")   # use for all three groups. change the demented value
summary(Clin_2groups$DEMENTED)
Clin_2groups$DEMENTED <- as.factor(as.character(Age_2groups$DEMENTED)) # ERROR
# t-test
t.test(age.at.visit ~ DEMENTED, data = Clin_2groups)
t.test(EDUC ~ DEMENTED, data = Clin_2groups)

# clinical follow up duration Max
MaxYear <- tapply(ClinDia_Patient_stats$years_to_Visit, ClinDia_Patient_stats$OASISID, max) 
summary(MaxYear)
sd(MaxYear)

#############################################################
# Find OASISIDs that have progression  ######################
#############################################################

load("ClinDia_Patient.RData")         #7653 obs. of  6 variables:
str(ClinDia_Patient)
n_distinct(ClinDia_Patient$OASISID)    # 1336 patients

DEMETED_Status_ClinDia <- do.call(rbind, tapply(ClinDia_Patient$DEMENTED, ClinDia_Patient$OASISID, summary))
DEMETED_Status2_ClinDia <- cbind(rownames(DEMETED_Status_ClinDia), DEMETED_Status_ClinDia)
is.data.frame(DEMETED_Status2_ClinDia)
DEMETED_Status2_ClinDia <- as.data.frame(DEMETED_Status2_ClinDia)
DEMETED_Status2_ClinDia$Zeros <- rowSums(DEMETED_Status2_ClinDia == 0)
View(DEMETED_Status2_ClinDia)
save(DEMETED_Status2_ClinDia, file = "DEMETED_Status2_ClinDia.RData")

##################
### Find IDs that has progression and define the type of progression
# CN: Cognitive Normal
# MCI: Mild cognitive impairment
# DMN: Dementia (Assuming AD is the same as Dementia)
##################

DEMETED_Progress_ClinDia <- filter(DEMETED_Status2_ClinDia, Zeros < 2)
colnames(DEMETED_Progress_ClinDia)[1] <- "OASISID" 
dim(DEMETED_Progress_ClinDia) # 347 patients has progressed 
view(DEMETED_Progress_ClinDia)
DEMETED_Progress_ClinDia$Zeros <- as.factor(DEMETED_Progress_ClinDia$Zeros)
summary(DEMETED_Progress_ClinDia)

DEMETED_Progress_ClinDia2 <- DEMETED_Progress_ClinDia
DEMETED_Progress_ClinDia2$Progress <- 
  ifelse(DEMETED_Progress_ClinDia2$Zeros == 0, 'CN-MCI-DMN', 
         ifelse(DEMETED_Progress_ClinDia2$`3` == 0, 'MCI-DMN', 
                ifelse(DEMETED_Progress_ClinDia2$`0` != 0, 'CN-MCI', 'CN-DMN')))
view(DEMETED_Progress_ClinDia2)
DEMETED_Progress_ClinDia2$Progress <- as.factor(DEMETED_Progress_ClinDia2$Progress)
summary(DEMETED_Progress_ClinDia2$Progress)
# CN-DMN     CN-MCI CN-MCI-DMN    MCI-DMN 
# 84         92        125         46
save(DEMETED_Progress_ClinDia2, file = "DEMETED_Progress_ClinDia2.RData")

###############################################
#######Find time takes to progress
############################################### 
# time takes to progress
# age
# gender

DEMETED_ClinDia <- merge(DEMETED_Progress_ClinDia2, ClinDia_Patient, by = "OASISID", all.x = TRUE)
save(DEMETED_ClinDia, file = "DEMETED_ClinDia.RData")
str(DEMETED_ClinDia)
summary(DEMETED_ClinDia)
view(DEMETED_ClinDia)
DEMETED_ClinDia2 <- DEMETED_ClinDia %>% 
  dplyr::select(c('OASISID', 'age.at.visit', 'days_to_visit','years_to_Visit', 'DEMENTED'))
view(DEMETED_ClinDia2)
save(DEMETED_ClinDia2, file = "DEMETED_ClinDia2.RData")
write_xlsx(DEMETED_ClinDia2, "DEMETED_ClinDia2.xlsx")

DEMETED_ClinDia3 <- DEMETED_ClinDia2 %>% arrange(OASISID, age.at.visit)
view(DEMETED_ClinDia3)
DEMETED_ClinDia3$DementedLetter <- ifelse(DEMETED_ClinDia3$DEMENTED == '3', 'CN', 
                                          ifelse(DEMETED_ClinDia3$DEMENTED == '0', 'MCI', 'DMN'))
DEMETED_ClinDia3$DementedNumber <- ifelse(DEMETED_ClinDia3$DEMENTED == '3', '10', 
                                          ifelse(DEMETED_ClinDia3$DEMENTED == '0', '11', '12'))
str(DEMETED_ClinDia3)
DEMETED_ClinDia3$DementedNumber <- as.integer(DEMETED_ClinDia3$DementedNumber)
save(DEMETED_ClinDia3, file = "DEMETED_ClinDia3.RData")
load("DEMETED_ClinDia3.RData")

# Box plot by group
ggplot(data = DEMETED_ClinDia3, mapping = aes(y = age.at.visit,                     
                                              x = DementedLetter,                           
                                              color = DementedLetter, fill = DementedLetter)) + 
  geom_boxplot()+ 
  geom_jitter() 


NCol <- ncol(DEMETED_ClinDia3)
DEMETED_ClinDia4 <- data.frame(matrix(ncol = NCol+1, nrow = 0))
cname <- c(colnames(DEMETED_ClinDia3), 'Progress')
colnames(DEMETED_ClinDia4) <- cname
DEMETED_ClinDia4
ListID <- unique(DEMETED_ClinDia3$OASISID)

for (OASID in ListID) {
  Patient <- filter(DEMETED_ClinDia3, OASISID == OASID)
  greatestDemented <- Patient$DementedNumber[1]
  greatestDementedLetter <- Patient$DementedLetter[1]
  for(i in 1:nrow(Patient)){
    if(greatestDemented <= Patient$DementedNumber[i]) {
      greatestDemented <- Patient$DementedNumber[i]
      greatestDementedLetter <- Patient$DementedLetter[i]
    }
    else{
      Patient$DementedNumber[i] <- greatestDemented
      Patient$DementedLetter[i] <- greatestDementedLetter
    }
  }
  Patient$Progress <- c(0, diff(Patient$DementedNumber))
  DEMETED_ClinDia4 <- rbind(DEMETED_ClinDia4, Patient)
}
view(DEMETED_ClinDia4)
save(DEMETED_ClinDia4, file = "DEMETED_ClinDia4.RData")

#########################################
##### Find duration
#####dataframe that calculates duration of each stage
#########################################
df<- DEMETED_ClinDia4
duration <- data.frame(matrix(ncol = 5, nrow = num.patients)) # ERROR
colnames(duration) <- c('OASISID', 'age.at.visit', 'CN', 'MCI', 'DMN') # ERROR
cur.id <- df$OASISID[1]
cur.letter <- df$DementedLetter[1]
original.age <- df$age.at.visit[1]
cur.row <- 1
duration$OASISID[cur.row] <- cur.id # ERROR
duration$age.at.visit[cur.row] <- original.age # ERROR
for(i in 1:nrow(df)){
  id <- df$OASISID[i]
  age <- df$age.at.visit[i]
  letter <- df$DementedLetter[i]
  years <- df$years_to_Visit[i]
  if(letter != cur.letter | id != cur.id){
    if(cur.letter == 'CN'){
      duration$CN[cur.row] <- round(df$age.at.visit[i - 1] - original.age + 1, 0)
    }
    if(cur.letter == 'MCI'){
      duration$MCI[cur.row] <- round(df$age.at.visit[i - 1] - original.age + 1, 0)
    }
    if(cur.letter == 'DMN'){
      duration$DMN[cur.row] <- round(df$age.at.visit[i - 1] - original.age + 1, 0)
    }
    original.age <- age
    cur.letter <- letter
  }
  if(id != cur.id){
    cur.row <- cur.row + 1
    cur.id <- id
    duration$OASISID[cur.row] <- cur.id
    duration$age.at.visit[cur.row] <- age
  }
}
view(duration)
save(duration, file = "duration.RData")
load("duration.RData")

duration3 <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(duration3) <- c('OASISID', 'age.at.visit', 'CN', 'MCI', 'DMN')
for(i in 1:nrow(duration)){
  sum_na <- sum(is.na(duration$CN[i])+is.na(duration$MCI[i])+is.na(duration$DMN[i]))
  if(sum_na < 2){
    duration3 <- rbind(duration3, duration[i,])
  }
}

CogProg <- duration3
CogProg <- CogProg %>%
  add_column(CN_MCI = NA) %>%
  add_column(MCI_DMN = NA) %>%
  add_column(CN_DMN = NA) 
for(i in 1:nrow(CogProg)) {
  if(!is.na(CogProg$CN[i]) & !is.na(CogProg$MCI[i]) & !is.na(CogProg$DMN[i])){
    CogProg$CN_MCI[i] <- CogProg$CN[i]
    CogProg$MCI_DMN[i] <- CogProg$MCI[i]
  } else if (!is.na(CogProg$CN[i]) & !is.na(CogProg$MCI[i]) & is.na(CogProg$DMN[i])) {
    CogProg$CN_MCI[i] <- CogProg$CN[i]
  } else if (!is.na(CogProg$CN[i]) & is.na(CogProg$MCI[i]) & !is.na(CogProg$DMN[i])) {
    CogProg$CN_DMN[i] <- CogProg$CN[i]
  } else  {CogProg$MCI_DMN[i] <- CogProg$MCI[i]}
}
view(CogProg)
nrow(CogProg)            # 272 patients
save(CogProg, file = "CogProg.RData")
load("CogProg.RData")
view(CogProg)

sum(!is.na(CogProg$CN_MCI))   # 133
summary(CogProg$CN_MCI)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   2.000   4.000   4.752   7.000  16.000     139
stat.desc(CogProg$CN_MCI)
# nbr.val     nbr.null       nbr.na          min          max        range          sum       median         mean 
# 133.0000000    0.0000000  139.0000000    1.0000000   16.0000000   15.0000000  632.0000000    4.0000000    4.7518797 
# SE.mean   CI.mean.0.95          var      std.dev     coef.var 
# 0.3213391    0.6356405   13.7334245    3.7058635    0.7798732

sum(!is.na(CogProg$MCI_DMN))   # 123
summary(CogProg$MCI_DMN)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   1.000   2.000   2.561   4.000  11.000     149 
stat.desc(CogProg$MCI_DMN)
#    nbr.val     nbr.null       nbr.na          min          max        range          sum       median         mean 
# 123.0000000    0.0000000  149.0000000    1.0000000   11.0000000   10.0000000  315.0000000    2.0000000    2.5609756 
#   SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 0.1789042    0.3541587    3.9368253    1.9841435    0.7747608 

sum(!is.na(CogProg$CN_DMN))   # 86
summary(CogProg$CN_DMN)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.00    1.00    3.50    4.93    7.75   16.00     186  
stat.desc(CogProg$CN_DMN)
#   nbr.val     nbr.null       nbr.na          min          max        range          sum       median         mean 
# 86.0000000    0.0000000  186.0000000    1.0000000   16.0000000   15.0000000  424.0000000    3.5000000    4.9302326 
# SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 0.4568346    0.9083096   17.9480164    4.2365099    0.8592921

### CN_MCI_DMN
CN_MCI_DMN <- filter(CogProg, !is.na(CogProg$CN_MCI) & !is.na(CogProg$MCI_DMN))
nrow(CN_MCI_DMN) #70

################################################################################
################### Plot time takes to progress ################################
################################################################################

view(CogProg)

df1 <- CogProg[!is.na(CogProg$CN_MCI),]
df1_cut <- df1 %>% dplyr::select(OASISID, CN_MCI)
df1_cut$ProgType <- "CN_MCI"
colnames(df1_cut)[which(names(df1_cut) == "CN_MCI")] <- "Duration"


df2 <- CogProg[!is.na(CogProg$CN_DMN),]
df2_cut <- df2 %>% dplyr::select(OASISID, CN_DMN)
df2_cut$ProgType <- "CN_DMN"
colnames(df2_cut)[which(names(df2_cut) == "CN_DMN")] <- "Duration"

df3 <- CogProg[!is.na(CogProg$MCI_DMN),]
df3_cut <- df3 %>% dplyr::select(OASISID, MCI_DMN)
df3_cut$ProgType <- "MCI_DMN"
colnames(df3_cut)[which(names(df3_cut) == "MCI_DMN")] <- "Duration"

CogProg_Type <- rbind(df1_cut, df2_cut, df3_cut)
view(CogProg_Type)

ggplot(data = CogProg_Type, mapping = aes(y = Duration,                     
                                         x = ProgType,                           
                                         color = ProgType)) +                      
  geom_boxplot()+ 
  #geom_jitter() +
  PlotTheme +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  # scale_fill_manual(values = c("green4", "#E7B800", "#FC4E07")) +
  # scale_color_manual(values = c( "green4", "#E7B800", "#FC4E07")) +
  LegendTheme +
  labs(title = "progression")


###### TO Check if there is significant difference between CN_MCI and CN_DMN
CogProg_FromCN <- filter(CogProg_Type, ProgType != "MCI_DMN")
t.test(Duration ~ ProgType, data = CogProg_FromCN)    # No Significant differences between CN_MCI and CN_DMN
# Welch Two Sample t-test
# 
# data:  Duration by ProgType
# t = 0.31932, df = 164.06, p-value = 0.7499
# alternative hypothesis: true difference in means between group CN_DMN and group CN_MCI is not equal to 0
# 95 percent confidence interval:
#   -0.9244827  1.2811884
# sample estimates:
#   mean in group CN_DMN mean in group CN_MCI 
# 4.930233             4.751880 

###### TO Check if there is significant difference between MCI_DMN and CN_DMN
CogProg_ToDMN <- filter(CogProg_Type, ProgType != "CN_MCI")
t.test(Duration ~ ProgType, data = CogProg_ToDMN)    # Significant differences between MCI_DMN and CN_DMN
# Welch Two Sample t-test
# 
# data:  Duration by ProgType
# t = 4.8291, df = 111.25, p-value = 4.409e-06
# alternative hypothesis: true difference in means between group CN_DMN and group MCI_DMN is not equal to 0
# 95 percent confidence interval:
#   1.397091 3.341422
# sample estimates:
#   mean in group CN_DMN mean in group MCI_DMN 
# 4.930233              2.560976 

###### TO Check if there is significant difference between CN_MCI and MCI_DMN
CogProg_OneStep <- filter(CogProg_Type, ProgType != "CN_DMN")
t.test(Duration ~ ProgType, data = CogProg_OneStep)    # Significant difference between CN_MCI and MCI_DMN
# Welch Two Sample t-test
# 
# data:  Duration by ProgType
# t = 5.957, df = 205.18, p-value = 1.102e-08
# alternative hypothesis: true difference in means between group CN_MCI and group MCI_DMN is not equal to 0
# 95 percent confidence interval:
#   1.465782 2.916026
# sample estimates:
#   mean in group CN_MCI mean in group MCI_DMN 
# 4.751880              2.560976


# Violin plot
ggplot(data = CogProg_Type, mapping = aes(y = Duration,                     
                                          x = ProgType, fill = ProgType, color = ProgType)) +                      
  geom_violin()+ 
  geom_jitter() +
  PlotTheme +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Progression Duration")

# Box plot
ggplot(data = CogProg_Type, mapping = aes(y = Duration,                     
                                          x = ProgType, fill = ProgType, color = ProgType)) +                      
  geom_boxplot()+ 
  geom_jitter() +
  PlotTheme +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Progression Duration")

#####################################################
###### patients that did not progressed
####################################################

CogNoProg <- ClinDia_Patient_stats[!ClinDia_Patient_stats$OASISID %in% CogProg$OASISID,]
nrow(CogNoProg)
nrow(ClinDia_Patient_stats)
view(CogNoProg)
n_distinct(CogNoProg$OASISID)
n_distinct(ClinDia_Patient_stats$OASISID)
n_distinct(CogProg$OASISID)

MaxYear_NoProg <- tapply(CogNoProg$years_to_Visit, CogNoProg$OASISID, max) 
n_distinct()
summary(MaxYear_NoProg)
sd(MaxYear_NoProg)


################################################################
########## Centiloid
#################################################################
load("Centiloid_raw.RData")
n_distinct(Centiloid_raw$OASISID)  # 1004 patients
view(Centiloid_raw)
str(Centiloid_raw)
Centiloid_fSUVR <- Centiloid_raw %>% dplyr::select(OASISID, tracer, Centiloid_fSUVR_TOT_CORTMEAN, years_to_Visit)
Clin_Centi <- merge(ClinDia_Patient_stats, Centiloid_fSUVR, by = c("OASISID", "years_to_Visit"), all.y = TRUE)
summary(Clin_Centi)
str(Clin_Centi)   #1914 obs. of  11 variables
n_distinct(Clin_Centi$OASISID)    
Clin_Centi2 <- na.omit(Clin_Centi)   
n_distinct(Clin_Centi2$OASISID)  #  941 patients
save(Clin_Centi2, file = "Clin_Centi2.RData")
load("Clin_Centi2.RData")
str(Clin_Centi2)            # 1585 obs. of  11 variables
view(ClinDia_Patient_stats)
nrow(Centiloid_raw)
nrow(Clin_Centi2)
view(Clin_Centi)

stat.desc(Clin_Centi2$Centiloid_fSUVR_TOT_CORTMEAN)
#     nbr.val      nbr.null        nbr.na           min           max         range           sum        median 
# 1585.0000000     0.0000000     0.0000000   -39.6984000   203.9508000   243.6492000 36828.8922000     6.9222000 
#       mean       SE.mean  CI.mean.0.95           var       std.dev      coef.var 
# 23.2358941     0.9262991     1.8169013  1359.9776919    36.8778754     1.5871081
tapply(Clin_Centi2$Centiloid_fSUVR_TOT_CORTMEAN, Clin_Centi2$DEMENTED, summary)
# $`0`
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -26.3980   0.9968  20.6370  44.3121  92.1145 203.9508 
# 
# $`1`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -27.54   30.37   81.38   68.86  100.16  159.42 
# 
# $`3`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -39.698  -1.575   4.910  16.874  22.024 176.793 
tapply(Clin_Centi2$Centiloid_fSUVR_TOT_CORTMEAN, Clin_Centi2$DEMENTED, sd)
#        0        1        3 
# 52.08534 43.90750 30.39122 
summary(Clin_Centi2$DEMENTED)

Centi_2groups <- filter(Clin_Centi2, DEMENTED != "3")
t.test(Centiloid_fSUVR_TOT_CORTMEAN ~ DEMENTED, data = Centi_2groups)

ggplot(data = Clin_Centi2, mapping = aes(y = Centiloid_fSUVR_TOT_CORTMEAN,                     
                                         x = DEMENTED,                           
                                         color = DEMENTED, fill = DEMENTED)) +                      
  geom_boxplot()+ 
  geom_jitter() +
  PlotTheme +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Corital Mean Centiloid vs. Cognitive Normality")


Clin_Centi2$DEMENTED = factor(Clin_Centi2$DEMENTED, levels=c("3", "0", "1"))

ggplot(data = Clin_Centi2, mapping = aes(y = Centiloid_fSUVR_TOT_CORTMEAN,                     
                                           x = DEMENTED,                           
                                           color = DEMENTED, fill = DEMENTED)) +                      
  geom_boxplot()+ 
  geom_jitter() +
  PlotTheme +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("green4", "#E7B800", "#FC4E07")) +
  scale_color_manual(values = c( "green4", "#E7B800", "#FC4E07")) +
  LegendTheme +
  labs(title = "Corital Mean Centiloid vs. Cognitive Normality")


################################################################
########## Freesurf
#################################################################
load("Freesurf_cut.RData")
n_distinct(Freesurf_cut$OASISID)    # 1316 subjects
Clin_FreeSuf <- merge(ClinDia_Patient_stats, Freesurf_cut, by=c("OASISID", "years_to_Visit"), all.y = TRUE)
n_distinct(Clin_FreeSuf$OASISID)  
str(Clin_FreeSuf)                     #2705 obs. of  95 variables
summary(Clin_FreeSuf$OASISID)
summary(Clin_FreeSuf)
Clin_FreeSuf2 <- Clin_FreeSuf[!is.na(Clin_FreeSuf$NORMCOG),]
n_distinct(Clin_FreeSuf2$OASISID)    # 1194 subjects
str(Clin_FreeSuf2)                   # 2164 obs. of  95 variables:
summary(Clin_FreeSuf2$DEMENTED)
save(Clin_FreeSuf2, file = "Clin_FreeSuf2.RData")

#########################################################################################
###################### General Stats on the merged data of all three files###############
#########################################################################################
load("final2.RData")
str(final2)    #1782 obs. of  95 variables
final_stats <- final2 %>% dplyr::select(-NORMCOG)
final_stats2 <- na.omit(final_stats)
str(final_stats2)    #1672 obs. of  94 variables
save(final_stats2, file = "final_stats2.RData")
n_distinct(final_stats2$OASISID)       # 894 patients

##### Merge with Final_NoNA
Merged_FinalDemo <- merge(final_stats2, Demo_raw2, by = "OASISID", all.x = TRUE)
str(Merged_FinalDemo)   #1672 obs. of  97 variables
demo_list <- c("GENDER", "EDUC", "HAND")
Merged_FinalDemo2 <- Merged_FinalDemo %>% relocate(unlist(demo_list), .before = DEMENTED)
dim(Merged_FinalDemo2)

#####################################
head(Merged_FinalDemo2)
dim(Merged_FinalDemo2)
n_distinct(Merged_FinalDemo2$OASISID)
summary(Merged_FinalDemo2)

#########################
####### age stats
#########################
stat.desc(Merged_FinalDemo2$age.at.visit)
tapply(Merged_FinalDemo2$age.at.visit, Merged_FinalDemo2$DEMENTED, summary) 
# $`0` MCI
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 57.50   70.57   74.99   74.79   80.36   91.37 
# 
# $`1`Demented
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 53.86   69.71   75.11   74.65   79.23   91.23 
# 
# $`3`Normal
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 42.50   64.45   69.96   69.41   74.87   92.26 
tapply(Merged_FinalDemo2$age.at.visit, Merged_FinalDemo2$DEMENTED, sd) 
#        0        1        3 
# 6.939787 6.876834 8.465600 

#########################
####### Education
#########################
stat.desc(Merged_FinalDemo2$EDUC)
#     nbr.val     nbr.null       nbr.na          min          max        range          sum       median         mean 
# 1.672000e+03 0.000000e+00 0.000000e+00 6.000000e+00 2.400000e+01 1.800000e+01 2.690200e+04 1.600000e+01 1.608971e+01 
# SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 6.187502e-02 1.213607e-01 6.401283e+00 2.530076e+00 1.572480e-01
tapply(Merged_FinalDemo2$EDUC, Merged_FinalDemo2$DEMENTED, summary) 
# $`0`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 9.00   14.00   16.00   15.69   18.00   20.00 
# 
# $`1`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.00   12.00   16.00   15.16   17.50   21.00 
# 
# $`3`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00   14.00   16.00   16.21   18.00   24.00 
tapply(Merged_FinalDemo2$EDUC, Merged_FinalDemo2$DEMENTED, sd)
#        0        1        3 
# 2.388015 2.816729 2.485100

#########################
####### GENDER
#########################
summary(Merged_FinalDemo2$GENDER)
#   1   2 
# 713 959

tapply(Merged_FinalDemo2$GENDER, Merged_FinalDemo2$DEMENTED, summary) 
# $`0`
# 1  2 
# 28 43 
# 
# $`1`
# 1  2 
# 84 66 
# 
# $`3`
# 1   2 
# 601 850 

#########################
####### HAND
#########################
summary(Merged_FinalDemo2$HAND)
#       B    L    R 
# 13    3  144 1512

tapply(Merged_FinalDemo2$HAND, Merged_FinalDemo2$DEMENTED, summary)
# $`0`
#    B  L  R 
# 2  0  5 64 
# 
# $`1`
#     B   L   R 
# 1   1  13 135 
# 
# $`3`
#       B    L    R 
# 10    2  126 1313 

#########################
####### Follow Up
#########################
stat.desc(Merged_FinalDemo2$years_to_Visit)
#      nbr.val     nbr.null       nbr.na          min          max        range          sum       median         mean 
# 1672.0000000  685.0000000    0.0000000    0.0000000   25.0000000   25.0000000 6355.0000000    3.0000000    3.8008373 
# SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 0.1100298    0.2158108   20.2421776    4.4991308    1.1837210

tapply(Merged_FinalDemo2$years_to_Visit, Merged_FinalDemo2$DEMENTED, summary)
# $`0`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   2.000   4.239   9.000  18.000 
# 
# $`1`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   0.000   2.067   2.750  17.000 
# 
# $`3`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   3.000   3.959   7.000  25.000 

tapply(Merged_FinalDemo2$years_to_Visit, Merged_FinalDemo2$DEMENTED, sd)
#        0        1        3 
# 5.194681 3.629977 4.508346

#########################################
## Progression from CN to MCI or AD 
##########################################

DEMETED_Status <- do.call(rbind, tapply(Merged_FinalDemo2$DEMENTED, Merged_FinalDemo2$OASISID, summary))
DEMETED_Status2 <- cbind(rownames(DEMETED_Status), DEMETED_Status)
# is.data.frame(DEMETED_Status2)
DEMETED_Status2 <- as.data.frame(DEMETED_Status2)
DEMETED_Status2$Zeros <- rowSums(DEMETED_Status2 == 0)
View(DEMETED_Status2)
save(DEMETED_Status2, file = "DEMETED_Status2.RData")

# Find OASISIDs that has progression
DEMETED_Progress <- filter(DEMETED_Status2, Zeros < 2)
dim(DEMETED_Progress) # 32 patients has progressed in the merged group (ClinDia+PET+MRI)



