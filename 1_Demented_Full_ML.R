############################# File #1 ##################################################
#### Read in Files: OASIS3 Clinical Diagnosis, freesurfer and Ceniloid Data 
#### Data cleaning
#### Data Exploration
#### Machine Learning with full low correlated variables 
# Neural Network
# Naive Baysein
# Random Forest
# Decision Tree

ClinDia_raw <- read.csv(file = 'OASIS3_UDSd1_diagnoses.csv')
str(ClinDia_raw)    # 8499 obs. of  149 variables
save(ClinDia_raw, file = "ClinDia_raw.RData")
ClinDia_Patient <- ClinDia_raw[, 1:7]
ClinDia_Patient <- ClinDia_Patient[, -5]
ClinDia_Patient <- ClinDia_Patient[, -2]   # remove oasis_session_id 
ClinDia_Patient$DEMENTED <- ifelse(ClinDia_Patient$NORMCOG == 1, 3, 
                                   ClinDia_Patient$DEMENTED) # 3 if Question 2 NORMCOG = 1 (Yes)
ClinDia_Patient$NORMCOG <- as.factor(ClinDia_Patient$NORMCOG)
ClinDia_Patient$DEMENTED <- as.factor(ClinDia_Patient$DEMENTED)
ClinDia_Patient <- na.omit(ClinDia_Patient)
ClinDia_Patient$years_to_Visit <- round(ClinDia_Patient$days_to_visit/360)
str(ClinDia_Patient)
summary(ClinDia_Patient)
save(ClinDia_Patient, file = "ClinDia_Patient.RData")
load("ClinDia_Patient.RData")
summary(ClinDia_Patient$DEMENTED)

Centiloid_raw <- read.csv(file = 'OASIS3_amyloid_centiloid.csv')
colnames(Centiloid_raw)[colnames(Centiloid_raw) == "subject_id"] <- "OASISID"
Centiloid_raw$days_to_visit <- str_sub(Centiloid_raw$oasis_session_id,-4)    
Centiloid_raw$days_to_visit <- as.integer(Centiloid_raw$days_to_visit)
Centiloid_raw$years_to_Visit <- round(Centiloid_raw$days_to_visit/360)
Centiloid_raw$tracer <- as.factor(Centiloid_raw$tracer)
Centiloid_raw <- Centiloid_raw[, -c(2:3)]
str(Centiloid_raw)  # 1893 obs. of  7 variables
# 'data.frame':	1893 obs. of  8 variables:
# $ OASISID                         : chr  "OAS30001" "OAS30002" "OAS30003" "OAS30003" ...
# $ tracer                          : Factor w/ 2 levels "AV45","PIB": 1 1 1 1 1 1 1 1 1 1 ...
# $ Centiloid_fBP_TOT_CORTMEAN      : num  NA NA NA NA NA NA NA NA NA NA ...
# $ Centiloid_fSUVR_TOT_CORTMEAN    : num  8.78 -2.35 -12.16 -11.51 15.16 ...
# $ Centiloid_fBP_rsf_TOT_CORTMEAN  : num  NA NA NA NA NA NA NA NA NA NA ...
# $ Centiloid_fSUVR_rsf_TOT_CORTMEAN: num  5.68 1.5 3.32 2.2 5.68 ...
# $ days_to_visit                   : int  2430 2340 2682 3731 2232 3457 2384 3453 2342 1636 ...
# $ years_to_Visit                  : num  7 6 7 10 6 10 7 10 7 5 ...
summary(Centiloid_raw)
save(Centiloid_raw, file = "Centiloid_raw.RData")
load("Centiloid_raw.RData")
n_distinct(Centiloid_raw$OASISID)  # 1004 patients

# Centiloid_measure <- Centiloid_raw[, c(3:6)]
# Centiloid_measure <- na.omit(Centiloid_measure)
# Centiloid_coor <- cor(Centiloid_measure[,1:4],use="complete.obs")
# Centiloid_coor
# highlyCorrelated_Centi <- findCorrelation(Centiloid_measure, cutoff=(0.59),verbose = FALSE)

#################################################
############### Freesurf   ######################
#################################################
Freesurf_raw <- read.csv(file = 'OASIS3_Freesurfer_output.csv')
colnames(Freesurf_raw)[colnames(Freesurf_raw) == "Subject"] <- "OASISID"
Freesurf_raw$days_to_visit <- str_sub(Freesurf_raw$MR_session,-4)
Freesurf_raw$days_to_visit <- as.integer(Freesurf_raw$days_to_visit)
Freesurf_raw$years_to_Visit <- round(Freesurf_raw$days_to_visit/360)
Freesurf_raw <- Freesurf_raw[, -c(2:7)]
str(Freesurf_raw)   # 2681 obs. of  199 variables
save(Freesurf_raw, file = "Freesurf_raw.RData")
load("Freesurf_raw.RData")
n_distinct(Freesurf_raw$OASISID)      # 1316 patients

#### Freesurf: find important variables
dim(Freesurf_raw)
Freesurf_measure <- Freesurf_raw[, -c(1, 198, 199)]
dim(Freesurf_measure)
#FreeSurf_coor <- cor(Freesurf_measure[,1:n],use="complete.obs")
#dim(FreeSurf_coor)
zv <- apply(Freesurf_measure, 2, function(x) length(unique(x)) == 1)
dfr <- Freesurf_measure[, !zv]
n=length(colnames(dfr))
FreeSurf_coor <- cor(dfr[,1:n],use="complete.obs")
# corrplot(FreeSurf_coor, method="circle")
# print(FreeSurf_coor)
highlyCorrelated <- findCorrelation(FreeSurf_coor, cutoff=(0.59),verbose = FALSE)

# According to literature
#  .00-.19 “very weak” 
#  .20-.39 “weak” 
#  .40-.59 “moderate” 
#  .60-.79 “strong” 
#  .80-1.0 “very strong”

# ggpairs(Freesurf_measure)

#print(highlyCorrelated)
important_var = colnames(Freesurf_measure[,-highlyCorrelated])  #get only weak correlation
important_var     # 85 variables

Freesruf_measure2 <- Freesurf_measure[,-highlyCorrelated]
Freesurf_cut <- Freesurf_raw[, -(highlyCorrelated+1)]
str(Freesurf_cut) # 2681 obs. of  88 variables
save(Freesurf_cut, file = "Freesurf_cut.RData")

# Merge data
total_0 <- merge(ClinDia_Patient,Centiloid_raw, by=c("OASISID", "years_to_Visit"), all.y = TRUE)
str(total_0)    # 1914 obs. of  12 variables
summary(total_0)
colnames(total_0)[which(names(total_0) == "days_to_visit.x")] <- "days_to_visit_ClinDia"
colnames(total_0)[which(names(total_0) == "days_to_visit.y")] <- "days_to_visit_Centiloid"
total_1 <- merge(total_0, Freesurf_cut, by=c("OASISID", "years_to_Visit"), all.x = TRUE)
colnames(total_1)[which(names(total_1) == "days_to_visit")] <- "days_to_visit_MR"
str(total_1) #2153 obs. of  209 variables
summary(total_1)
total_2 <- total_1 %>% relocate(days_to_visit_Centiloid, .before = age.at.visit)
total_3 <- total_2 %>% relocate(days_to_visit_MR, .before = age.at.visit)
str(total_3)
save(total_3, file = "total_3.RData")

n_distinct(total_3$OASISID)    # 1004 SUBJECTS
summary(total_3$NORMCOG)
summary(total_3$DEMENTED)
final <- total_3[!is.na(total_3$NORMCOG),]
summary(final$NORMCOG)   # Does the subject have normal cognition: 0 = No, 1 = Yes
summary(final$DEMENTED) # Does the subject meet criteria for dementia?: 0 = No, 1 = Yes, 3 = 1 from NORMCOG (to avoid NA)
save(final, file = "final.RData")
load("final.RData")

#########################################
######### SURV and BP   #################
#########################################

#    Standardized uptake value ratio (SUVR) is the most common quantitative method used to 
# make regional comparisons within a subject as well as between subjects and computed as the 
# degree of radiotracer uptake in a target region of interest with respect to a reference region. 
# In amyloid and tau imaging, SUVR is typically generated using some portion or the entire cerebellum 
# as a reference because cerebellum is not affected until late in the progression of AD.


## Currently, two amyloid imaging tracers are used in our studies, 
# i.e. [11C]-Pittsburgh Compound B (PiB) and [18F]-Florbetapir (AV45). 
# For both tracers, two modeling approaches are implemented: 
# 1) binding potential (BPND) is calculated using Logan graphical analysis 
# (Logan 1996; Mintun 2006; Su 2013, 2015, 2016), when full dynamic PET imaging data are available, 
# i.e. PET acquisition was started in synchronization with tracer administration and PET images 
# were reconstructed into multiple time frames; 2) regional target-to-reference intensity ratio, 
#a.k.a, standard uptake ratio (SUVR), is estimated for all processable PET data.

# both PiB and AV45 have been calibrated to the Centiloid scale for both non-partial volume and 
# partial volume correction (rsf) using standard PUP

summary(final$Centiloid_fBP_TOT_CORTMEAN)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -37.342  -2.246   2.062  17.450  27.275 148.147     781 
summary(final$Centiloid_fSUVR_TOT_CORTMEAN)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -39.6984  -0.9038   7.2218  23.3095  34.5179 203.9508
summary(final$Centiloid_fBP_rsf_TOT_CORTMEAN)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -17.959   1.067   3.816  17.911  25.484 141.207     781
summary(final$Centiloid_fSUVR_rsf_TOT_CORTMEAN)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -18.0616  -0.8688   5.6832  21.5318  30.1137 256.8528 

final_test <- final[!is.na(final$Centiloid_fBP_TOT_CORTMEAN),]

centiloid_coor <- cor(final_test[, c('Centiloid_fBP_TOT_CORTMEAN','Centiloid_fSUVR_TOT_CORTMEAN',
                                'Centiloid_fBP_rsf_TOT_CORTMEAN', 'Centiloid_fSUVR_rsf_TOT_CORTMEAN')])
corrplot(centiloid_coor, method="number")
highlyCorrelated_Centi <- findCorrelation(centiloid_coor, cutoff=(0.59),verbose = FALSE)
highlyCorrelated_Centi 
centiloid_coor    # all the values are highly coorrelated, so can pick only one centiloid. 
# Centiloid_fSUVR_TOT_CORTMEAN will be used

#                                               Centiloid_fBP_TOT_CORTMEAN Centiloid_fSUVR_TOT_CORTMEAN Centiloid_fBP_rsf_TOT_CORTMEAN
# Centiloid_fBP_TOT_CORTMEAN                        1.0000000                    0.9952025                      0.9913208
# Centiloid_fSUVR_TOT_CORTMEAN                      0.9952025                    1.0000000                      0.9848654
# Centiloid_fBP_rsf_TOT_CORTMEAN                    0.9913208                    0.9848654                      1.0000000
# Centiloid_fSUVR_rsf_TOT_CORTMEAN                  0.9890575                    0.9913715                      0.9937697
# Centiloid_fSUVR_rsf_TOT_CORTMEAN
# Centiloid_fBP_TOT_CORTMEAN                              0.9890575
# Centiloid_fSUVR_TOT_CORTMEAN                            0.9913715
# Centiloid_fBP_rsf_TOT_CORTMEAN                          0.9937697
# Centiloid_fSUVR_rsf_TOT_CORTMEAN                        1.0000000

####### Plot to include tracer
summary(final$tracer)
# AV45  PIB 
# 741 1041 
############# Centiloid_fBP_TOT_CORTMEAN #########################
# Violoin plot
ggplot(data = final, mapping = aes(y = Centiloid_fSUVR_TOT_CORTMEAN,                     
                                   x = DEMENTED,                           
                                   color = tracer, fill = tracer)) +                      
  geom_violin()+                                  
  labs(title = "Violin plot by DEMENTED") 

# Box plot by group
ggplot(data = final, mapping = aes(y = Centiloid_fSUVR_TOT_CORTMEAN,                     
                                   x = DEMENTED,                           
                                   color = tracer, fill = tracer)) + 
  geom_boxplot()+                     
  theme(legend.position = "none")+   
  labs(title = "Boxplot by DEMENTED")

# no significant difference in tracer. Tracer does not matter too much
final2 <- final %>% select(-Centiloid_fBP_TOT_CORTMEAN, 
                           -Centiloid_fBP_rsf_TOT_CORTMEAN, 
                           -Centiloid_fSUVR_rsf_TOT_CORTMEAN)
str(final2)    # 1782 obs. of  95 variables
save(final2, file = "final2.RData")
load("final2.RData")
missmap(final2)

#Remove NA
final3 <- final2[, -c(2:6)]
final_NoNA <- na.omit(final3)
str(final_NoNA)       # 1672 obs. of  90 variables
n_distinct(final_NoNA$OASISID)  # 894 subjects
save(final_NoNA, file = "final_NoNA.RData")
load("final_NoNA.RData")
# summary(final_NoNA$X5th.Ventricle_volume)
# summary(final_NoNA$Left.non.WM.hypointensities_volume)
# final4 <- final_NoNA[ , colSums(final_NoNA[, -c(1:4)]) != 0]
# str(final4)


# final_NoNA[4:25] %>% gather() %>%
#   ggplot(aes(x=value)) + 
#   geom_point() +
#   theme_minimal() +
#   facet_wrap(~key, scales="free")
# 
# final_NoNA[4:25] %>% gather() %>%
# ggplot(data = final_NoNA, mapping = aes(x = DEMENTED,                           
#                                         color = DEMENTED, fill = DEMENTED)) + 
#   geom_point(y = value) +
#   facet_wrap(~key, scales="free")

########################################################################################################
############################## Modeling ###############################################################
#######################################################################################################

# Remove OASISID, NORMCOG, AND tracer
final_NoNA_DEMENTED <- final_NoNA[, -c(1:2, 4)]
final_NoNA_DEMENTED2 <- final_NoNA_DEMENTED %>% dplyr::select(-Left.non.WM.hypointensities_volume)
save(final_NoNA_DEMENTED2, file = "final_NoNA_DEMENTED2.RData")
load("final_NoNA_DEMENTED2.RData")
str(final_NoNA_DEMENTED2) #1672 obs. of  86 variables:

set.seed(4365677)
DEMENTED_sampling_vector <- createDataPartition(final_NoNA_DEMENTED2$DEMENTED, p = 0.80, list = FALSE)
DEMENTED_train <- final_NoNA_DEMENTED2[DEMENTED_sampling_vector,]
DEMENTED_test <- final_NoNA_DEMENTED2[-DEMENTED_sampling_vector,]
save(DEMENTED_train, file = "DEMENTED_train.RData")
save(DEMENTED_test, file = "DEMENTED_test.RData")

# normalize the input data
DEMENTED_pp <- preProcess(DEMENTED_train, method = c("range"))
norm_DEMENTED_train <- predict(DEMENTED_pp, DEMENTED_train)
norm_DEMENTED_test <- predict(DEMENTED_pp, DEMENTED_test)
#summary(norm_DEMENTED_train)

###########################################################
############# use a neural network (nnet)##################
###########################################################

# create model
start_time <- proc.time()
DEMENTED_model <- nnet(DEMENTED ~ ., data = norm_DEMENTED_train, size = 10, maxit = 10000, decay =0.01 )
end_time <- proc.time()
end_time - start_time
#summary(DEMENTED_model)

# check accuracy of model
nn_train_predictions <- predict(DEMENTED_model, norm_DEMENTED_train, type = "class")
mean(nn_train_predictions == DEMENTED_train$DEMENTED)
table(DEMENTED_train$DEMENTED, nn_train_predictions)

nn_test_predictions <- predict(DEMENTED_model, norm_DEMENTED_test, type = "class")
mean(nn_test_predictions == DEMENTED_test$DEMENTED)
table(DEMENTED_test$DEMENTED, nn_test_predictions)

# compare different models variables of importance
DEMENTED_model_varImp <- varImp(DEMENTED_model)
DEMENTED_model_varImp_df <- as.data.frame(DEMENTED_model_varImp)
DEMENTED_model_varImp2 <- DEMENTED_model_varImp_df[order(-DEMENTED_model_varImp_df$Overall), ,drop = FALSE]
DEMENTED_model_varImp2$labels <- factor(rownames(DEMENTED_model_varImp2))
DEMENTED_model_varImp2$labels <- reorder(DEMENTED_model_varImp2$labels, DEMENTED_model_varImp2$Overall)
DEMENTED_model_varImp3 <- DEMENTED_model_varImp2[c(1:30), ]
DEMENTED_nn_ImpVar <- rownames(DEMENTED_model_varImp3)

# plot the importance
ggplot(data = DEMENTED_model_varImp3, aes(x = labels,y = Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + 
  geom_point(color='blue') + 
  ylab("Importance Score") +
  xlab("Variables") +
  ggtitle("Feature Importance of Neural Network") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  PlotTheme +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

###################################################
##### Focus on these 10 important factor ##########
###################################################
DEMENTED_model_varImp4 <- DEMENTED_model_varImp2[c(1:10), ]
TopList <- append(DEMENTED_model_varImp4, "DEMENTED")

Imp_col <- final_NoNA %>% dplyr::select(DEMENTED, rownames(DEMENTED_model_varImp4))
str(Imp_col)

set.seed(4365677)
DEMENTED_imp_sampling_vector <- createDataPartition(Imp_col$DEMENTED, p = 0.80, list = FALSE)
DEMENTED_imp_train <- Imp_col[DEMENTED_imp_sampling_vector,]
DEMENTED_imp_test <- Imp_col[-DEMENTED_imp_sampling_vector,]

# normalize our input data
DEMENTED_imp_pp <- preProcess(DEMENTED_imp_train, method = c("range"))
norm_DEMENTED_imp_train <- predict(DEMENTED_imp_pp, DEMENTED_imp_train)
norm_DEMENTED_imp_test <- predict(DEMENTED_imp_pp, DEMENTED_imp_test)

# create model
start_imp_time <- proc.time()
DEMENTED_imp_model <- nnet(DEMENTED ~ ., data = norm_DEMENTED_imp_train, size = 20, maxit = 10000, decay =0.01 )
end_imp_time <- proc.time()
end_imp_time - start_imp_time

# check accuracy of model
train_imp_predictions <- predict(DEMENTED_imp_model, norm_DEMENTED_imp_train, type = "class")
mean(train_imp_predictions == DEMENTED_imp_train$DEMENTED)

test_imp_predictions <- predict(DEMENTED_imp_model, norm_DEMENTED_imp_test, type = "class")
mean(test_imp_predictions == DEMENTED_imp_test$DEMENTED)

varImp(DEMENTED_imp_model)

###########################################################
####################### NavieBayes ########################
###########################################################
nbmodel <- naiveBayes(DEMENTED ~., DEMENTED_train)
# Evaluate training data
nb_train_predicted <- predict(nbmodel, DEMENTED_train)
mean(DEMENTED_train$DEMENTED == nb_train_predicted)
table(nb_train_predicted, DEMENTED_train$DEMENTED)
# Evaluate test data
nb_test_predicted <- predict(nbmodel, DEMENTED_test)
mean(DEMENTED_test$DEMENTED == nb_test_predicted)
table(DEMENTED_test$DEMENTED, nb_test_predicted)


# #create objects x which holds the predictor variables and y which holds the response variables
# x = DEMENTED_train[ , -1]
# y = DEMENTED_train$DEMENTED
# nbmodel2 = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
# X <- varImp(nbmodel)

###########################################################
####################### Random Forest ########################
###########################################################
rfmodel <- randomForest(DEMENTED ~., data = DEMENTED_train, proximity = TRUE) 
print(rfmodel)

# Training data
rf_train_predicted <- predict(rfmodel, DEMENTED_train)
mean(rf_train_predicted == DEMENTED_train$DEMENTED)
confusionMatrix(rf_train_predicted, DEMENTED_train$DEMENTED)

# Test data
rf_test_predicted <- predict(rfmodel, DEMENTED_test)
mean(rf_test_predicted == DEMENTED_test$DEMENTED)
confusionMatrix(rf_test_predicted, DEMENTED_test$DEMENTED)

# Find important factors
DEMENTED_rf_varImp <- varImp(rfmodel)
DEMENTED_rf_varImp2 <- DEMENTED_rf_varImp[order(-DEMENTED_rf_varImp$Overall), , drop = FALSE]
DEMENTED_rf_varImp2$labels <- factor(rownames(DEMENTED_rf_varImp2))
DEMENTED_rf_varImp2$labels <- reorder(DEMENTED_rf_varImp2$labels, DEMENTED_rf_varImp2$Overall)
DEMENTED_rf_varImp3 <- DEMENTED_rf_varImp2[c(1:10), ]
DEMENTED_rf_varImp3
DEMENTED_rf_ImpVar <- rownames(DEMENTED_rf_varImp3)

# plot the importance
ggplot(data= DEMENTED_rf_varImp3, aes(x = labels,y = Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + 
  geom_point(color='blue') + 
  ylab("Importance Score") +
  xlab("Variables") +
  ggtitle("Feature Importance of Random Forest") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  PlotTheme +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

###########################################################
#################### Decision Tree ########################
###########################################################
# penalty matrix
# penalty.matrix <- matrix(c(0,1,10,0), byrow = TRUE, nrow=2)
# building the classification tree with rpart
DEMENTED_tree <- rpart(DEMENTED ~.,
              data = DEMENTED_train,
              method = "class")
#identify best cp value to use
best <- DEMENTED_tree$cptable[which.min(DEMENTED_tree$cptable[,"xerror"]),"CP"]
pruned_DEMENTED_tree <- prune(DEMENTED_tree, cp=best)
# Visualize the decision tree with rpart.plot
prp(pruned_DEMENTED_tree)
#plot decision tree using custom arguments
prp(pruned_DEMENTED_tree,
    faclen = 1, #use full names for factor labels
    extra = 1, #display number of observations for each terminal node
    roundint = F) #don't round to integers in output
#    digits = 3) #display 5 decimal places in output

rpart.plot(pruned_DEMENTED_tree, type = 4)

# ######## Decision Tree test
# # run decision stump model
# ctrl <- list(cp = 0, minbucket = 5, maxdepth = 3)
# fit <- rpart(DEMENTED ~., data = DEMENTED_train, control = ctrl)
# 
# # plot tree 
# par(mar = c(1, 1, 1, 1))
# rpart.plot(fit, type = 4)

#Testing the training model
DEMENTED_dt_train_pred <- predict(object = DEMENTED_tree, DEMENTED_train[-1], type="class")
#Calculating accuracy
mean(DEMENTED_train$DEMENTED == DEMENTED_dt_train_pred)
table(DEMENTED_train$DEMENTED, DEMENTED_dt_train_pred)
confusionMatrix(DEMENTED_train$DEMENTED, DEMENTED_dt_train_pred)

#Testing the test model
DEMENTED_dt_test_pred <- predict(object = DEMENTED_tree, DEMENTED_test[-1], type="class")
#Calculating accuracy
table(DEMENTED_test$DEMENTED, DEMENTED_dt_test_pred)
mean(DEMENTED_test$DEMENTED == DEMENTED_dt_test_pred)
confusionMatrix(DEMENTED_test$DEMENTED, DEMENTED_dt_test_pred)

DEMENTED_dt_varImp <- varImp(DEMENTED_tree)
DEMENTED_dt_varImp2 <- DEMENTED_dt_varImp[order(-DEMENTED_dt_varImp$Overall), , drop = FALSE]
DEMENTED_dt_varImp2$labels <- factor(rownames(DEMENTED_dt_varImp2))
DEMENTED_dt_varImp2$labels <- reorder(DEMENTED_dt_varImp2$labels, DEMENTED_dt_varImp2$Overall)
DEMENTED_dt_varImp3 <- DEMENTED_dt_varImp2[c(1:10), ]
DEMENTED_dt_varImp3
DEMENTED_dt_ImpVar <- rownames(DEMENTED_dt_varImp3)

# plot the importance
ggplot(data= DEMENTED_dt_varImp3, aes(x = labels,y = Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + 
  geom_point(color='blue') + 
  ylab("Importance Score") +
  xlab("Variables") +
  ggtitle("Feature Importance of Decision Tree") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  PlotTheme +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

##################################################################################################
################### post processing ##############################################################
##################################################################################################

ImpVar_Com <- data.frame(unlist(DEMENTED_nn_ImpVar),unlist(DEMENTED_rf_ImpVar), unlist(DEMENTED_dt_ImpVar))
names(ImpVar_Com) <- c("NeuralNetwork","RandomForest", "Decision Tree")
View(ImpVar_Com)
write_xlsx(ImpVar_Com,"Important_Factors_3.xlsx")



##################################################################
################### ROC ##########################################
##################################################################



par(pty="s") 
nnROC <- roc(vdata_Y ~ LR_predict,
             plot=TRUE,print.auc=TRUE,
             col="green",lwd =4,
             legacy.axes=TRUE,
             main="ROC Curves")

################################################
########### Plot Important Variables ###########
################################################

######## TOTAL_HIPPOCAMPUS_VOLUME
# Violin plot
ggplot(data = final_NoNA, mapping = aes(y = TOTAL_HIPPOCAMPUS_VOLUME,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) +                      
  geom_violin()+ 
  geom_jitter() +
  PlotTheme +
  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "TOTAL_HIPPOCAMPUS_VOLUME vs. Cognitive Normality") 
# Box plot by group
ggplot(data = final_NoNA, mapping = aes(y = TOTAL_HIPPOCAMPUS_VOLUME,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) + 
  geom_boxplot()+ 
  geom_jitter() +
  PlotTheme +
#  scale_shape_discrete(labels = c("Not Demented", "Demented", "Normal Cognition")) +
#  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "TOTAL_HIPPOCAMPUS_VOLUME vs. Cognitive Normality")

######## Centiloid_fSUVR_TOT_CORTMEAN
# Violin plot
ggplot(data = final_NoNA, mapping = aes(y = Centiloid_fSUVR_TOT_CORTMEAN,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) +                      
  geom_violin()+ 
  geom_jitter() +
  PlotTheme +
  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Centiloid vs. Cognitive Normality") 
# Box plot by group
final_NoNA$DEMENTED = factor(final_NoNA$DEMENTED, levels=c("3", "0", "1"))
ggplot(data = final_NoNA, mapping = aes(y = Centiloid_fSUVR_TOT_CORTMEAN,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED)) + 
  geom_boxplot()+ 
  #geom_jitter() +
  PlotTheme +
  #  scale_shape_discrete(labels = c("Not Demented", "Demented", "Normal Cognition")) +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("green4", "#E7B800", "#FC4E07")) +
  scale_color_manual(values = c("green4", "#E7B800", "#FC4E07")) +
  LegendTheme +
  labs(title = "Centiloid vs. Cognitive Normality")

######## lh_middletemporal_thickness
# Violin plot
ggplot(data = final_NoNA, mapping = aes(y = lh_middletemporal_thickness,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) +                      
  geom_violin()+ 
  geom_jitter() +
  PlotTheme +
  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Left Hemispehere Middletemporal Thickness vs. Cognitive Normality") 
# Box plot by group
ggplot(data = final_NoNA, mapping = aes(y = lh_middletemporal_thickness,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) + 
  geom_boxplot()+ 
  geom_jitter() +
  PlotTheme +
  #  scale_shape_discrete(labels = c("Not Demented", "Demented", "Normal Cognition")) +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Left Hemispehere Middletemporal Thickness vs. Cognitive Normality")

######## Left.Amygdala_volume
# Violin plot
ggplot(data = final_NoNA, mapping = aes(y = Left.Amygdala_volume,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) +                      
  geom_violin()+ 
  geom_jitter() +
  PlotTheme +
#  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Left Amygdala volume vs. Cognitive Normality") 
# Box plot by group
ggplot(data = final_NoNA, mapping = aes(y = Left.Amygdala_volume,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) + 
  geom_boxplot()+ 
  geom_jitter() +
  PlotTheme +
  #  scale_shape_discrete(labels = c("Not Demented", "Demented", "Normal Cognition")) +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Left Amygdala Volume vs. Cognitive Normality")

######## Right.Amygdala_volume
# Violin plot
ggplot(data = final_NoNA, mapping = aes(y = Right.Amygdala_volume,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) +                      
  geom_violin()+ 
  geom_jitter() +
  PlotTheme +
#  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Right Amygdala Volume vs. Cognitive Normality") 
# Box plot by group
ggplot(data = final_NoNA, mapping = aes(y = Right.Amygdala_volume,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) + 
  geom_boxplot()+ 
  geom_jitter() +
  PlotTheme +
  #  scale_shape_discrete(labels = c("Not Demented", "Demented", "Normal Cognition")) +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Right Amygdala Volume vs. Cognitive Normality")

######## lh_superiortemporal_thickness
# Violin plot
ggplot(data = final_NoNA, mapping = aes(y = lh_superiortemporal_thickness,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) +                      
  geom_violin()+ 
  geom_jitter() +
  PlotTheme +
#  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "left Hemispehere Superiortemporal Thickness vs. Cognitive Normality") 
# Box plot by group
ggplot(data = final_NoNA, mapping = aes(y = lh_superiortemporal_thickness,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) + 
  geom_boxplot()+ 
  geom_jitter() +
  PlotTheme +
  #  scale_shape_discrete(labels = c("Not Demented", "Demented", "Normal Cognition")) +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "left Hemispehere Superiortemporal Thickness vs. Cognitive Normality")

######## rh_fusiform_volume
# Violin plot
ggplot(data = final_NoNA, mapping = aes(y = rh_fusiform_volume,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) +                      
  geom_violin()+ 
  geom_jitter() +
  PlotTheme +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "rh_fusiform_volume vs. Cognitive Normality") 
# Box plot by group
ggplot(data = final_NoNA, mapping = aes(y = rh_fusiform_volume,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) + 
  geom_boxplot()+ 
  geom_jitter() +
  PlotTheme +
  #  scale_shape_discrete(labels = c("Not Demented", "Demented", "Normal Cognition")) +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "rh_fusiform_volume vs. Cognitive Normality")

######## Left.Accumbens.area_volume
# Violin plot
ggplot(data = final_NoNA, mapping = aes(y = Left.Accumbens.area_volume,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) +                      
  geom_violin()+ 
  geom_jitter() +
  PlotTheme +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Left Accumbens Area_volume vs. Cognitive Normality") 
# Box plot by group
ggplot(data = final_NoNA, mapping = aes(y = Left.Accumbens.area_volume,                     
                                        x = DEMENTED,                           
                                        color = DEMENTED, fill = DEMENTED)) + 
  geom_boxplot()+ 
  geom_jitter() +
  PlotTheme +
  #  scale_shape_discrete(labels = c("Not Demented", "Demented", "Normal Cognition")) +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Left Accumbens Area_volume vs. Cognitive Normality")


#############################################################################################################
############################ RFE  ###########################################################################
#############################################################################################################

####################################################
#######################Random Forest ###############
####################################################

control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

# DEMENTED_train <- final_NoNA_DEMENTED2[DEMENTED_sampling_vector,]
# DEMENTED_test <- final_NoNA_DEMENTED2[-DEMENTED_sampling_vector,]
x_DEMENTED_train <- DEMENTED_train[, -1]
y_DEMENTED_train <- DEMENTED_train[, 1]

x_DEMENTED_test <- DEMENTED_test[, -1]
y_DEMENTED_test <- DEMENTED_test[, 1]

DEMENTED_rfe1 <- rfe(x = x_DEMENTED_train, 
                   y = y_DEMENTED_train, 
                   sizes = c(1:13),
                   rfeControl = control)
DEMENTED_rfe1 
# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (10 fold, repeated 5 times) 
# 
# Resampling performance over subset size:
#   
#   Variables Accuracy  Kappa AccuracySD KappaSD Selected
# 1   0.8420 0.2728    0.02652 0.09272         
# 2   0.8978 0.4719    0.01505 0.08511         
# 3   0.9011 0.4822    0.01545 0.08780         
# 4   0.9030 0.5004    0.01547 0.08344         
# 5   0.9046 0.5064    0.01469 0.08221         
# 6   0.9048 0.5031    0.01626 0.08724         
# 7   0.9066 0.5091    0.01650 0.09378         
# 8   0.9073 0.5061    0.01678 0.09383        *
#   9   0.9069 0.5101    0.01746 0.09698         
# 10   0.9060 0.5049    0.01649 0.09208         
# 11   0.9057 0.5003    0.01694 0.09511         
# 12   0.9055 0.4947    0.01687 0.10099         
# 13   0.9058 0.4926    0.01704 0.10420         
# 85   0.9006 0.4059    0.01411 0.10847         
# 
# The top 5 variables (out of 8):
#   Centiloid_fSUVR_TOT_CORTMEAN, TOTAL_HIPPOCAMPUS_VOLUME, Left.Amygdala_volume, 
# lh_middletemporal_thickness, Right.Inf.Lat.Vent_volume

predictors(DEMENTED_rfe1)
# [1] "Centiloid_fSUVR_TOT_CORTMEAN"  "TOTAL_HIPPOCAMPUS_VOLUME"      "Left.Amygdala_volume"         
# [4] "lh_middletemporal_thickness"   "Right.Inf.Lat.Vent_volume"     "Right.Amygdala_volume"        
# [7] "lh_inferiortemporal_thickness" "Left.Accumbens.area_volume" 

confusionMatrix(DEMENTED_rfe1)

ggplot(data = DEMENTED_rfe1, metric = "Accuracy") + theme_bw()
ggplot(data = DEMENTED_rfe1, metric = "Kappa") + theme_bw()

# about Kappa
# Landis and Koch (1977) provide a way to characterize values. 
# According to their scheme a value
# < 0 is indicating no agreement ,
# 0–0.20 as slight, 
# 0.21–0.40 as fair, 
# 0.41–0.60 as moderate, 
# 0.61–0.80 as substantial, 
# and 0.81–1 as almost perfect agreement.

DEMENTED_varimp_data <- data.frame(feature = row.names(varImp(DEMENTED_rfe1))[1:8],
                          importance = varImp(DEMENTED_rfe1)[1:8, 1])
save(DEMENTED_varimp_data, file = "DEMENTED_varimp_data.RData")

ggplot(data = DEMENTED_varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")

postResample(predict(DEMENTED_rfe1, x_DEMENTED_test), y_DEMENTED_test)
# Accuracy     Kappa 
# 0.8892216 0.4384769 


DEMENTED_rfe2 <- rfe(x = x_DEMENTED_train, 
                     y = y_DEMENTED_train, 
                     sizes = c(1:20, 30, 40, 50, 60, 80),
                     rfeControl = control)
DEMENTED_rfe2

# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (10 fold, repeated 5 times) 
# 
# Resampling performance over subset size:
#   
#   Variables Accuracy  Kappa AccuracySD KappaSD Selected
# 1   0.8453 0.2797    0.01966 0.08577         
# 2   0.8997 0.4748    0.01685 0.10677         
# 3   0.9002 0.4829    0.02056 0.11518         
# 4   0.9006 0.4906    0.01938 0.11454         
# 5   0.9034 0.4978    0.01651 0.10521         
# 6   0.9055 0.5013    0.01545 0.10388         
# 7   0.9058 0.4958    0.01686 0.11254         
# 8   0.9049 0.4912    0.01385 0.09574         
# 9   0.9051 0.4969    0.01527 0.09976         
# 10   0.9054 0.4980    0.01699 0.10875         
# 11   0.9049 0.4898    0.01638 0.10835         
# 12   0.9040 0.4836    0.01451 0.09435         
# 13   0.9043 0.4825    0.01569 0.10405         
# 14   0.9048 0.4843    0.01575 0.10449         
# 15   0.9051 0.4833    0.01579 0.10461         
# 16   0.9061 0.4930    0.01607 0.10474         
# 17   0.9066 0.4947    0.01573 0.10370        *
#   18   0.9045 0.4814    0.01452 0.09667         
# 19   0.9058 0.4872    0.01465 0.09825         
# 20   0.9057 0.4831    0.01472 0.10074         
# 30   0.9037 0.4621    0.01544 0.10653         
# 40   0.9030 0.4541    0.01436 0.10413         
# 50   0.9027 0.4464    0.01634 0.11454         
# 60   0.9018 0.4290    0.01329 0.09880         
# 80   0.8990 0.3934    0.01379 0.11304         
# 85   0.9000 0.4039    0.01363 0.10330         
# 
# The top 5 variables (out of 17):
#   Centiloid_fSUVR_TOT_CORTMEAN, TOTAL_HIPPOCAMPUS_VOLUME, Left.Amygdala_volume, 
# lh_middletemporal_thickness, Right.Amygdala_volume

save(DEMENTED_rfe2, file = "DEMENTED_rfe2.RData")

ggplot(data = DEMENTED_rfe2, metric = "Accuracy") + theme_bw()
DEMENTED_rfe2_df <- as.data.frame(DEMENTED_rfe2)

DEMENTED_varimp_data2 <- data.frame(feature = row.names(varImp(DEMENTED_rfe2))[1:10],
                                   importance = varImp(DEMENTED_rfe2)[1:10, 1])
DEMENTED_varimp_data2
# Overall
# Centiloid_fSUVR_TOT_CORTMEAN      17.990925
# TOTAL_HIPPOCAMPUS_VOLUME          13.690887
# Left.Amygdala_volume               8.493141
# lh_middletemporal_thickness        8.150358
# Right.Amygdala_volume              7.062642
# Right.Inf.Lat.Vent_volume          7.034519
# lh_inferiortemporal_thickness      6.569979
# Left.Accumbens.area_volume         6.537992
# lh_lateralorbitofrontal_volume     6.160786
# lh_lateralorbitofrontal_thickness  6.100141
# rh_inferiortemporal_thickness      5.813300
# lh_superiortemporal_thickness      5.702618
# lh_parahippocampal_volume          5.475015
# rh_inferiorparietal_thickness      5.433586
# lh_lateraloccipital_thickness      5.117261
# lh_fusiform_thickness              5.051513
# SubCortGrayVol                     4.911731
# lh_supramarginal_thickness         4.851681
# lh_temporalpole_volume             4.850936
# lh_medialorbitofrontal_thickness   4.774478
# lh_caudalmiddlefrontal_volume      4.689674
# Right.Thalamus.Proper_volume       4.653768
# rh_cuneus_thickness                4.629022
# Right.Putamen_volume               4.627973
# rh_parsopercularis_thickness       4.491856
# lh_temporalpole_thickness          4.488446
# Left.Cerebellum.Cortex_volume      4.477741
# CC_Posterior_volume                4.424581
# rh_superiortemporal_volume         4.374137
# rh_bankssts_thickness              4.359301
# lh_precentral_thickness            4.065789

save(DEMENTED_varimp_data2, file = "DEMENTED_varimp_data2.RData")

postResample(predict(DEMENTED_rfe2, x_DEMENTED_test), y_DEMENTED_test)

# Accuracy     Kappa 
# 0.8982036 0.4650462 

# plot the importance
ggplot(data = DEMENTED_varimp_data2, 
       aes(x = reorder(feature, importance), y = importance, fill = feature)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(x = "Features", y = "Variable Importance") + 
  #geom_text(aes(label = round(importance, 2)), vjust = 0.5, color="white", size = 4) + 
  theme_bw() + 
  coord_flip() +
  theme(legend.position = "none") +
  PlotTheme

####################################################
####################### TreeBag model ##############
####################################################

treeBagcontrol <- rfeControl(functions = treebagFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

DEMENTED_rfe_treeBag <- rfe(x = x_DEMENTED_train, 
                            y = y_DEMENTED_train, 
                            sizes = c(1:20, 30, 40, 50, 60, 80),
                            rfeControl = treeBagcontrol)
DEMENTED_rfe_treeBag
# Recursive feature selection

# Outer resampling method: Cross-Validated (10 fold, repeated 5 times) 
# 
# Resampling performance over subset size:
#   
#   Variables Accuracy  Kappa AccuracySD KappaSD Selected
# 1   0.8450 0.2708    0.02316 0.10415         
# 2   0.8849 0.4294    0.02151 0.10974         
# 3   0.8961 0.4822    0.01710 0.08828         
# 4   0.8958 0.4813    0.01725 0.09184         
# 5   0.8943 0.4785    0.02084 0.09655         
# 6   0.8966 0.4913    0.01937 0.09934         
# 7   0.8955 0.4822    0.02048 0.10695         
# 8   0.8967 0.4833    0.01975 0.10153         
# 9   0.8987 0.4996    0.01943 0.10524         
# 10   0.8963 0.4860    0.01856 0.10000         
# 11   0.8996 0.5012    0.01847 0.09953         
# 12   0.9002 0.5041    0.02023 0.10334         
# 13   0.9000 0.4937    0.01804 0.09546         
# 14   0.9002 0.5009    0.01895 0.10226         
# 15   0.9000 0.4995    0.01945 0.09866         
# 16   0.9014 0.5057    0.02049 0.10695         
# 17   0.8984 0.4850    0.01933 0.10485         
# 18   0.9024 0.5018    0.01853 0.10264        *
#   19   0.9014 0.5066    0.01816 0.10035         
# 20   0.9006 0.4982    0.01752 0.09918         
# 30   0.9021 0.5008    0.01778 0.10122         
# 40   0.9009 0.4927    0.01655 0.10429         
# 50   0.8981 0.4818    0.01742 0.10667         
# 60   0.8994 0.4846    0.01823 0.10577         
# 80   0.8996 0.4948    0.01879 0.10619         
# 85   0.9002 0.4933    0.01769 0.09851         
# 
# The top 5 variables (out of 18):
#   Centiloid_fSUVR_TOT_CORTMEAN, TOTAL_HIPPOCAMPUS_VOLUME, Left.Amygdala_volume, 
# Right.Amygdala_volume, lh_middletemporal_thickness

save(DEMENTED_rfe_treeBag, file = "DEMENTED_rfe_treeBag.RData")

predictors(DEMENTED_rfe_treeBag)
# [1] "Centiloid_fSUVR_TOT_CORTMEAN"  "TOTAL_HIPPOCAMPUS_VOLUME"      "Left.Amygdala_volume"         
# [4] "Right.Amygdala_volume"         "lh_middletemporal_thickness"   "Left.Accumbens.area_volume"   
# [7] "CC_Posterior_volume"           "Right.Inf.Lat.Vent_volume"     "X4th.Ventricle_volume"        
# [10] "SubCortGrayVol"                "CSF_volume"                    "lh_superiortemporal_thickness"
# [13] "rh_inferiortemporal_thickness" "lh_fusiform_thickness"         "lh_lateraloccipital_thickness"
# [16] "Left.Cerebellum.Cortex_volume" "X5th.Ventricle_volume"         "Right.Thalamus.Proper_volume" 

confusionMatrix(DEMENTED_rfe_treeBag)
# Cross-Validated (10 fold, repeated 5 times) Confusion Matrix 
# 
# (entries are percentual average cell counts across resamples)
# 
# Reference
# Prediction    0    1    3
# 0  0.6  0.2  0.4
# 1  1.0  4.7  1.4
# 3  2.7  4.1 84.9
# 
# Accuracy (average) : 0.9024

ggplot(data = DEMENTED_rfe_treeBag, metric = "Accuracy") + theme_bw()
ggplot(data = DEMENTED_rfe_treeBag, metric = "Kappa") + theme_bw()

DEMENTED_treeBag_varimp_data <- data.frame(feature = row.names(varImp(DEMENTED_rfe_treeBag))[1:18],
                                   importance = varImp(DEMENTED_rfe_treeBag)[1:18, 1])
save(DEMENTED_treeBag_varimp_data, file = "DEMENTED_treeBag_varimp_data.RData")

postResample(predict(DEMENTED_rfe_treeBag, x_DEMENTED_test), y_DEMENTED_test)
# Accuracy     Kappa 
# 0.8802395 0.3787202  

# plot the importance
ggplot(data = DEMENTED_treeBag_varimp_data, 
       aes(x = reorder(feature, importance), y = importance, fill = feature)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(x = "Features", y = "Variable Importance") + 
  #geom_text(aes(label = round(importance, 2)), vjust = 0.5, color="white", size = 4) + 
  theme_bw() + 
  coord_flip() +
  theme(legend.position = "none") +
  PlotTheme




