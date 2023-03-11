############################################# File 5.5 ##################################################
################### Machine learning for the patients that have progression
###### data used for ML is from File 5: "ClinDia_Prog_Centi_FreeSurf.RData"
###### machine learning with Full FreeSurfer varibales
###### machine learning with partial freesufer variables identified from File 1 and same as File 2
###### Output: CN, DMN and Prog (CN_MCI, CN_MCI_DMN, CN_DMN)
      ### if a patient has progression, all the visits will be marked as "Prog"
      ### included all the visit for each patient
      ### MCI, MCI_DMN and DMN are excluded

load("ClinDia_Prog_Centi_FreeSurf.RData")
VisitProgCN <- filter(ClinDia_Prog_Centi_FreeSurf, DementedLetter == "CN" & grepl("CN", ProgStatus))
VisitProgDMN <- filter(ClinDia_Prog_Centi_FreeSurf, ProgStatus == "DMN")
VisitProg <- rbind(VisitProgCN, VisitProgDMN)
str(VisitProg)
VisitProg <- VisitProg %>% dplyr::select(-Left.non.WM.hypointensities_volume)
VisitProg$ProgStatus <- as.factor(as.character(VisitProg$ProgStatus))   # to get rid of extra factor
table(VisitProg$ProgStatus)
save(VisitProg, file = "VisitProg.RData")
load("VisitProg.RData")

VisitProg2 <- VisitProg[, -(1:3)]
VisitProg2 <- VisitProg2[, -3]
summary(VisitProg2$ProgStatus)
VisitProg2$ProgStatus <- as.factor(ifelse(VisitProg2$ProgStatus == 'CN', 'CN', 
                                          ifelse(VisitProg2$ProgStatus == 'DMN', 'DMN', 'Prog')))
save(VisitProg2, file = "VisitProg2.RData")
load("VisitProg2.RData")

###############################################################################################################
####################### Modeling: full Variables
###############################################################################################################

####### parittion Data
set.seed(4365677)
VisitProg_sampling_vector <- createDataPartition(VisitProg2$ProgStatus, p = 0.80, list = FALSE)
VisitProg_train <- VisitProg2[VisitProg_sampling_vector,]
VisitProg_test <- VisitProg2[-VisitProg_sampling_vector,]
save(VisitProg_train, file = "VisitProg_train.RData")
save(VisitProg_test, file = "VisitProg_test.RData")


# normalize the input data
VisitProg_pp <- preProcess(VisitProg_train, method = c("range"))
norm_VisitProg_train <- predict(VisitProg_pp, VisitProg_train)
norm_VisitProg_test <- predict(VisitProg_pp, VisitProg_test)

###########################################################
############# use a neural network (nnet)##################
###########################################################

nn_VisitProg_model <- nnet(ProgStatus ~ ., data = norm_VisitProg_train, size = 10, maxit = 10000, decay =0.01 )

# check accuracy of model
nn_VisitProg_train_predictions <- predict(nn_VisitProg_model, norm_VisitProg_train, type = "class")
mean(nn_VisitProg_train_predictions == VisitProg_train$ProgStatus)    # 1
table(nn_VisitProg_train_predictions, VisitProg_train$ProgStatus)
# nn_VisitProg_train_predictions   CN  DMN Prog
#                           CN   1047    0    0
#                           DMN     0  101    0
#                           Prog    0    0  220

nn_VisitProg_test_predictions <- predict(nn_VisitProg_model, norm_VisitProg_test, type = "class")
mean(nn_VisitProg_test_predictions == norm_VisitProg_test$ProgStatus)  # 0.8588235
table(nn_VisitProg_test_predictions, norm_VisitProg_test$ProgStatus)
#nn_VisitProg_test_predictions  CN DMN Prog
#                         CN   246   5   20
#                         DMN    3  15    3
#                         Prog  12   5   31

nn_VisitProg_varImp <- varImp(nn_VisitProg_model)
nn_VisitProg_varImp_df <- as.data.frame(nn_VisitProg_varImp)
nn_VisitProg_varImp2 <- nn_VisitProg_varImp_df[order(-nn_VisitProg_varImp_df$Overall), ,drop = FALSE]
nn_VisitProg_varImp2$labels <- factor(rownames(nn_VisitProg_varImp2))
nn_VisitProg_varImp2$labels <- reorder(nn_VisitProg_varImp2$labels, nn_VisitProg_varImp2$Overall)
nn_VisitProg_varImp3 <- nn_VisitProg_varImp2[c(1:10), ]
nn_VisitProg_ImpVar <- rownames(nn_VisitProg_varImp3)

# plot the importance
ggplot(data = nn_VisitProg_varImp3, aes(x = labels,y = Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + 
  geom_point(color='blue') + 
  ylab("Importance Score") +
  xlab("Variables") +
  ggtitle("Feature Importance of Neural Network for Progression from CN") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  PlotTheme +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))


###########################################################
####################### Random Forest ########################
###########################################################
rf_VisitProg_model <- randomForest(ProgStatus ~ ., data = VisitProg_train, proximity = TRUE) 
print(rf_VisitProg_model)

# Call:
# randomForest(formula = ProgStatus ~ ., data = VisitProg_train,      proximity = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 9
# 
# OOB estimate of  error rate: 14.33%
# Confusion matrix:
#   CN DMN Prog class.error
# CN   1043   4    0 0.003820439
# DMN    62  39    0 0.613861386
# Prog  129   1   90 0.590909091

# Training data
rf_VisitProg_train_predicted <- predict(rf_VisitProg_model, VisitProg_train)
mean(rf_VisitProg_train_predicted  == VisitProg_train$ProgStatus)    # 1
confusionMatrix(rf_VisitProg_train_predicted, VisitProg_train$ProgStatus)
#               Reference
# Prediction   CN  DMN Prog
#       CN   1047    0    0
#       DMN     0  101    0
#       Prog    0    0  220

# Test data
rf_VisitProg_test_predicted <- predict(rf_VisitProg_model, VisitProg_test)
mean(rf_VisitProg_test_predicted  == VisitProg_test$ProgStatus)    # 0.8529412
confusionMatrix(rf_VisitProg_test_predicted, VisitProg_test$ProgStatus)
#               Reference
# Prediction  CN DMN Prog
#       CN   260  14   33
#       DMN    1  10    1
#       Prog   0   1   20

# Find important factors
rf_VisitProg_varImp <- varImp(rf_VisitProg_model)
rf_VisitProg_varImp2 <- rf_VisitProg_varImp[order(-rf_VisitProg_varImp$Overall), , drop = FALSE]
rf_VisitProg_varImp2$labels <- factor(rownames(rf_VisitProg_varImp2))
rf_VisitProg_varImp2$labels <- reorder(rf_VisitProg_varImp2$labels, rf_VisitProg_varImp2$Overall)
rf_VisitProg_varImp3 <- rf_VisitProg_varImp2[c(1:10), ]
rf_VisitProg_varImp3
rf_VisitProg_ImpVar <- rownames(rf_VisitProg_varImp3)

# plot the importance
ggplot(data= rf_VisitProg_varImp3, aes(x = labels,y = Overall)) +
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
tree_VisitProg_model <- rpart(ProgStatus ~ ., data = VisitProg_train, method = "class")
#identify best cp value to use
dt_VisitProg_best <- tree_VisitProg_model$cptable[which.min(tree_VisitProg_model$cptable[,"xerror"]),"CP"]
pruned_VisitProg_tree <- prune(tree_VisitProg_model, cp = dt_VisitProg_best)
# Visualize the decision tree with rpart.plot
prp(pruned_VisitProg_tree)
#plot decision tree using custom arguments
prp(pruned_VisitProg_tree,
    faclen = 1, #use full names for factor labels
    extra = 1, #display number of observations for each terminal node
    roundint = F) #don't round to integers in output
#    digits = 3) #display 5 decimal places in output

rpart.plot(pruned_VisitProg_tree, type = 4)

#Testing the training model
tree_VisitProg_train_pred <- predict(object = tree_VisitProg_model, VisitProg_train[-1], type="class")
#Calculating accuracy
mean(VisitProg_train$ProgStatus == tree_VisitProg_train_pred) # 0.870614
table(VisitProg_train$ProgStatus, tree_VisitProg_train_pred)
# tree_VisitProg_train_pred
#        CN  DMN Prog
# CN   1022   12   13
# DMN    29   63    9
# Prog  108    6  106
confusionMatrix(VisitProg_train$ProgStatus, tree_VisitProg_train_pred)


#Testing the test model
tree_VisitProg_test_pred <- predict(object = tree_VisitProg_model, VisitProg_test[-1], type="class")
#Calculating accuracy
mean(VisitProg_test$ProgStatus == tree_VisitProg_test_pred)    # 0.8294118
table(VisitProg_test$ProgStatus, tree_VisitProg_test_pred)
# tree_VisitProg_test_pred
#       CN DMN Prog
# CN   252   6    3
# DMN    8  14    3
# Prog  32   6   16
confusionMatrix(VisitProg_test$ProgStatus, tree_VisitProg_test_pred)

tree_VisitProg_varImp <- varImp(tree_VisitProg_model)
tree_VisitProg_varImp2 <- tree_VisitProg_varImp[order(-tree_VisitProg_varImp$Overall), , drop = FALSE]
tree_VisitProg_varImp2$labels <- factor(rownames(tree_VisitProg_varImp2))
tree_VisitProg_varImp2$labels <- reorder(tree_VisitProg_varImp2$labels, tree_VisitProg_varImp2$Overall)
tree_VisitProg_varImp3 <- tree_VisitProg_varImp2[c(1:10), ]
tree_VisitProg_varImp3
tree_VisitProg_ImpVar <- rownames(tree_VisitProg_varImp3)

# plot the importance
ggplot(data = tree_VisitProg_varImp3, aes(x = labels,y = Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + 
  geom_point(color='blue') + 
  ylab("Importance Score") +
  xlab("Variables") +
  ggtitle("Feature Importance of Decision Tree") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  PlotTheme +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

############################################
################### post processing ########
############################################

VisitProg_ImpVar_Com <- data.frame(unlist(nn_VisitProg_ImpVar),
                                 unlist(rf_VisitProg_ImpVar), 
                                 unlist(tree_VisitProg_ImpVar))
names(VisitProg_ImpVar_Com) <- c("NeuralNetwork","RandomForest", "Decision Tree")
View(VisitProg_ImpVar_Com)
write_xlsx(VisitProg_ImpVar_Com,"Important_Factors_VisitProg.xlsx")


###############################################################################################################
####################### Modeling: Progression from CN to MCI and/or DMN (top 8 variable)
###############################################################################################################
ImpVarList <- c('ProgStatus', 'Centiloid_fSUVR_TOT_CORTMEAN','TOTAL_HIPPOCAMPUS_VOLUME', 'Left.Amygdala_volume',
                'Right.Amygdala_volume', 'lh_middletemporal_thickness', 'Left.Accumbens.area_volume', 
                'lh_temporalpole_thickness', 'lh_superiortemporal_thickness', 'Right.Inf.Lat.Vent_volume')

VisitProg_Top8_train <- VisitProg_train %>% dplyr::select(ImpVarList)
VisitProg_Top8_test <- VisitProg_test %>% dplyr::select(ImpVarList)
norm_VisitProg_Top8_train <- norm_VisitProg_train %>% dplyr::select(ImpVarList)
norm_VisitProg_Top8_test <- norm_VisitProg_test %>% dplyr::select(ImpVarList)

###########################################################
############# neural network (nnet)##################
###########################################################

nn_VisitProg_Top8_model <- nnet(ProgStatus ~ ., 
                                data = norm_VisitProg_Top8_train, 
                                size = 10, 
                                maxit = 10000, 
                                decay =0.01 )
# check accuracy of model
nn_VisitProg_train_Top8_predictions <- predict(nn_VisitProg_Top8_model, 
                                               norm_VisitProg_Top8_train, 
                                               type = "class")
mean(nn_VisitProg_train_Top8_predictions == VisitProg_Top8_train$ProgStatus)   # 0.8611111
table(nn_VisitProg_train_Top8_predictions, VisitProg_Top8_train$ProgStatus)
# nn_VisitProg_train_Top8_predictions   CN  DMN Prog
#                                CN   1001   32  104
#                                DMN    15   65    4
#                                Prog   31    4  112
table(nn_VisitProg_train_Top8_predictions)
# nn_VisitProg_train_Top8_predictions
#   CN  DMN Prog 
# 1166   83  119

nn_VisitProg_test_Top8_predictions <- predict(nn_VisitProg_Top8_model, 
                                              norm_VisitProg_Top8_test, 
                                              type = "class")
mean(nn_VisitProg_test_Top8_predictions == VisitProg_Top8_test$ProgStatus)    #0.8235294
table(nn_VisitProg_test_Top8_predictions, VisitProg_Top8_test$ProgStatus)
# nn_VisitProg_test_Top8_predictions  CN DMN Prog
#                               CN   253   8   35
#                               DMN    3  13    5
#                               Prog   5   4   14

nn_VisitProg_Top8_varImp <- varImp(nn_VisitProg_Top8_model)
nn_VisitProg_Top8_varImp <- as.data.frame(nn_VisitProg_Top8_varImp)
nn_VisitProg_Top8_varImp2 <- nn_VisitProg_Top8_varImp[order(-nn_VisitProg_Top8_varImp$Overall), , drop = FALSE]
nn_VisitProg_Top8_varImp2$labels <- factor(rownames(nn_VisitProg_Top8_varImp2))
nn_VisitProg_Top8_varImp2$labels <- reorder(nn_VisitProg_Top8_varImp2$labels, nn_VisitProg_Top8_varImp2$Overall)
nn_VisitProg_Top8_ImpVar <- rownames(nn_VisitProg_Top8_varImp2)

# plot the importance
ggplot(data = nn_VisitProg_Top8_varImp2, aes(x = labels,y = Overall)) +
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
####################### Random Forest ########################
###########################################################
rf_VisitProg_Top8_model <- randomForest(ProgStatus ~ ., data = VisitProg_Top8_train, proximity = TRUE) 
print(rf_VisitProg_Top8_model )
# Call:
#   randomForest(formula = ProgStatus ~ ., data = VisitProg_Top8_train,      proximity = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3
# 
# OOB estimate of  error rate: 14.47%
# Confusion matrix:
#   CN DMN Prog class.error
# CN   1018  12   17  0.02769819
# DMN    42  51    8  0.49504950
# Prog  115   4  101  0.54090909

# Training data
rf_VisitProg_Top8_train_predicted <- predict(rf_VisitProg_Top8_model, VisitProg_Top8_train)
mean(rf_VisitProg_Top8_train_predicted  == VisitProg_Top8_train$ProgStatus)   # 1
confusionMatrix(rf_VisitProg_Top8_train_predicted, VisitProg_Top8_train$ProgStatus)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   CN  DMN Prog
# CN   1047    0    0
# DMN     0  101    0
# Prog    0    0  220
# 
# Overall Statistics
# 
# Accuracy : 1          
# 95% CI : (0.9973, 1)
# No Information Rate : 0.7654     
# P-Value [Acc > NIR] : < 2.2e-16  
# 
# Kappa : 1          
# 
# Mcnemar's Test P-Value : NA         
# 
# Statistics by Class:
# 
#                      Class: CN Class: DMN Class: Prog
# Sensitivity             1.0000    1.00000      1.0000
# Specificity             1.0000    1.00000      1.0000
# Pos Pred Value          1.0000    1.00000      1.0000
# Neg Pred Value          1.0000    1.00000      1.0000
# Prevalence              0.7654    0.07383      0.1608
# Detection Rate          0.7654    0.07383      0.1608
# Detection Prevalence    0.7654    0.07383      0.1608
# Balanced Accuracy       1.0000    1.00000      1.0000

# Test data
rf_VisitProg_Top8_test_predicted <- predict(rf_VisitProg_Top8_model, VisitProg_Top8_test)
mean(rf_VisitProg_Top8_test_predicted  == VisitProg_Top8_test$ProgStatus)   # 0.85
confusionMatrix(rf_VisitProg_Top8_test_predicted, VisitProg_Top8_test$ProgStatus)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  CN DMN Prog
# CN   252   6   29
# DMN    1  15    3
# Prog   8   4   22
# 
# Overall Statistics
# 
# Accuracy : 0.85            
# 95% CI : (0.8075, 0.8862)
# No Information Rate : 0.7676          
# P-Value [Acc > NIR] : 0.0001111       
# 
# Kappa : 0.5482          
# 
# Mcnemar's Test P-Value : 0.0013482       
# 
# Statistics by Class:
# 
#                      Class: CN Class: DMN Class: Prog
# Sensitivity             0.9655    0.60000     0.40741
# Specificity             0.5570    0.98730     0.95804
# Pos Pred Value          0.8780    0.78947     0.64706
# Neg Pred Value          0.8302    0.96885     0.89542
# Prevalence              0.7676    0.07353     0.15882
# Detection Rate          0.7412    0.04412     0.06471
# Detection Prevalence    0.8441    0.05588     0.10000
# Balanced Accuracy       0.7612    0.79365     0.68272


###########################################################
#################### Decision Tree ########################
###########################################################

tree_VisitProg_Top8_model <- rpart(ProgStatus ~ ., data = VisitProg_Top8_train, method = "class")
#identify best cp value to use
dt_VisitProg_Top8_best <- tree_VisitProg_Top8_model$cptable[which.min(tree_VisitProg_Top8_model$cptable[,"xerror"]),"CP"]
pruned_VisitProg_Top8_tree <- prune(tree_VisitProg_Top8_model, dt_VisitProg_Top8_best)
# Visualize the decision tree with rpart.plot
prp(pruned_VisitProg_Top8_tree)
#plot decision tree using custom arguments
prp(pruned_VisitProg_Top8_tree,
    faclen = 1, #use full names for factor labels
    extra = 1, #display number of observations for each terminal node
    roundint = F) #don't round to integers in output
#    digits = 3) #display 5 decimal places in output

rpart.plot(pruned_VisitProg_Top8_tree, type = 4)

#Testing the training model
tree_VisitProg_train_Top8_pred <- predict(object = tree_VisitProg_Top8_model, VisitProg_Top8_train[-1], type="class")
#Calculating accuracy
mean(VisitProg_Top8_train$ProgStatus == tree_VisitProg_train_Top8_pred)   # 0.8472222
table(VisitProg_Top8_train$ProgStatus, tree_VisitProg_train_Top8_pred)
# tree_VisitProg_train_Top8_pred
#        CN  DMN Prog
# CN   1014    1   32
# DMN    42   44   15
# Prog  118    1  101
confusionMatrix(VisitProg_Top8_train$ProgStatus, tree_VisitProg_train_Top8_pred)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   CN  DMN Prog
# CN   1014    1   32
# DMN    42   44   15
# Prog  118    1  101
# 
# Overall Statistics
# 
# Accuracy : 0.8472         
# 95% CI : (0.827, 0.8659)
# No Information Rate : 0.8582         
# P-Value [Acc > NIR] : 0.8844         
# 
# Kappa : 0.5274         
# 
# Mcnemar's Test P-Value : <2e-16         
# 
# Statistics by Class:
# 
#                      Class: CN Class: DMN Class: Prog
# Sensitivity             0.8637    0.95652     0.68243
# Specificity             0.8299    0.95688     0.90246
# Pos Pred Value          0.9685    0.43564     0.45909
# Neg Pred Value          0.5016    0.99842     0.95906
# Prevalence              0.8582    0.03363     0.10819
# Detection Rate          0.7412    0.03216     0.07383
# Detection Prevalence    0.7654    0.07383     0.16082

tree_VisitProg_test_Top8_pred <- predict(object = tree_VisitProg_Top8_model, VisitProg_Top8_test[-1], type="class")
#Calculating accuracy
mean(VisitProg_Top8_test$ProgStatus == tree_VisitProg_test_Top8_pred)  # 0.8147059
table(VisitProg_Top8_test$ProgStatus, tree_VisitProg_test_Top8_pred)
# tree_VisitProg_test_Top8_pred
#       CN DMN Prog
# CN   249   3    9
# DMN   13   8    4
# Prog  30   4   20
confusionMatrix(VisitProg_Top8_test$ProgStatus, tree_VisitProg_test_Top8_pred)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  CN DMN Prog
# CN   249   3    9
# DMN   13   8    4
# Prog  30   4   20
# 
# Overall Statistics
# 
# Accuracy : 0.8147          
# 95% CI : (0.7693, 0.8546)
# No Information Rate : 0.8588          
# P-Value [Acc > NIR] : 0.9902618       
# 
# Kappa : 0.4247          
# 
# Mcnemar's Test P-Value : 0.0005426       
# 
# Statistics by Class:
# 
#                      Class: CN Class: DMN Class: Prog
# Sensitivity             0.8527    0.53333     0.60606
# Specificity             0.7500    0.94769     0.88925
# Pos Pred Value          0.9540    0.32000     0.37037
# Neg Pred Value          0.4557    0.97778     0.95455
# Prevalence              0.8588    0.04412     0.09706
# Detection Rate          0.7324    0.02353     0.05882
# Detection Prevalence    0.7676    0.07353     0.15882
# Balanced Accuracy       0.8014    0.74051     0.74766


###############################################################################################################
####################### Recursive Feature Elimination usinf random forest
###############################################################################################################

# ImpVarList_RFE <- c('ProgStatus', 'Centiloid_fSUVR_TOT_CORTMEAN', 'TOTAL_HIPPOCAMPUS_VOLUME', 'Left.Amygdala_volume',
#                     'Right.Amygdala_volume', 'lh_middletemporal_thickness', 'Left.Accumbens.area_volume', 
#                     'lh_temporalpole_thickness', 'lh_superiortemporal_thickness', 'Right.Inf.Lat.Vent_volume',
#                     'SubCortGrayVol', 'rh_inferiortemporal_thickness','CC_Posterior_volume', 'X4th.Ventricle_volume')
ImpVarList_RFE <- c('ProgStatus', 'Centiloid_fSUVR_TOT_CORTMEAN', 'TOTAL_HIPPOCAMPUS_VOLUME', 'Left.Amygdala_volume',
                    'Right.Amygdala_volume', 'lh_middletemporal_thickness', 'Left.Accumbens.area_volume',
                    'lh_temporalpole_thickness', 'lh_superiortemporal_thickness', 'Right.Inf.Lat.Vent_volume',
                    'SubCortGrayVol', 'rh_inferiortemporal_thickness','CC_Posterior_volume', 'X4th.Ventricle_volume')

VisitProg_RFE_train <- VisitProg_train %>% dplyr::select(ImpVarList_RFE)
VisitProg_RFE_test <- VisitProg_test %>% dplyr::select(ImpVarList_RFE)

rf_VisitProg_RFE_model <- randomForest(ProgStatus ~ ., data = VisitProg_RFE_train, proximity = TRUE) 
#print(rf_VisitProg_RFE_model )

# Training data
rf_VisitProg_RFE_train_predicted <- predict(rf_VisitProg_RFE_model, VisitProg_RFE_train)
mean(rf_VisitProg_RFE_train_predicted  == VisitProg_RFE_train$ProgStatus)   # 1
# confusionMatrix(rf_VisitProg_RFE_train_predicted, VisitProg_RFE_train$ProgStatus)


# Test data
rf_VisitProg_RFE_test_predicted <- predict(rf_VisitProg_RFE_model, VisitProg_RFE_test)
mean(rf_VisitProg_RFE_test_predicted  == VisitProg_RFE_test$ProgStatus)   # 0.828125
#confusionMatrix(rf_VisitProg_RFE_test_predicted, VisitProg_RFE_test$ProgStatus)



#############################################################################################################
############################ RFE using Random Forest ########################################################
#############################################################################################################

control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

x_VisitProg_train <- VisitProg_train[, -1]
y_VisitProg_train <- VisitProg_train[, 1]

x_VisitProg_test <- VisitProg_test[, -1]
y_VisitProg_test <- VisitProg_test[, 1]

VisitProg_rfe1 <- rfe(x = x_VisitProg_train, 
                     y = y_VisitProg_train, 
                     sizes = c(1:13),
                     rfeControl = control)
VisitProg_rfe1 
# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (10 fold, repeated 5 times) 
# 
# Resampling performance over subset size:
#   
#   Variables Accuracy  Kappa AccuracySD KappaSD Selected
# 1   0.7538 0.3278    0.03439 0.08766         
# 2   0.7962 0.3915    0.02677 0.07193         
# 3   0.8310 0.4804    0.02438 0.08282         
# 4   0.8349 0.4918    0.02579 0.08517         
# 5   0.8382 0.4966    0.02526 0.08909         
# 6   0.8463 0.5178    0.02183 0.07652         
# 7   0.8450 0.5081    0.02537 0.09345         
# 8   0.8500 0.5207    0.02342 0.08832         
# 9   0.8482 0.5202    0.02253 0.08371         
# 10   0.8506 0.5259    0.02580 0.09605         
# 11   0.8510 0.5254    0.02492 0.08970         
# 12   0.8510 0.5239    0.02250 0.08830         
# 13   0.8532 0.5282    0.02091 0.08145        *
#   85   0.8522 0.4929    0.01947 0.08823         
# 
# The top 5 variables (out of 13):
#   Centiloid_fSUVR_TOT_CORTMEAN, Right.Inf.Lat.Vent_volume, TOTAL_HIPPOCAMPUS_VOLUME, 
#Left.Amygdala_volume, Right.Amygdala_volume

ggplot(data = VisitProg_rfe1, metric = "Accuracy") + theme_bw()
ggplot(data = VisitProg_rfe1, metric = "Kappa") + theme_bw()

VisitProg_varimp_data <- data.frame(feature = row.names(varImp(VisitProg_rfe1))[1:13],
                          importance = varImp(VisitProg_rfe1)[1:13, 1])
save(VisitProg_varimp_data, file = "VisitProg_varimp_data.RData")

ggplot(data = VisitProg_varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")

postResample(predict(VisitProg_rfe1, x_VisitProg_test), y_VisitProg_test)
#  Accuracy     Kappa 
# 0.8647059 0.5734701 