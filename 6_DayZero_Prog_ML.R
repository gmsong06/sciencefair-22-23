###############################################  File 6 ####################################
########## Machine Learning based on Day 0 values (based on File 5)
######### Group of CN at day 0:
# Prog: has progression to DMN (CN_(MCI)_DMN, etc)
# NoProg: no progression (CN)

load("ProgFromCN2.RData")    # From file 5
str(ProgFromCN2)
#### Find OASISID with CN, and OASISID that is not CN
# ProgID <- ProgFromCN2 %>% dplyr::select(OASISID, ProgStatus)
# ProgID[!duplicated(ProgID)]
Prog <- ProgFromCN2[!duplicated(ProgFromCN2$OASISID), ]
View(Prog)
ProgID <- Prog %>% dplyr::select(OASISID, ProgStatus)
NoProgID <- filter(ProgID, ProgStatus == "CN")
YesProgID <- filter(ProgID, ProgStatus != "CN")
ListNoProg <- NoProgID$OASISID
ListYesProg <- YesProgID$OASISID
ListYesProg

############## Find Day 0
load("ClinDia_Patient.RData")
ClinDia_DayZero <- filter(ClinDia_Patient, years_to_Visit == 0)
nrow(ClinDia_DayZero)
n_distinct(ClinDia_DayZero$OASISID)
ClinDia_DayZero_cut <- ClinDia_DayZero %>% dplyr::select(OASISID, DEMENTED)
summary(ClinDia_DayZero_cut$DEMENTED)
view(ClinDia_DayZero_cut)
str(ClinDia_DayZero_cut)

# for (i in 1:nrow(ClinDia_DayZero_cut)) {
#   if (ClinDia_DayZero_cut$DEMENTED[i] == '1') {
#     ClinDia_DayZero_cut$ProgStatus[i] <- 'MDN'
#     print(i)
#   } else if (ClinDia_DayZero_cut$OASISID[i] %in% ProgID){
#     ClinDia_DayZero_cut$ProgStatus[i] <- 'Prog'
#   }
#   else {ClinDia_DayZero_cut$ProgStatus[i] <- 'CN'}
# }
# 
# ClinDia_DayZero_cut$ProgStatus <- 'CN'
# 
# for (i in 1:nrow(ClinDia_DayZero_cut)) {
#   if (ClinDia_DayZero_cut$DEMENTED[i] == '1') {
#     ClinDia_DayZero_cut$ProgStatus[i] <- 'MDN'
#   } 
#   else if(ClinDia_DayZero_cut$OASISID[i] %in% ListYesProg){
#     print(i)
#     ClinDia_DayZero_cut$ProgStatus[i] <- 'Prog'
#   }
#   else {ClinDia_DayZero_cut$ProgStatus[i] <- 'CN'}
# }

for (i in 1:nrow(ClinDia_DayZero_cut)) {
  if (ClinDia_DayZero_cut$DEMENTED[i] == '1') {
    ClinDia_DayZero_cut$ProgStatus[i] <- 'MDN'
  } 
  else if(ClinDia_DayZero_cut$OASISID[i] %in% ListYesProg){
    ClinDia_DayZero_cut$ProgStatus[i] <- 'Prog'
  }
  else if(ClinDia_DayZero_cut$DEMENTED[i] == '0'){
    ClinDia_DayZero_cut$ProgStatus[i] <- 'MCI'
  }
  else {ClinDia_DayZero_cut$ProgStatus[i] <- 'CN'}
}
ClinDia_DayZero_cut$ProgStatus <- as.factor(as.character(ClinDia_DayZero_cut$ProgStatus))
summary(ClinDia_DayZero_cut$ProgStatus)
# CN  MCI  MDN Prog 
# 694   79  222  106 

ClinDia_DayZero_cut <- filter(ClinDia_DayZero_cut, ProgStatus != "MCI")
ClinDia_DayZero_cut$ProgStatus <- as.factor(as.character(ClinDia_DayZero_cut$ProgStatus))
summary(ClinDia_DayZero_cut$ProgStatus)

save(ClinDia_DayZero_cut, file = "ClinDia_DayZero_cut.RData")
load("ClinDia_DayZero_cut.RData")
view(ClinDia_DayZero_cut)

ClinDia_DayZero_Prog <- ClinDia_DayZero_cut[, -2]
load("Freesurf_cut.RData")
Freesurf_cut_DayZero <- filter(Freesurf_cut, years_to_Visit == 0)
ClinDia_DayZero_Fs <- merge(ClinDia_DayZero_Prog, Freesurf_cut_DayZero, by.x = "OASISID")
str(ClinDia_DayZero_Fs)

n_distinct(ClinDia_DayZero_Fs$OASISID)    # 745 patients
load("Centiloid_raw.RData")
Centiloid_raw_DyZero <- filter(Centiloid_raw, years_to_Visit == 0)
str(Centiloid_raw_DyZero)
Centiloid_raw_DyZero <- Centiloid_raw_DyZero %>% dplyr::select(OASISID, Centiloid_fSUVR_TOT_CORTMEAN)
str(ClinDia_DayZero_Fs)
ClinDia_DayZero_FsCenti <- merge(ClinDia_DayZero_Fs, Centiloid_raw_DyZero)
str(ClinDia_DayZero_FsCenti)
save(ClinDia_DayZero_FsCenti, file = "ClinDia_DayZero_FsCenti.RData")

Day_Zero <- ClinDia_DayZero_FsCenti %>% dplyr::select(-years_to_Visit, -days_to_visit,
                                                      -Left.non.WM.hypointensities_volume)
str(Day_Zero)
save(Day_Zero, file = "Day_Zero.RData")
load("Day_Zero.RData")
Day_Zero2 <- Day_Zero[, -1]

###############################################################################################################
####################### Modeling: Progression from CN to MCI and/or DMN (full Variables)
###############################################################################################################

####### parittion Data
set.seed(4365677)
Day_Zero_sampling_vector <- createDataPartition(Day_Zero2$ProgStatus, p = 0.80, list = FALSE)
Day_Zero_train <- Day_Zero2[Day_Zero_sampling_vector,]
Day_Zero_test <- Day_Zero2[-Day_Zero_sampling_vector,]

save(Day_Zero_train, file = "Day_Zero_train.RData")
save(Day_Zero_test, file = "Day_Zero_test.RData")


# normalize the input data
Day_Zero_pp <- preProcess(Day_Zero_train, method = c("range"))
norm_Day_Zero_train <- predict(Day_Zero_pp, Day_Zero_train)
norm_Day_Zero_test <- predict(Day_Zero_pp, Day_Zero_test)

###########################################################
############# use a neural network (nnet)##################
###########################################################

nn_Day_Zero_model <- nnet(ProgStatus ~ ., data = norm_Day_Zero_train, size = 10, maxit = 10000, decay = 0.01)

# check accuracy of model
nn_Day_Zero_train_predictions <- predict(nn_Day_Zero_model, norm_Day_Zero_train, type = "class")
mean(nn_Day_Zero_train_predictions == Day_Zero_train$ProgStatus)    # 1
table(Day_Zero_train$ProgStatus, nn_Day_Zero_train_predictions)
#      nn_Day_Zero_train_predictions
#       CN MDN Prog
# CN   384   0    0
# MDN    0  71    0
# Prog   0   0   68

nn_Day_Zero_test_predictions <- predict(nn_Day_Zero_model, norm_Day_Zero_test, type = "class")
mean(nn_Day_Zero_test_predictions == norm_Day_Zero_test$ProgStatus)  # 0.75
table(nn_Day_Zero_test_predictions, norm_Day_Zero_test$ProgStatus)
# nn_Day_Zero_test_predictions CN MDN Prog
#                         CN   84   4   12
#                         MDN   2   9    1
#                         Prog  9   4    3

nn_Day_Zero_varImp <- varImp(nn_Day_Zero_model)
nn_Day_Zero_varImp_df <- as.data.frame(nn_Day_Zero_varImp)
nn_Day_Zero_varImp2 <- nn_Day_Zero_varImp_df[order(-nn_Day_Zero_varImp_df$Overall), ,drop = FALSE]
nn_Day_Zero_varImp2$labels <- factor(rownames(nn_Day_Zero_varImp2))
nn_Day_Zero_varImp2$labels <- reorder(nn_Day_Zero_varImp2$labels, nn_Day_Zero_varImp2$Overall)
nn_Day_Zero_varImp3 <- nn_Day_Zero_varImp2[c(1:10), ]
nn_Day_Zero_ImpVar <- rownames(nn_Day_Zero_varImp3)

# plot the importance
ggplot(data = nn_Day_Zero_varImp3, aes(x = labels,y = Overall)) +
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
rf_Day_Zero_model <- randomForest(ProgStatus ~ ., data = Day_Zero_train, proximity = TRUE) 
print(rf_Day_Zero_model )
# Call:
#   randomForest(formula = ProgStatus ~ ., data = Day_Zero_train,      proximity = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 9
# 
# OOB estimate of  error rate: 15.68%
# Confusion matrix:
#   CN MDN Prog class.error
# CN   382   2    0 0.005208333
# MDN   27  44    0 0.380281690
# Prog  44   9   15 0.779411765

# Training data
rf_Day_Zero_train_predicted <- predict(rf_Day_Zero_model, Day_Zero_train)
mean(rf_Day_Zero_train_predicted  == Day_Zero_train$ProgStatus)    # 1
confusionMatrix(rf_Day_Zero_train_predicted, Day_Zero_train$ProgStatus)
# Prediction  CN MDN Prog
#       CN   384   0    0
#       MDN    0  71    0
#       Prog   0   0   68

# Test data
rf_Day_Zero_test_predicted <- predict(rf_Day_Zero_model, Day_Zero_test)
mean(rf_Day_Zero_test_predicted  == Day_Zero_test$ProgStatus)    # 0.8125
confusionMatrix(rf_Day_Zero_test_predicted, Day_Zero_test$ProgStatus)   
#              Reference
# Prediction  CN MDN Prog
#       CN    95  10   14
#       MDN    0   7   0
#       Prog   0   0    2

# Find important factors
rf_Day_Zero_varImp <- varImp(rf_Day_Zero_model)
rf_Day_Zero_varImp2 <- rf_Day_Zero_varImp[order(-rf_Day_Zero_varImp$Overall), , drop = FALSE]
rf_Day_Zero_varImp2$labels <- factor(rownames(rf_Day_Zero_varImp2))
rf_Day_Zero_varImp2$labels <- reorder(rf_Day_Zero_varImp2$labels, rf_Day_Zero_varImp2$Overall)
rf_Day_Zero_varImp3 <- rf_Day_Zero_varImp2[c(1:10), ]
rf_Day_Zero_varImp3
rf_Day_Zero_ImpVar <- rownames(rf_Day_Zero_varImp3)

# plot the importance
ggplot(data= rf_Day_Zero_varImp3, aes(x = labels,y = Overall)) +
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

tree_Day_Zero_model <- rpart(ProgStatus ~ ., data = Day_Zero_train, method = "class")
#identify best cp value to use
dt_Day_Zero_best <- tree_Day_Zero_model$cptable[which.min(tree_Day_Zero_model$cptable[,"xerror"]),"CP"]
pruned_Day_Zero_tree <- prune(tree_Day_Zero_model, cp = dt_Day_Zero_best)
# Visualize the decision tree with rpart.plot
prp(pruned_Day_Zero_tree)
#plot decision tree using custom arguments
prp(pruned_Day_Zero_tree,
    faclen = 1, #use full names for factor labels
    extra = 1, #display number of observations for each terminal node
    roundint = F) #don't round to integers in output
#    digits = 3) #display 5 decimal places in output

rpart.plot(pruned_Day_Zero_tree, type = 4)

#Testing the training model
tree_Day_Zero_train_pred <- predict(object = tree_Day_Zero_model, Day_Zero_train[-1], type="class")
#Calculating accuracy
mean(Day_Zero_train$ProgStatus == tree_Day_Zero_train_pred) # 0.8565966
table(Day_Zero_train$ProgStatus, tree_Day_Zero_train_pred)
#       CN MDN Prog
# CN   362  11   11
# MDN   10  56    5
# Prog  28  10   30
confusionMatrix(Day_Zero_train$ProgStatus, tree_Day_Zero_train_pred)

#Testing the test model
tree_Day_Zero_test_pred <- predict(object = tree_Day_Zero_model, Day_Zero_test[-1], type="class")
#Calculating accuracy
mean(Day_Zero_test$ProgStatus == tree_Day_Zero_test_pred)    # 0.7109375
table(Day_Zero_test$ProgStatus, tree_Day_Zero_test_pred)
# tree_Day_Zero_test_pred
#      CN MDN Prog
# CN   80   4   11
# MDN   8   7    2
# Prog 11   1    4
confusionMatrix(ProgFromCN_test$ProgStatus, tree_Day_Zero_test_pred)

tree_Day_Zero_varImp <- varImp(tree_Day_Zero_model)
tree_Day_Zero_varImp2 <- tree_Day_Zero_varImp[order(-tree_Day_Zero_varImp$Overall), , drop = FALSE]
tree_Day_Zero_varImp2$labels <- factor(rownames(tree_Day_Zero_varImp2))
tree_Day_Zero_varImp2$labels <- reorder(tree_Day_Zero_varImp2$labels, tree_Day_Zero_varImp2$Overall)
tree_Day_Zero_varImp3 <- tree_Day_Zero_varImp2[c(1:10), ]
tree_Day_Zero_varImp3
tree_Day_Zero_ImpVar <- rownames(tree_Day_Zero_varImp3)

# plot the importance
ggplot(data = tree_Day_Zero_varImp3, aes(x = labels,y = Overall)) +
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

Day_Zero_ImpVar_Com <- data.frame(unlist(nn_Day_Zero_ImpVar),
                                 unlist(rf_Day_Zero_ImpVar), 
                                 unlist(tree_Day_Zero_ImpVar))
names(Day_Zero_ImpVar_Com) <- c("NeuralNetwork","RandomForest", "Decision Tree")
View(Day_Zero_ImpVar_Com)
write_xlsx(Day_Zero_ImpVar_Com,"Important_Factors_Day_Zero.xlsx")

###############################################################################################################
####################### Modeling: Progression from CN to MCI and/or DMN (top 8 variable)
###############################################################################################################
ImpVarList <- c('ProgStatus', 'Centiloid_fSUVR_TOT_CORTMEAN','TOTAL_HIPPOCAMPUS_VOLUME', 'Left.Amygdala_volume',
                'Right.Amygdala_volume', 'lh_middletemporal_thickness', 'Left.Accumbens.area_volume', 
                'lh_temporalpole_thickness', 'lh_superiortemporal_thickness', 'Right.Inf.Lat.Vent_volume')

Day_Zero_Top8_train <- Day_Zero_train %>% dplyr::select(ImpVarList)
Day_Zero_Top8_test <- Day_Zero_test %>% dplyr::select(ImpVarList)
norm_Day_Zero_Top8_train <- norm_Day_Zero_train %>% dplyr::select(ImpVarList)
norm_Day_Zero_Top8_test <- norm_Day_Zero_test %>% dplyr::select(ImpVarList)

###########################################################
############# neural network (nnet)##################
###########################################################

nn_Day_Zero_Top8_model <- nnet(ProgStatus ~ ., 
                                data = norm_Day_Zero_Top8_train, 
                                size = 10, 
                                maxit = 10000, 
                                decay =0.01 )
# check accuracy of model
nn_Day_Zero_train_Top8_predictions <- predict(nn_Day_Zero_Top8_model, 
                                               norm_Day_Zero_Top8_train, 
                                               type = "class")
mean(nn_Day_Zero_train_Top8_predictions == Day_Zero_Top8_train$ProgStatus)   # 0.917782
table(nn_Day_Zero_train_Top8_predictions, Day_Zero_Top8_train$ProgStatus)
# nn_Day_Zero_train_Top8_predictions   CN  DMN Prog
#                                CN   379  10   26
#                                MDN    1  61    2
#                                Prog   4   0   40
table(nn_Day_Zero_train_Top8_predictions)
# nn_Day_Zero_train_Top8_predictions
#   CN  DMN Prog 
#  415   64   44

nn_Day_Zero_test_Top8_predictions <- predict(nn_Day_Zero_Top8_model, 
                                              norm_Day_Zero_Top8_test, 
                                              type = "class")
mean(nn_Day_Zero_test_Top8_predictions == Day_Zero_Top8_test$ProgStatus)    #0.7734375
table(nn_Day_Zero_test_Top8_predictions, Day_Zero_Top8_test$ProgStatus)
# nn_Day_Zero_test_Top8_predictions CN MDN Prog
#                              CN   86   6   12
#                              MDN   3   9    0
#                              Prog  6   2    4

###########################################################
####################### Random Forest ########################
###########################################################
rf_Day_Zero_Top8_model <- randomForest(ProgStatus ~ ., data = Day_Zero_Top8_train, proximity = TRUE) 
print(rf_Day_Zero_Top8_model )
# Call:
#   randomForest(formula = ProgStatus ~ ., data = Day_Zero_Top8_train,      proximity = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3
# 
# OOB estimate of  error rate: 15.87%
# Confusion matrix:
#   CN MDN Prog class.error
# CN   377   4    3  0.01822917
# MDN   21  47    3  0.33802817
# Prog  41  11   16  0.76470588

# Training data
rf_Day_Zero_Top8_train_predicted <- predict(rf_Day_Zero_Top8_model, Day_Zero_Top8_train)
mean(rf_Day_Zero_Top8_train_predicted  == Day_Zero_Top8_train$ProgStatus)   # 1
confusionMatrix(rf_Day_Zero_Top8_train_predicted, Day_Zero_Top8_train$ProgStatus)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  CN MDN Prog
# CN   384   0    0
# MDN    0  71    0
# Prog   0   0   68
# 
# Overall Statistics
# 
# Accuracy : 1         
# 95% CI : (0.993, 1)
# No Information Rate : 0.7342    
# P-Value [Acc > NIR] : < 2.2e-16 
# 
# Kappa : 1         
# 
# Mcnemar's Test P-Value : NA        
# 
# Statistics by Class:
# 
#                      Class: CN Class: MDN Class: Prog
# Sensitivity             1.0000     1.0000        1.00
# Specificity             1.0000     1.0000        1.00
# Pos Pred Value          1.0000     1.0000        1.00
# Neg Pred Value          1.0000     1.0000        1.00
# Prevalence              0.7342     0.1358        0.13
# Detection Rate          0.7342     0.1358        0.13
# Detection Prevalence    0.7342     0.1358        0.13
# Balanced Accuracy       1.0000     1.0000        1.00

# Test data
rf_Day_Zero_Top8_test_predicted <- predict(rf_Day_Zero_Top8_model, Day_Zero_Top8_test)
mean(rf_Day_Zero_Top8_test_predicted  == Day_Zero_Top8_test$ProgStatus)   # 0.828125
confusionMatrix(rf_Day_Zero_Top8_test_predicted, Day_Zero_Top8_test$ProgStatus)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction CN MDN Prog
# CN   95   9   13
# MDN   0   8    0
# Prog  0   0    3
# 
# Overall Statistics
# 
# Accuracy : 0.8281         
# 95% CI : (0.7514, 0.889)
# No Information Rate : 0.7422         
# P-Value [Acc > NIR] : 0.0142         
# 
# Kappa : 0.4462         
# 
# Mcnemar's Test P-Value : NA             
# 
# Statistics by Class:
# 
#                      Class: CN Class: MDN Class: Prog
# Sensitivity             1.0000     0.4706     0.18750
# Specificity             0.3333     1.0000     1.00000
# Pos Pred Value          0.8120     1.0000     1.00000
# Neg Pred Value          1.0000     0.9250     0.89600
# Prevalence              0.7422     0.1328     0.12500
# Detection Rate          0.7422     0.0625     0.02344
# Detection Prevalence    0.9141     0.0625     0.02344
# Balanced Accuracy       0.6667     0.7353     0.59375

###########################################################
#################### Decision Tree ########################
###########################################################

tree_Day_Zero_Top8_model <- rpart(ProgStatus ~ ., data = Day_Zero_Top8_train, method = "class")
#identify best cp value to use
dt_Day_Zero_Top8_best <- tree_Day_Zero_Top8_model$cptable[which.min(tree_Day_Zero_Top8_model$cptable[,"xerror"]),"CP"]
pruned_Day_Zero_Top8_tree <- prune(tree_Day_Zero_Top8_model, dt_Day_Zero_Top8_best)
# Visualize the decision tree with rpart.plot
prp(pruned_Day_Zero_Top8_tree)
#plot decision tree using custom arguments
prp(pruned_Day_Zero_Top8_tree,
    faclen = 1, #use full names for factor labels
    extra = 1, #display number of observations for each terminal node
    roundint = F) #don't round to integers in output
#    digits = 3) #display 5 decimal places in output

rpart.plot(pruned_Day_Zero_Top8_tree, type = 4)

#Testing the training model
tree_Day_Zero_train_Top8_pred <- predict(object = tree_Day_Zero_Top8_model, Day_Zero_Top8_train[-1], type="class")
#Calculating accuracy
mean(Day_Zero_Top8_train$ProgStatus == tree_Day_Zero_train_Top8_pred)   # 0.8260038
table(Day_Zero_Top8_train$ProgStatus, tree_Day_Zero_train_Top8_pred)
#    tree_Day_Zero_train_Top8_pred
#       CN MDN Prog
# CN   382   2    0
# MDN   21  50    0
# Prog  57  11    0
confusionMatrix(Day_Zero_Top8_train$ProgStatus, tree_Day_Zero_train_Top8_pred)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  CN MDN Prog
# CN   382   2    0
# MDN   21  50    0
# Prog  57  11    0
# 
# Overall Statistics
# 
# Accuracy : 0.826           
# 95% CI : (0.7907, 0.8575)
# No Information Rate : 0.8795          
# P-Value [Acc > NIR] : 0.9999          
# 
# Kappa : 0.485           
# 
# Mcnemar's Test P-Value : <2e-16          
# 
# Statistics by Class:
# 
#                      Class: CN Class: MDN Class: Prog
# Sensitivity             0.8304     0.7937          NA
# Specificity             0.9683     0.9543        0.87
# Pos Pred Value          0.9948     0.7042          NA
# Neg Pred Value          0.4388     0.9712          NA
# Prevalence              0.8795     0.1205        0.00
# Detection Rate          0.7304     0.0956        0.00
# Detection Prevalence    0.7342     0.1358        0.13
# Balanced Accuracy       0.8993     0.8740          NA

tree_Day_Zero_test_Top8_pred <- predict(object = tree_Day_Zero_Top8_model, Day_Zero_Top8_test[-1], type="class")
#Calculating accuracy
mean(Day_Zero_Top8_test$ProgStatus == tree_Day_Zero_test_Top8_pred)  # 0.79687
table(Day_Zero_Top8_test$ProgStatus, tree_Day_Zero_test_Top8_pred)
# tree_Day_Zero_test_Top8_pred
#     CN MDN Prog
# CN   95   0    0
# MDN  10   7    0
# Prog 15   1    0
confusionMatrix(Day_Zero_Top8_test$ProgStatus, tree_Day_Zero_test_Top8_pred)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction CN MDN Prog
# CN   95   0    0
# MDN  10   7    0
# Prog 15   1    0
# 
# Overall Statistics
# 
# Accuracy : 0.7969          
# 95% CI : (0.7167, 0.8628)
# No Information Rate : 0.9375          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.3135          
# 
# Mcnemar's Test P-Value : 9.537e-06       
# 
# Statistics by Class:
# 
#                      Class: CN Class: MDN Class: Prog
# Sensitivity             0.7917    0.87500          NA
# Specificity             1.0000    0.91667       0.875
# Pos Pred Value          1.0000    0.41176          NA
# Neg Pred Value          0.2424    0.99099          NA
# Prevalence              0.9375    0.06250       0.000
# Detection Rate          0.7422    0.05469       0.000
# Detection Prevalence    0.7422    0.13281       0.125
# Balanced Accuracy       0.8958    0.89583          NA


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
                    'SubCortGrayVol', 'rh_inferiortemporal_thickness','CC_Posterior_volume')

Day_Zero_RFE_train <- Day_Zero_train %>% dplyr::select(ImpVarList_RFE)
Day_Zero_RFE_test <- Day_Zero_test %>% dplyr::select(ImpVarList_RFE)

rf_Day_Zero_RFE_model <- randomForest(ProgStatus ~ ., data = Day_Zero_RFE_train, proximity = TRUE) 
#print(rf_Day_Zero_RFE_model )

# Training data
rf_Day_Zero_RFE_train_predicted <- predict(rf_Day_Zero_RFE_model, Day_Zero_RFE_train)
mean(rf_Day_Zero_RFE_train_predicted  == Day_Zero_RFE_train$ProgStatus)   # 1
# confusionMatrix(rf_Day_Zero_RFE_train_predicted, Day_Zero_RFE_train$ProgStatus)


# Test data
rf_Day_Zero_RFE_test_predicted <- predict(rf_Day_Zero_RFE_model, Day_Zero_RFE_test)
mean(rf_Day_Zero_RFE_test_predicted  == Day_Zero_RFE_test$ProgStatus)   # 0.828125
#confusionMatrix(rf_Day_Zero_RFE_test_predicted, Day_Zero_RFE_test$ProgStatus)


#############################################################################################################
############################ RFE using Random Forest ########################################################
#############################################################################################################

control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

x_Day_Zero_train <- Day_Zero_train[, -1]
y_Day_Zero_train <- Day_Zero_train[, 1]

x_Day_Zero_test <- Day_Zero_test[, -1]
y_Day_Zero_test <- Day_Zero_test[, 1]

Day_Zero_rfe1 <- rfe(x = x_Day_Zero_train, 
                      y = y_Day_Zero_train, 
                      sizes = c(1:13),
                      rfeControl = control)
Day_Zero_rfe1 
# Recursive feature selection

# Outer resampling method: Cross-Validated (10 fold, repeated 5 times) 
# 
# Resampling performance over subset size:
#   
#   Variables Accuracy  Kappa AccuracySD KappaSD Selected
# 1   0.7161 0.3023    0.04186 0.08969         
# 2   0.8075 0.4861    0.03665 0.10440         
# 3   0.8367 0.5509    0.03430 0.10972         
# 4   0.8332 0.5464    0.03742 0.11808         
# 5   0.8317 0.5431    0.03973 0.12530         
# 6   0.8317 0.5400    0.03501 0.11855         
# 7   0.8405 0.5619    0.03442 0.11253         
# 8   0.8401 0.5581    0.03568 0.11553         
# 9   0.8412 0.5640    0.03898 0.13359         
# 10   0.8393 0.5541    0.03492 0.11960         
# 11   0.8439 0.5678    0.03550 0.12074        *
#   12   0.8394 0.5540    0.03696 0.12231         
# 13   0.8401 0.5550    0.03321 0.11329         
# 85   0.8363 0.5085    0.03438 0.13708         
# 
# The top 5 variables (out of 11):
#   Centiloid_fSUVR_TOT_CORTMEAN, TOTAL_HIPPOCAMPUS_VOLUME, Right.Inf.Lat.Vent_volume, 
# rh_isthmuscingulate_thickness, Right.Amygdala_volume

ggplot(data = Day_Zero_rfe1, metric = "Accuracy") + theme_bw()
ggplot(data = Day_Zero_rfe1, metric = "Kappa") + theme_bw()

Day_Zero_varimp_data <- data.frame(feature = row.names(varImp(Day_Zero_rfe1))[1:11],
                                    importance = varImp(Day_Zero_rfe1)[1:11, 1])
save(Day_Zero_varimp_data, file = "Day_Zero_varimp_data.RData")

ggplot(data = Day_Zero_varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")

postResample(predict(Day_Zero_rfe1, x_Day_Zero_test), y_Day_Zero_test)
#  Accuracy     Kappa 
# 0.8125000 0.4140759 

