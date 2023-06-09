######################################### File #2 ##################################################
#### Partial ML: using common top important variables identified by RF and Decision Tree from File 1
# Centiloid_fSUVR_TOT_CORTMEAN
# TOTAL_HIPPOCAMPUS_VOLUME
# Left.Amygdala_volume
# Right.Amygdala_volume
# lh_middletemporal_thickness
# Left.Accumbens.area_volume
# lh_temporalpole_thickness
# lh_superiortemporal_thickness

###########################################
### prepare the file
############################################
load("final_NoNA_DEMENTED.RData")
str(final_NoNA_DEMENTED)
DEMENTED_Top8 <- final_NoNA_DEMENTED %>% dplyr::select(DEMENTED,
                                                        Centiloid_fSUVR_TOT_CORTMEAN,
                                                        TOTAL_HIPPOCAMPUS_VOLUME,
                                                        Left.Amygdala_volume,
                                                        Right.Amygdala_volume,
                                                        lh_middletemporal_thickness,
                                                        Left.Accumbens.area_volume,
                                                        lh_temporalpole_thickness,
                                                       lh_superiortemporal_thickness
                                                       # Right.Inf.Lat.Vent_volume,
                                                       # CC_Posterior_volume
                                                       )
str(DEMENTED_Top8)
# 'data.frame':	1672 obs. of  8 variables:
#   $ Centiloid_fSUVR_TOT_CORTMEAN : num  5.8 3.9 8.78 5.02 13.52 ...
# $ TOTAL_HIPPOCAMPUS_VOLUME     : num  7648 7106 7106 6862 6964 ...
# $ Left.Amygdala_volume         : num  1511 1293 1293 1263 1346 ...
# $ Right.Amygdala_volume        : num  1368 1543 1543 1334 1408 ...
# $ lh_middletemporal_thickness  : num  2.78 2.67 2.67 2.75 2.76 ...
# $ Left.Accumbens.area_volume   : num  280 348 348 412 316 ...
# $ lh_temporalpole_thickness    : num  3.85 3.64 3.64 3.69 3.55 ...
# $ lh_superiortemporal_thickness: num  2.81 2.55 2.55 2.63 2.52 ...

set.seed(4365677)
DEMENTED_Top8_sampling_vector <- createDataPartition(DEMENTED_Top8$DEMENTED, p = 0.80, list = FALSE)
DEMENTED_Top8_train <- DEMENTED_Top8[DEMENTED_Top8_sampling_vector,]
DEMENTED_Top8_test <- DEMENTED_Top8[-DEMENTED_Top8_sampling_vector,]

# normalize the input data
DEMENTED_Top8_pp <- preProcess(DEMENTED_Top8_train, method = c("range"))
norm_DEMENTED_Top8_train <- predict(DEMENTED_Top8_pp, DEMENTED_Top8_train)
norm_DEMENTED_Top8_test <- predict(DEMENTED_Top8_pp, DEMENTED_Top8_test)

#####################################################################################
############# use a neural network (nnet)############################################
#####################################################################################

# create model
# start_time <- proc.time()
nn_DEMENTED_Top8_model <- nnet(DEMENTED ~ ., data = norm_DEMENTED_Top8_train, size = 10, maxit = 10000, decay =0.01 )
# end_time <- proc.time()
# end_time - start_time
# #summary(DEMENTED_model)

# check accuracy of model
nn_DEMENTED_Top8_train_predictions <- predict(nn_DEMENTED_Top8_model, norm_DEMENTED_Top8_train, type = "class")
mean(nn_DEMENTED_Top8_train_predictions == DEMENTED_Top8_train$DEMENTED) # 0.9297459
table(nn_DEMENTED_Top8_train_predictions, DEMENTED_Top8_train$DEMENTED)
#      0    1    3
# 0   10    1    2
# 1    8   84    9
# 3   39   35 1150

nn_DEMENTED_Top8_test_predictions <- predict(nn_DEMENTED_Top8_model, norm_DEMENTED_Top8_test, type = "class")
mean(nn_DEMENTED_Top8_test_predictions == DEMENTED_Top8_test$DEMENTED)   # 0.8772455
table(nn_DEMENTED_Top8_test_predictions, DEMENTED_Top8_test$DEMENTED)
# nn_DEMENTED_Top8_test_predictions   0   1   3
#                                 0   1   2   2
#                                 1   6  14  10
#                                 3   7  14 278
summary(factor(nn_DEMENTED_Top8_test_predictions))
# 0   1   3 
# 5  30 299

# compare different models variables of importance
nn_DEMENTED_Top8_varImp <- varImp(nn_DEMENTED_Top8_model)
nn_DEMENTED_Top8_varImp_df <- as.data.frame(nn_DEMENTED_Top8_varImp)
nn_DEMENTED_Top8_varImp2 <- nn_DEMENTED_Top8_varImp_df[order(-nn_DEMENTED_Top8_varImp_df$Overall), ,drop = FALSE]
nn_DEMENTED_Top8_varImp2$labels <- factor(rownames(nn_DEMENTED_Top8_varImp2))
nn_DEMENTED_Top8_varImp2$labels <- reorder(nn_DEMENTED_Top8_varImp2$labels, nn_DEMENTED_Top8_varImp2$Overall)
DEMENTED_Top8_nn_ImpVar <- rownames(nn_DEMENTED_Top8_varImp2)

# plot the importance
ggplot(data = nn_DEMENTED_Top8_varImp2, aes(x = labels,y = Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + 
  geom_point(color='blue') + 
  ylab("Importance Score") +
  xlab("Variables") +
  ggtitle("Feature Importance of Neural Network") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  PlotTheme +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))


#######################################################################################
####################### Random Forest #################################################
#######################################################################################
rf_DEMENTED_Top8_model <- randomForest(DEMENTED ~., data = DEMENTED_Top8_train, proximity = TRUE) 
print(rf_DEMENTED_Top8_model)

# Training data
rf_DEMENTED_Top8_train_predicted <- predict(rf_DEMENTED_Top8_model, DEMENTED_Top8_train)
mean(rf_DEMENTED_Top8_train_predicted == DEMENTED_Top8_train$DEMENTED)   # 1
confusionMatrix(rf_DEMENTED_Top8_train_predicted, DEMENTED_Top8_train$DEMENTED)
#    Row == prediction
#               Reference
# Prediction    0    1    3
#          0   57    0    0
#          1    0  120    0
#          3    0    0 1161  


# Test data
rf_DEMENTED_Top8_test_predicted <- predict(rf_DEMENTED_Top8_model, DEMENTED_Top8_test)
mean(rf_DEMENTED_Top8_test_predicted == DEMENTED_Top8_test$DEMENTED)     # 0.8862275
confusionMatrix(rf_DEMENTED_Top8_test_predicted, DEMENTED_Top8_test$DEMENTED)
#              Reference
# Prediction   0   1   3
#          0   1   0   2
#          1   8  15   8
#          3   5  15 280

# Find important factors
DEMENTED_Top8_rf_varImp <- varImp(rf_DEMENTED_Top8_model)
DEMENTED_Top8_rf_varImp2 <- DEMENTED_Top8_rf_varImp[order(-DEMENTED_Top8_rf_varImp$Overall), , drop = FALSE]
DEMENTED_Top8_rf_varImp2$labels <- factor(rownames(DEMENTED_Top8_rf_varImp2))
DEMENTED_Top8_rf_varImp2$labels <- reorder(DEMENTED_Top8_rf_varImp2$labels, DEMENTED_Top8_rf_varImp2$Overall)
DEMENTED_Top8_rf_varImp2
DEMENTED_Top8_rf_ImpVar <- rownames(DEMENTED_Top8_rf_varImp2)

# plot the importance
ggplot(data = DEMENTED_Top8_rf_varImp2, aes(x = labels,y = Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + 
  geom_point(color='blue') + 
  ylab("Importance Score") +
  xlab("Variables") +
  ggtitle("Feature Importance of Random Forest") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  PlotTheme +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))


#########################################################################################
#################### Decision Tree ######################################################
#########################################################################################
tree_Demented_Top8 <- rpart(DEMENTED ~., data = DEMENTED_Top8_train, method = "class")
# identify best cp value to use
best_Demented_Top8 <- tree_Demented_Top8$cptable[which.min(tree_Demented_Top8$cptable[,"xerror"]),"CP"]
pruned_tree_Demented_Top8 <- prune(tree_Demented_Top8, cp=best_Demented_Top8)
# Visualize the decision tree with rpart.plot
prp(pruned_tree_Demented_Top8)
# plot decision tree using custom arguments
prp(pruned_tree_Demented_Top8,
    faclen = 1, # use full names for factor labels
    extra = 1, # display number of observations for each terminal node
    roundint = F) # don't round to integers in output
#    digits = 3) # display 5 decimal places in output

rpart.plot(pruned_tree_Demented_Top8, type = 4)


# Testing the training model
tree_Demented_Top8_train_pred <- predict(object = tree_Demented_Top8, DEMENTED_Top8_train[-1], type="class")
# Calculating accuracy
mean(DEMENTED_Top8_train$DEMENTED == tree_Demented_Top8_train_pred) # 0.9207773
table(DEMENTED_Top8_train$DEMENTED, tree_Demented_Top8_train_pred)
# tree_Demented_Top8_train_pred      # rwo is whatever is in the front
#      0    1    3
# 0    5   13   39
# 1    0   84   36
# 3    5   13 1143
confusionMatrix(DEMENTED_Top8_train$DEMENTED, tree_Demented_Top8_train_pred)
#                Reference
# Prediction    0    1    3
#          0    5   13   39
#          1    0   84   36
#          3    5   13 1143

#Testing the test model
tree_Demented_Top8_test_pred <- predict(object = tree_Demented_Top8, DEMENTED_Top8_test[-1], type="class")
#Calculating accuracy
mean(DEMENTED_Top8_test$DEMENTED == tree_Demented_Top8_test_pred)  # 0.8862275
table(DEMENTED_Top8_test$DEMENTED, tree_Demented_Top8_test_pred)
confusionMatrix(DEMENTED_Top8_test$DEMENTED, tree_Demented_Top8_test_pred)
#              Reference
# Prediction   0   1   3
#          0   0   7   7
#          1   0  17  13
#          3   3   8 279

DEMENTED_Top8_dt_varImp <- varImp(tree_Demented_Top8)
DEMENTED_Top8_dt_varImp2 <- DEMENTED_Top8_dt_varImp[order(-DEMENTED_Top8_dt_varImp$Overall), , drop = FALSE]
DEMENTED_Top8_dt_varImp2$labels <- factor(rownames(DEMENTED_Top8_dt_varImp2))
DEMENTED_Top8_dt_varImp2$labels <- reorder(DEMENTED_Top8_dt_varImp2$labels, DEMENTED_Top8_dt_varImp2$Overall)
DEMENTED_Top8_dt_ImpVar <- rownames(DEMENTED_Top8_dt_varImp2)

# plot the importance
ggplot(data= DEMENTED_Top8_dt_varImp2, aes(x = labels,y = Overall)) +
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

DEMENTED_Top8_ImpVar_Com <- data.frame(unlist(DEMENTED_Top8_nn_ImpVar),
                                       unlist(DEMENTED_Top8_rf_ImpVar), 
                                       unlist(DEMENTED_Top8_dt_ImpVar))
names(DEMENTED_Top8_ImpVar_Com) <- c("NeuralNetwork","RandomForest", "Decision Tree")
View(DEMENTED_Top8_ImpVar_Com)
write_xlsx(DEMENTED_Top8_ImpVar_Com,"Important_Factors_Top8.xlsx")

par(pty="s") 
nnROC <- multiclass.roc(DEMENTED_Top8_test$DEMENTED ~ as.numeric(nn_DEMENTED_Top8_test_predictions),
             plot=TRUE,
             print.auc=TRUE,
             col="green",
             lwd =4,
             legacy.axes=TRUE,
             main="ROC Curves")

rfROC <- multiclass.roc(DEMENTED_Top8_test$DEMENTED ~ as.numeric(rf_DEMENTED_Top8_test_predicted),
                        plot=TRUE,
                        print.auc=TRUE,
                        col="blue",
                        lwd = 4,
                        print.auc.y=0.4,
                        legacy.axes=TRUE,
                        add = TRUE)

dtROC <- multiclass.roc(DEMENTED_Top8_test$DEMENTED ~ as.numeric(tree_Demented_Top8_test_pred),
                        plot=TRUE,
                        print.auc=TRUE,
                        col="red",
                        lwd = 4,
                        print.auc.y=0.3,
                        legacy.axes=TRUE,
                        add = TRUE)


###############################################################################################################
####################### Recursive Feature Elimination usinf random forest
###############################################################################################################

# ImpVarList_RFE <- c('ProgStatus', 'Centiloid_fSUVR_TOT_CORTMEAN', 'TOTAL_HIPPOCAMPUS_VOLUME', 'Left.Amygdala_volume',
#                     'Right.Amygdala_volume', 'lh_middletemporal_thickness', 'Left.Accumbens.area_volume', 
#                     'lh_temporalpole_thickness', 'lh_superiortemporal_thickness', 'Right.Inf.Lat.Vent_volume',
#                     'SubCortGrayVol', 'rh_inferiortemporal_thickness','CC_Posterior_volume', 'X4th.Ventricle_volume')
DEMENTED_RFE<- final_NoNA_DEMENTED2 %>% dplyr::select(DEMENTED,
                                                       Centiloid_fSUVR_TOT_CORTMEAN,
                                                       TOTAL_HIPPOCAMPUS_VOLUME,
                                                       Left.Amygdala_volume,
                                                       Right.Amygdala_volume,
                                                       lh_middletemporal_thickness,
                                                       Left.Accumbens.area_volume,
                                                       lh_temporalpole_thickness,
                                                       lh_superiortemporal_thickness,
                                                       Right.Inf.Lat.Vent_volume,
                                                       SubCortGrayVol,
                                                       rh_inferiortemporal_thickness,
                                                       CC_Posterior_volume,
                                                       X4th.Ventricle_volume,
                                                      lh_transversetemporal_thickness,
                                                      lh_parahippocampal_thickness
                                                        )
set.seed(4365677)
DEMENTED_RFE_sampling_vector <- createDataPartition(DEMENTED_RFE$DEMENTED, p = 0.80, list = FALSE)
DEMENTED_RFE_train <- DEMENTED_RFE[DEMENTED_RFE_sampling_vector,]
DEMENTED_RFE_test <- DEMENTED_RFE[-DEMENTED_RFE_sampling_vector,]

rf_DEMENTED_RFE_model <- randomForest(DEMENTED ~ ., data = DEMENTED_RFE_train, proximity = TRUE) 

# Training data
rf_DEMENTED_RFE_train_predicted <- predict(rf_DEMENTED_RFE_model, DEMENTED_RFE_train)
mean(rf_DEMENTED_RFE_train_predicted  == DEMENTED_RFE_train$DEMENTED)   # 1
# confusionMatrix(rf_DEMENTED_RFE_train_predicted, DEMENTED_RFE_train$DEMENTED)


# Test data
rf_DEMENTED_RFE_test_predicted <- predict(rf_DEMENTED_RFE_model, DEMENTED_RFE_test)
mean(rf_DEMENTED_RFE_test_predicted  == DEMENTED_RFE_test$DEMENTED)   # 0.828125
