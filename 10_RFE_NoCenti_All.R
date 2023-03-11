######### This file is focused on Recursive Feature Elimination for all three:
# DEMENTED: Load data from file 1;
# VisitProg: load data from file 5.5
# Day_Zero: Load data from file 6

#### All the files use two models: Random Forest and Tree Bagging 


ref_size <- c(1:20, 30, 40, 50, 60, 80)
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

treeBagcontrol <- rfeControl(functions = treebagFuncs, # random forest
                             method = "repeatedcv", # repeated cv
                             repeats = 5, # number of repeats
                             number = 10) # number of folds

#############################################################################################################
############################ DEMENTED: From File 1 ##########################################################
#############################################################################################################

####################################################
#######################Random Forest ###############
####################################################

load("DEMENTED_train.RData")
load("DEMENTED_test.RData")

x_DEMENTED_train <- DEMENTED_train[, -1]
x_DEMENTED_train_NoCenti <- x_DEMENTED_train[, -1]
y_DEMENTED_train <- DEMENTED_train[, 1]

x_DEMENTED_test <- DEMENTED_test[, -1]
x_DEMENTED_test_NoCenti <- x_DEMENTED_test[, -1]
y_DEMENTED_test <- DEMENTED_test[, 1]



DEMENTED_rfe_Nocenti <- rfe(x = x_DEMENTED_train_NoCenti, 
                     y = y_DEMENTED_train, 
                     sizes = ref_size,
                     rfeControl = control)

save(DEMENTED_rfe_Nocenti, file = "DEMENTED_rfe_Nocenti.RData")
load("DEMENTED_rfe_Nocenti.RData")

DEMENTED_rfe_Nocenti

ggplot(data = DEMENTED_rfe_Nocenti, metric = "Accuracy") +
  theme_bw() +
  theme(axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text = element_text(size = 14, face = "bold"))

