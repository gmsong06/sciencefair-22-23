# this file combines the feature importance for 
# file 1: important feature for DEMENTED status: DEMENTED_varimp_data
# file 5.5: progression with all visits: VisitProg_varimp_data
# file 6: progression with Day 0: Day_Zero_varimp_data 

load("DEMENTED_varimp_data.RData")
load("VisitProg_varimp_data.RData")
load("Day_Zero_varimp_data.RData")

load("DEMENTED_treeBag_varimp_data.RData")
load("VisitProg_treeBag_varimp_data.RData")
load("Day_Zero_treeBag_varimp_data.RData")

DEMENTED_varimp_data2 <- DEMENTED_varimp_data
VisitProg_varimp_data2 <- VisitProg_varimp_data
Day_Zero_varimp_data2 <- Day_Zero_varimp_data

DEMENTED_treeBag_varimp_data2 <- DEMENTED_treeBag_varimp_data
VisitProg_treeBag_varimp_data2 <- VisitProg_treeBag_varimp_data
Day_Zero_treeBag_varimp_data2 <- Day_Zero_treeBag_varimp_data

DEMENTED_varimp_data2$type <- "DEMENTED_rf"
VisitProg_varimp_data2$type <- "VisitProg_rf"
Day_Zero_varimp_data2$type <- "DayZero_rf"

DEMENTED_treeBag_varimp_data2$type <- "DEMENTED_tb"
VisitProg_treeBag_varimp_data2$type <- "VisitProg_tb"
Day_Zero_treeBag_varimp_data2$type <- "DayZero_tb"

VarImpAll <- rbind(DEMENTED_varimp_data2, 
                   VisitProg_varimp_data2, 
                   Day_Zero_varimp_data2, 
                   DEMENTED_treeBag_varimp_data2,
                   VisitProg_treeBag_varimp_data2,
                   Day_Zero_treeBag_varimp_data2)
view(VarImpAll)

FeatureFre <- table(VarImpAll$feature)
is.data.frame(FeatureFre)
str(FeatureFre)
FeatureFre_sort <- FeatureFre[order(-FeatureFre)] 
FeatureFre_sort
FeatureFre_sort2 <- as.data.frame(FeatureFre_sort)
FeatureFre_sort2
save(FeatureFre_sort2, file = "FeatureFre_sort2.RData")
load("FeatureFre_sort2.RData")

VarImp_Prog <- rbind(VisitProg_varimp_data2, 
                   Day_Zero_varimp_data2, 
                   VisitProg_treeBag_varimp_data2,
                   Day_Zero_treeBag_varimp_data2)
view(VarImp_Prog)
FeatureFre_Prog <- table(VarImp_Prog$feature)
is.data.frame(FeatureFre_Prog)
str(FeatureFre_Prog)
FeatureFre_Prog_sort <- FeatureFre_Prog[order(-FeatureFre_Prog)] 
FeatureFre_Prog_sort
FeatureFre_Prog_sort2 <- as.data.frame(FeatureFre_Prog_sort)
FeatureFre_Prog_sort2
save(FeatureFre_Prog_sort2, file = "FeatureFre_Prog_sort2.RData")
load("FeatureFre_Prog_sort2.RData")
