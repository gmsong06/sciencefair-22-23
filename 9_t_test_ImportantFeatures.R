###############
###### t test for the important factors 
# Among all the 85 features, the following features have been shown to be top 20 features by all 6 models:
# Var1 Freq
# 1       Centiloid_fSUVR_TOT_CORTMEAN    6
# 2         Left.Accumbens.area_volume    6
# 3               Left.Amygdala_volume    6
# 4        lh_middletemporal_thickness    6
# 5      lh_superiortemporal_thickness    6
# 6              Right.Amygdala_volume    6
# 7          Right.Inf.Lat.Vent_volume    6
# 8           TOTAL_HIPPOCAMPUS_VOLUME    6
# 9      rh_inferiortemporal_thickness    5
# 10                    SubCortGrayVol    5

##### Load file: from File 1

load("final_NoNA.RData")

str(final_NoNA)

################################################################################################
########################### CN VS AD
################################################################################################

final_NoNA_2groups <- filter(final_NoNA, DEMENTED != "0")
final_NoNA_2groups$DEMENTED <- as.character(final_NoNA_2groups$DEMENTED)
final_NoNA_2groups$DEMENTED <- as.factor(final_NoNA_2groups$DEMENTED)


#### Centiloid_fSUVR_TOT_CORTMEAN
t.test(Centiloid_fSUVR_TOT_CORTMEAN ~ DEMENTED, data = final_NoNA_2groups)
# Welch Two Sample t-test
# 
# data:  Centiloid_fSUVR_TOT_CORTMEAN by DEMENTED
# t = 14.123, df = 164.79, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group 1 and group 3 is not equal to 0
# 95 percent confidence interval:
#   44.77122 59.32463
# sample estimates:
#   mean in group 1 mean in group 3 
# 69.47546        17.42753

t.test(TOTAL_HIPPOCAMPUS_VOLUME ~ DEMENTED, data = final_NoNA_2groups)   
# Welch Two Sample t-test
# 
# data:  TOTAL_HIPPOCAMPUS_VOLUME by DEMENTED
# t = -13.857, df = 170.28, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group 1 and group 3 is not equal to 0
# 95 percent confidence interval:
#   -1600.120 -1201.078
# sample estimates:
#   mean in group 1 mean in group 3 
# 6252.413        7653.013 

t.test(Left.Amygdala_volume ~ DEMENTED, data = final_NoNA_2groups)   
# Welch Two Sample t-test
# 
# data:  Left.Amygdala_volume by DEMENTED
# t = -9.1863, df = 168.36, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group 1 and group 3 is not equal to 0
# 95 percent confidence interval:
#   -293.9165 -189.9356
# sample estimates:
#   mean in group 1 mean in group 3 
# 1241.702        1483.628 

t.test(Right.Amygdala_volume ~ DEMENTED, data = final_NoNA_2groups) 
# Welch Two Sample t-test
# 
# data:  Right.Amygdala_volume by DEMENTED
# t = -10.556, df = 170.63, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group 1 and group 3 is not equal to 0
# 95 percent confidence interval:
#   -331.7820 -227.2406
# sample estimates:
#   mean in group 1 mean in group 3 
# 1274.862        1554.373

t.test(Left.Accumbens.area_volume ~ DEMENTED, data = final_NoNA_2groups) 
# Welch Two Sample t-test
# 
# data:  Left.Accumbens.area_volume by DEMENTED
# t = -9.4662, df = 181.67, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group 1 and group 3 is not equal to 0
# 95 percent confidence interval:
#   -122.67358  -80.35485
# sample estimates:
#   mean in group 1 mean in group 3 
# 401.0087        502.5229

t.test(Right.Inf.Lat.Vent_volume ~ DEMENTED, data = final_NoNA_2groups) 
# Welch Two Sample t-test
# 
# data:  Right.Inf.Lat.Vent_volume by DEMENTED
# t = 9.0111, df = 155.95, p-value = 6.887e-16
# alternative hypothesis: true difference in means between group 1 and group 3 is not equal to 0
# 95 percent confidence interval:
#   525.6646 820.8232
# sample estimates:
#   mean in group 1 mean in group 3 
# 1175.0033        501.7595

t.test(lh_middletemporal_thickness ~ DEMENTED, data = final_NoNA_2groups) 
# Welch Two Sample t-test
# 
# data:  lh_middletemporal_thickness by DEMENTED
# t = -11.026, df = 166.22, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group 1 and group 3 is not equal to 0
# 95 percent confidence interval:
#   -0.2280093 -0.1587563
# sample estimates:
#   mean in group 1 mean in group 3 
# 2.518540        2.711923

t.test(lh_superiortemporal_thickness ~ DEMENTED, data = final_NoNA_2groups)
# Welch Two Sample t-test
# 
# data:  lh_superiortemporal_thickness by DEMENTED
# t = -10.115, df = 170.43, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group 1 and group 3 is not equal to 0
# 95 percent confidence interval:
#   -0.2082870 -0.1402665
# sample estimates:
#   mean in group 1 mean in group 3 
# 2.428053        2.602330 

t.test(rh_inferiortemporal_thickness ~ DEMENTED, data = final_NoNA_2groups)
# Welch Two Sample t-test
# 
# data:  rh_inferiortemporal_thickness by DEMENTED
# t = -9.4312, df = 166.25, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group 1 and group 3 is not equal to 0
# 95 percent confidence interval:
#   -0.2092371 -0.1367981
# sample estimates:
#   mean in group 1 mean in group 3 
# 2.550113        2.723131

t.test(SubCortGrayVol ~ DEMENTED, data = final_NoNA_2groups)
# Welch Two Sample t-test
# 
# data:  SubCortGrayVol by DEMENTED
# t = -10.505, df = 699.47, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group 1 and group 3 is not equal to 0
# 95 percent confidence interval:
#   -7221.363 -4947.136
# sample estimates:
#   mean in group 1 mean in group 3 
# 48497.64        54581.89 



###############################################################################################3
############################ progress
################################################################################################
load("VisitProg2.RData")
str(VisitProg2)

ggplot(data = VisitProg2, mapping = aes(y = Centiloid_fSUVR_TOT_CORTMEAN,                     
                                          x = ProgStatus, color = ProgStatus)) +                      
  geom_boxplot()+ 
  #geom_jitter() +
  PlotTheme +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("green4", "#FC4E07", "#E7B800")) +
  scale_color_manual(values = c("green4", "#FC4E07", "#E7B800")) +
  LegendTheme +
  labs(title = "Centiloid_fSUVR_TOT_CORTMEAN")

ggplot(data = VisitProg2, mapping = aes(y = TOTAL_HIPPOCAMPUS_VOLUME,                     
                                        x = ProgStatus, color = ProgStatus)) +                      
  geom_boxplot()+ 
  #geom_jitter() +
  PlotTheme +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("green4", "#FC4E07", "#E7B800")) +
  scale_color_manual(values = c("green4", "#FC4E07", "#E7B800")) +
  LegendTheme +
  labs(title = "TOTAL_HIPPOCAMPUS_VOLUME")

ggplot(data = VisitProg2, mapping = aes(y =  Right.Amygdala_volume,                     
                                        x = ProgStatus, color = ProgStatus)) +                      
  geom_boxplot()+ 
  #geom_jitter() +
  PlotTheme +
  #  scale_fill_discrete(name = "Cognition Condition", labels = c("Not Demented", "Demented", "Normal Cognition")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "green4")) +
  LegendTheme +
  labs(title = "Right.Amygdala_volume")

VisitProg2_2groups <- filter(VisitProg2, ProgStatus != "DMN")
VisitProg2_2groups$ProgStatus<- as.character(VisitProg2_2groups$ProgStatus)
VisitProg2_2groups$ProgStatus <- as.factor(VisitProg2_2groups$ProgStatus)

t.test(Centiloid_fSUVR_TOT_CORTMEAN ~ ProgStatus, data = VisitProg2_2groups)
t.test(Left.Accumbens.area_volume ~ ProgStatus, data = VisitProg2_2groups)
t.test(Left.Amygdala_volume ~ ProgStatus, data = VisitProg2_2groups)
t.test(lh_middletemporal_thickness ~ ProgStatus, data = VisitProg2_2groups)
t.test(lh_superiortemporal_thickness ~ ProgStatus, data = VisitProg2_2groups)
t.test(Right.Amygdala_volume ~ ProgStatus, data = VisitProg2_2groups)
t.test(Right.Inf.Lat.Vent_volume ~ ProgStatus, data = VisitProg2_2groups)
t.test(TOTAL_HIPPOCAMPUS_VOLUME ~ ProgStatus, data = VisitProg2_2groups)
t.test(rh_inferiortemporal_thickness ~ ProgStatus, data = VisitProg2_2groups)
t.test(SubCortGrayVol ~ ProgStatus, data = VisitProg2_2groups)


