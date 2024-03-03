setwd('RF-BART-Comparison/ex1_kneesleeve')
source('DataImport.R')
source("ModelFunctionsandMetrics.R")

library(tidymodels)
library(caret)

# Random Forest packages
library(randomForest)
library(reprtree)

# BART packages
library(BART)

set.seed(1)
theme_set(theme_classic())

# DATA PREP ---------------------------------------------------------------

f_maxes <- pred.Maxes(S1.flex01)
# choosing train, valid, and test sets from knee sleeve data
ex1 <- ggplot(data = S1.flex01, aes(x = time, y = angle)) +
  geom_line() +
  geom_point(data = f_maxes, mapping = aes(x = time, y = maxes), color = "black", size = 3, shape = 17) +
  geom_vline(xintercept = 45, color = "red", lty = 2) +
  labs(title = "Subject Knee Angle Over Exercise Period",
       subtitle = "Goal is to predict the black triangles",
       caption = "Red Line Indicates My Tuning/Testing Split",
       x = "Time", y = "Angle")

ggsave(filename = "~/General_Resarch/RF-BART-Comparison/ex1_kneesleeve/graphics/1-ex1graph.png",
       device = "png", plot = ex1, width = 6, height = 4, units = "in")

# Set train and test sets. (Train = calibration, Valid, Test = flex. Make sure subject number is the same)
df_train <- S1.calib01
df_valid_orig <- S1.flex01 %>% filter(time <= 45)
df_test_orig <- S1.flex01 %>% filter(time > 45)

# cleaning bad sensors, rescaling sensors, rescaling angle/target
df_valid <- TransformData(df_train, df_valid_orig, recalib = T, angle_shift = T)
df_test <- TransformData(df_train, df_test_orig, recalib = T, angle_shift = T)

# data setup
X <- df_train %>% select(contains('sensor'))
y <- df_train$angle

X_valid <- df_valid %>% select(contains('sensor')) 
keep_sensors_valid <- X_valid %>% colnames() # which sensors in both flex and calib are bad? identifies good ones
y_valid <- df_valid$angle

X_test <- df_test %>% select(contains('sensor')) 
keep_sensors_test <- X_test %>% colnames() 
y_test <- df_test$angle

X2 <- X[keep_sensors_test] # X2 for looking at model fit
X <- X[keep_sensors_valid] # X for tuning

# S1 RANDOM FOREST ----------------------------------------------------------------------

# tuning parameters
ntree_arg <- c(100,250,500)
mtrys <- 1:length(keep_sensors_valid) # 16 = number of sensors
nodes <- c(1, 5, 10, 20)

ntree_arg <- c(250,500)
mtrys <- c(3,5)
nodes <- c(5)

# set up grid
cv <- 5
param_grid_rf <- expand.grid('ntree' = ntree_arg, 'mtry' = mtrys, 'node' = nodes)
cv_results_rf <- KneeGridCV(X, y, X_valid, y_valid, df_valid, param_grid = param_grid_rf, cv = cv, model = "RF")

cv_results_rf[which.min(cv_results_rf$max_rmse),]
# best rf at. s1: I found ntree = 1000, mtry = 13, nodesize = 1

# Evaluate on test set
best_params_rf = data.frame('ntree' = 1000, 'mtry' = 13, 'nodesize' = 1)
AnalyzeRF(X, y, X_valid, y_valid, X_test, y_test, df_valid = df_valid, df_test = df_test, best_params = best_params_rf)  


# S1 BART -----------------------------------------------------------------

# tuning parameters

base <- c(.05, .25, .5, .75, .95) # a ~ (0,1) - initial probability of split, default = .95
power = c(.5,1,2,3) # b ~ (0, inf) 
# µ
k <- c(1,2,3)
# σ
sigquant <- c(.1, .5, .9, 1) # prior variance scale

# Other tuning
ntree_arg <- c(50,100,200)
cv <- 5 

param_grid_bart <- expand.grid('ntree' = ntree_arg, 'base' = base, 'power' = power,
                               'k' = k, 'sigquant' = sigquant)

cv_results_bart <- KneeGridCV(X, y, X_valid, y_valid, df_valid, param_grid = param_grid_bart, cv = cv, model = "BART")
cv_results_bart[which.min(cv_results_bart$max_rmse),]
# best bart at S1: ntree = 100, base = .9, power = 1, k = 3

best_params_bart = data.frame('ntree' = 100, 'base' = .9, 'power' = 1, 'k' = 3, 'sigquant' = 1)
AnalyzeBART(X, y, X_valid, y_valid, X_test, y_test, df_valid = df_valid, df_test = df_test, best_params = best_params_bart)  


