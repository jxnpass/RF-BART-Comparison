library(tidymodels)
library(tidyverse)
library(randomForest)
library(BART3)
library(caret)
library(pROC)

cores <- 4

## Import data -------
setwd('RF-BART-Comparison/ex2-diabetes')
diabetes <- read_csv("diabetes_prediction_dataset.csv") %>% 
  mutate(diabetes = ifelse(diabetes == 1, T, F)) %>% 
  mutate(diabetes = as.factor(diabetes))
num_features <- c("age", "bmi", "HbA1c_level", "blood_glucose_level")
cat_features <- c("gender", "hypertension", "heart_disease", "smoking_history")
## Data split -------

data_split <- initial_split(diabetes, strata = "diabetes", prop = 0.75)
d_train <- training(data_split)
d_test  <- testing(data_split)

X_train <- d_train %>% select(-diabetes)
y_train <- d_train$diabetes
X_test <- d_test %>% select(-diabetes)
y_test <- d_test$diabetes

# How is our features?
# numerical
eda_sample <- sample(size = nrow(d_test)*.2, x = 1:nrow(d_test))
diabetes_eda_num <- d_test[eda_sample,c(num_features, "diabetes")] %>% gather(num_features, key = "feature", value = "value")
num_plot <- ggplot(data = diabetes_eda_num, aes(x = value, y = diabetes)) +
  facet_wrap(~ feature, scales = "free_x") +
  geom_jitter(height = .2, width = .2) +
  geom_smooth(mapping = aes(group = feature), se = F, method = "loess") + 
  labs(title = "Continuous Features on Diabetes",
       x = "Value",
       y = "Diabetic")

# categorical
diabetes_eda_cat <- d_test[eda_sample,c(cat_features, "diabetes")]
diabetes_counts <- diabetes_eda_cat %>% group_by(diabetes) %>% summarize(n = n()) %>% ungroup() 
hypertension_counts <- diabetes_eda_cat %>% mutate(hypertension = ifelse(hypertension == 1, "Hypertension", "No Hypertension")) %>% 
  group_by(diabetes, hypertension) %>% summarize(count = n()) %>% 
  mutate(prop = ifelse(diabetes == T, count/diabetes_counts$n[2], count/diabetes_counts$n[1])) %>% ungroup() %>% 
  rename(feature = hypertension) %>% 
  filter(feature == "Hypertension")
heart_disease_counts <- diabetes_eda_cat %>% mutate(heart_disease = ifelse(heart_disease == 1, "Heart Disease", "No Heart Disease")) %>% 
  group_by(diabetes, heart_disease) %>% summarize(count = n()) %>% 
  mutate(prop = ifelse(diabetes == T, count/diabetes_counts$n[2], count/diabetes_counts$n[1])) %>% ungroup() %>% 
  rename(feature = heart_disease) %>% 
  filter(feature == "Heart Disease")
smoking_history_counts <- diabetes_eda_cat %>% 
  mutate(smoking_history = ifelse(smoking_history %in% c("current", "former"), "Current/Former Smoker", "Non-Smoker")) %>% 
  group_by(diabetes, smoking_history) %>% summarize(count = n()) %>%
  mutate(prop = ifelse(diabetes == T, count/diabetes_counts$n[2], count/diabetes_counts$n[1])) %>% ungroup() %>%
  rename(feature = smoking_history) %>%
  filter(feature == "Current/Former Smoker")

diabetes_eda_cat <- rbind(hypertension_counts, heart_disease_counts, smoking_history_counts)

cat_plot <- ggplot(data = diabetes_eda_cat, aes(x = feature, y = prop, fill = diabetes)) +
  geom_col(position = "dodge") +
  labs(title = "Categorical Features on Diabetes",
       x = "Feature",
       y = "Marginal Proportion",
       fill = "Diabetic")

ex2 <- ggpubr::ggarrange(num_plot, cat_plot, ncol = 2)

ggsave(filename = "~/General_Resarch/RF-BART-Comparison/ex2-diabetes/graphics/1-ex2graph.png",
       device = "png", plot = ex2, width = 11, height = 7, units = "in")

# Tuning ------
# tuning parameters
ntree_arg <- c(100,250,500)
mtrys <- seq(2, ncol(X_train), by = 2)
nodes <- c(1, 5, 10, 20)

# set up grid
cv <- 5
param_grid_rf <- expand.grid('ntree' = ntree_arg, 'mtry' = mtrys, 'node' = nodes)

# DiabetesGridCV <- function(X, y, param_grid, cv = 1, model = "RF", sampsize = .75) {
#   
#   n <- nrow(X)
#   valid_results <- param_grid %>% mutate(accuracy = NA, sensitivity = NA, 
#                                          specificity = NA, comp_time = NA) # for cross validation
#   
#   print(paste("Performing Cross-Validation model fits", cv, "times for each unique set of parameters..."))
#   for (i in 1:nrow(param_grid)) {
#     
#     if (model == "RF") {
#       # identify params on grid
#       ntree <- param_grid$ntree[i]
#       mtry <- param_grid$mtry[i]
#       node <- param_grid$node[i]
#       
#       print(paste("RandomForest Fit: Ntree = ", ntree, ", Mtry = ", mtry, ", Node = ", node, sep = ""))
#     }
#     
#     else if (model == "BART") {
#       # identify params on grid
#       ntree <- param_grid$ntree[i]
#       base <- param_grid$base[i]
#       power <- param_grid$power[i]
#       k <- param_grid$k[i]
# 
#       print(paste("BART Fit: Ntree = ", ntree, ", base = ", base, ", power = ", 
#                   power, ", k = ", k, sep = ""))
#       
#     }
#     # starting with cross validation
#     results_cv <- data.frame(cv = 1:cv, accuracy = NA, sensitivity = NA, 
#                              specificity = NA, comp_time = NA)
#     
#     for (n_cv in 1:cv) {
#       
#       samp <- createDataPartition(y, p = sampsize, list = FALSE, times = 1)
#       
#       X_train = X[samp,] 
#       y_train = y[samp] 
#       X_valid = X[-samp,] 
#       y_valid = y[-samp] 
#       
#       # randomForest fit
#       start_time <- Sys.time()
#       if (model == "RF") {
#         my_model <- randomForest(X_train, y_train, ntree = ntree, mtry = mtry, nodesize = node)
#       }
#       else if (model == "BART") {
#         my_model <- mc.pbart(X_train, y_train, ntree = ntree, base = base, power = power, k = k, 
#                             mc.cores = 4, printevery = 0)
#       }
#       end_time <- Sys.time()
#       results_cv[n_cv, c('comp_time')] <- end_time - start_time
#       
#       # check preds on validation set
#       if (model == "RF") {
#         y_pred_cv <- predict(my_model, X_valid) 
#         results = data.frame(y_pred_cv, y_valid)
#       }
#       else if (model == "BART") {
#         y_pred_cv <- as.numeric(predict(my_model, X_valid, mc.cores = 4)$prob.test.mean > .5)
#         results = data.frame(y_pred_cv, y_valid) %>% 
#           mutate(y_pred_cv = as.factor(y_pred_cv),
#                  y_valid = as.factor(y_valid))
#       }
#       
#       
#       # save metrics
#       confmat <- (results %>% conf_mat(., truth = y_valid, estimate = y_pred_cv))$table
#       results_cv[n_cv, c('accuracy')] <- (confmat[1,1] + confmat[2,2]) / (confmat[1,1] + confmat[1,2] + confmat[2,1] + confmat[2,2])
#       results_cv[n_cv, c('sensitivity')] <- confmat[2,2]/(confmat[2,2] + confmat[1,2])
#       results_cv[n_cv, c('specificity')] <- confmat[1,1]/(confmat[1,1] + confmat[2,1])
#       
#       }
#     
#     valid_results[i, c('accuracy', 'sensitivity', 'specificity', 'comp_time')] <- results_cv %>% 
#       summarise(accuracy = mean(accuracy), sensitivity = mean(sensitivity),
#                 specificity = mean(specificity), comp_time = mean(comp_time))
#   }
#   
#   # Finish CV  
#   print("Cross-Validation Complete")
#   
#   return(valid_results)
# }

# cv_results_rf <- DiabetesGridCV(X_train, y_train, param_grid = param_grid_rf, cv = 1)
# cv_results_rf[which.max(cv_results_rf$accuracy),]
# write.csv(file = "~/General_Resarch/SRC2024/ex2-diabetes/rf_cv_diabetes.csv", x = cv_results_rf)
# ntree = 250, mtry = 2, node = 5

### Best RF

rf <- randomForest(diabetes ~ ., data = d_train, ntree = 250, mtry = 2, node = 5)

rf_preds <- predict(rf, X_test) %>% as_tibble()

preds_rf_df = cbind(y_test, rf_preds) %>% 
  as_tibble() %>% 
  rename(estimate = value, truth = y_test) 

preds_rf_df %>% 
  metrics(., truth = truth, estimate = estimate)

rf_confmat <- conf_mat(preds_rf_df, truth = truth, estimate = estimate)$table
rf_accuracy <- (rf_confmat[1,1] + rf_confmat[2,2]) / (rf_confmat[1,1] + rf_confmat[1,2] + rf_confmat[2,1] + rf_confmat[2,2])
rf_sensitivity <- rf_confmat[2,2]/(rf_confmat[2,2] + rf_confmat[1,2])
rf_specificity <- rf_confmat[1,1]/(rf_confmat[1,1] + rf_confmat[2,1])

# ROC

rf_votes <- predict(rf, X_test, type = "vote")[,2]

rf_roc_curve = roc(as.numeric(preds_rf_df$truth)-1, rf_votes)
rf_auc_value <- round(auc(rf_roc_curve), 3)
rf_roc_spec = rf_roc_curve$specificities
rf_roc_sens = rf_roc_curve$sensitivities

# BART ---------

base <- c(.5, .75, .95) # a ~ (0,1) - initial probability of split, default = .95
power = c(.5,1,2) # b ~ (0, inf) 
# µ
k <- c(1,2,3,5)

# Other tuning
ntree_arg <- c(50,100,200)
cv <- 1

param_grid_bart <- expand.grid('ntree' = ntree_arg, 
                               'base' = base, 
                               'power' = power,
                               'k' = k)

d_train <- d_train %>% 
  mutate(diabetes = as.numeric(diabetes)-1) 
d_test <- d_test %>% 
  mutate(diabetes = as.numeric(diabetes)-1)

bart_lm <- lm(diabetes ~ -1+., data = d_train)
X_train_bart <- model.matrix(bart_lm)
bart_lm <- lm(diabetes ~ -1+., data = d_test)
X_test_bart <- model.matrix(bart_lm)
y_train_bart <- as.numeric(y_train)-1
y_test_bart <- as.numeric(y_test)-1

# cv_results_bart <- DiabetesGridCV(X_train_bart, y_train_bart, param_grid = param_grid_bart, model = "BART")
# cv_results_bart[which.max(cv_results_bart$accuracy),]
# write.csv(file = "~/General_Resarch/SRC2024/ex2-diabetes/bart_cv_diabetes.csv", x = cv_results_bart)

# all performances were almost exactly the same, so I'll use the least computationally intensive
# cv_results_bart[which.min(cv_results_bart$comp_time),]

bart <- mc.pbart(X_train_bart, y_train_bart, ntree = 100, base = .75, power = .5, k = 3, mc.cores = cores)
bart_preds_tb <- predict(bart, X_test_bart)
bart_votes <- bart_preds_tb$prob.test.mean

preds_bart_df = cbind(y_test_bart, bart_votes) %>% 
  as_tibble() %>% 
  rename(estimate_prob = bart_votes, truth = y_test_bart) %>% 
  mutate(estimate = ifelse(estimate_prob >= .5, 1, 0))

preds_bart_df %>% 
  mutate(truth = as.factor(truth), estimate = as.factor(estimate)) %>% 
  metrics(., truth = truth, estimate = estimate)

bart_confmat <- preds_bart_df %>%   
  mutate(truth = as.factor(truth), estimate = as.factor(estimate)) %>% 
  conf_mat(truth = truth, estimate = estimate)
bart_confmat <- bart_confmat$table

bart_accuracy <- (bart_confmat[1,1] + bart_confmat[2,2]) / (bart_confmat[1,1] + bart_confmat[1,2] + bart_confmat[2,1] + bart_confmat[2,2])
bart_sensitivity <- bart_confmat[2,2]/(bart_confmat[2,2] + bart_confmat[1,2])
bart_specificity <- bart_confmat[1,1]/(bart_confmat[1,1] + bart_confmat[2,1])

# Bonus graph on how BART does posterior predictive probabilities
print(idx <- sample(1:nrow(d_test), size = 1))
d_test[idx,]
# diabetes = TRUE includes idx = 3, 6061
d_ind_post_preds = predict(bart, matrix(X_test_bart[idx,], nrow = 1))
ggplot() + 
  geom_histogram(mapping = aes(x = d_ind_post_preds$prob.test)) +
  labs(title = paste("Posterior Predictive on Individual with Diabetes = ", 
                     d_test$diabetes[idx]),
       subtitle = paste(d_test$age[idx], " year-old ", d_test$gender[idx], " with ", 
                     "hypertension = ", d_test$hypertension[idx], 
                     ", heart disease = ", d_test$heart_disease[idx], 
                     ", smoking history = ", d_test$smoking_history[idx],
                     ", bmi = ", d_test$bmi[idx], 
                     ", HbA1c_level = ", d_test$HbA1c_level[idx], 
                     ", BGL = ", d_test$blood_glucose_level[idx], sep = ""),
       x = "Predictive Probability of Diabetic", y = "Count") 


# ROC
bart_roc_curve = roc(preds_bart_df$truth, preds_bart_df$estimate_prob)
bart_auc_value <- round(auc(bart_roc_curve), 3)
bart_roc_spec = bart_roc_curve$specificities
bart_roc_sens = bart_roc_curve$sensitivities

# Comparing ROC Curves ------
roc_curve_plot <- ggplot() +
  geom_line(aes(x = 1-rf_roc_spec, y = rf_roc_sens, color = "RF: 0.961")) +
  geom_line(aes(x = 1-bart_roc_spec, y = bart_roc_sens, color = "BART: 0.978")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve", x = "1-Specificity", y = "Sensitivity", color = "AUC") +
  scale_color_manual(values = c("RF: 0.961" = "red3", "BART: 0.978" = "pink1")) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(filename = "~/General_Resarch/RF-BART-Comparison/ex2-diabetes/graphics/2-roccurves.png",
       device = "png", plot = roc_curve_plot, width = 11, height = 7, units = "in")

