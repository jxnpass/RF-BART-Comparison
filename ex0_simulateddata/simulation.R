setwd('RF-BART-Comparison/ex0-diabetes')
library(tidyverse)
library(ggh4x)
library(tidymodels)
library(caret)

# Random Forest packages
library(randomForest)
library(reprtree)

# BART packages
library(BART)

set.seed(1)
theme_set(theme_classic())
cores <- 4 # change this value based on computer preferences

# Our simulated f(x)
f <- function(x) {
  y = x^2 + rnorm(length(x), mean = 0, sd = .5)
  # x^2 = pattern/trend
  # rnorm = simulated noise
  return(y)
}

### Data setup ------

x <- seq(0,3,by = .005)
y <- f(x)
x_2 <- data.frame(X = x, Y = y)

index_test <- sample(c(1:length(x)), size = .1*length(x)) 
x_2_train <- x_2[-index_test,]
x_2_test <- x_2[index_test,]

title1 <- expression(paste("x"^2, " curve with added noise from ", N(0,.5)))

ex0 <- ggplot(data = x_2, mapping = aes(x = X, y = Y, color = "Training Data")) + 
  geom_point(alpha = .25) +
  geom_point(data = x_2_test, aes(x = X, y = Y, color = "Test Data"), alpha = .5) +
  labs(title = title1, color = "") +
  scale_color_manual(values = c("Training Data" = "black", "Test Data" = "red")) +
  theme(legend.position = 'bottom')

# ggsave(filename = "~/General_Resarch/RF-BART-Comparison/ex0-simulateddata/graphics/1-ex0graph.png",
#       device = "png", plot = ex0, width = 6, height = 4, units = "in")

### Random Forest ----------

### Tuning

ntrees <- c(1,10, 50, 100, 250, 500, 750, 1000, 1250, 1500, 1750, 2000)
mtrys <- 1 # default, only one explanatory x
nodes <- c(1, 5, 20, 50, 100, 250, 500)

param_grid_rf <- expand.grid('ntree' = ntrees, 'node' = nodes)
results_df_rf <- param_grid_rf %>% mutate(rmse = NA, rsq = NA, mae = NA, comp_time = NA) 

## Approximately 10 seconds
for (i in 1:nrow(param_grid_rf)) {
  
  ntrees <- param_grid_rf$ntree[i]
  nodes <- param_grid_rf$node[i]
  start.time <- Sys.time()
  curr_rf <- randomForest(Y ~ X, data = x_2_train, ntree = ntrees, mtry = 1, nodesize = nodes)
  end.time <- Sys.time()
  preds_rf <- predict(curr_rf, x_2_test)
  
  results <- cbind(x_2_test, preds_rf) %>% metrics(truth = Y, estimate = preds_rf)
  results_df_rf[i,c('rmse','rsq','mae')] <- results$.estimate
  results_df_rf[i, c('comp_time')] <- end.time - start.time
  
}
# FYI: No validation set was used for this part of tuning. 

# write.csv(x = results_df_rf, file = "~/General_Resarch/SRC2024/ex0-simulateddata/rf_results.csv")

# Tuning Grid on Predictions
ggplot() +
  geom_raster(data = results_df_rf, mapping = aes(x = as.factor(ntree), y = as.factor(node), fill = rmse)) +
  labs(title = 'ntree and nodesize for tuning')

# Tuning grid on Computation Time
ggplot() +
  geom_raster(data = results_df_rf, mapping = aes(x = as.factor(ntree), y = as.factor(node), fill = comp_time)) +
  labs(title = 'ntree and nodesize for tuning')

rf_ct <- ggplot(data = results_df_rf, aes(x = as.factor(ntree), y = comp_time, color = as.factor(node))) +
  geom_point() +
  geom_line(aes(group = as.factor(node))) +
  labs(title = "Random Forest on Computation Time",
       color = "Node Size") +
  xlab("Number of Trees") + ylab("Computation Time (s)")

ggsave(filename = "~/General_Resarch/RF-BART-Comparison/ex0-simulateddata/graphics/2-rfcomptime.png",
       device = "png", plot = rf_ct, width = 6, height = 4, units = "in")



# Comparing all models on data for prediction

# Default
rf_default <- randomForest(Y ~ X, data = x_2_train)
rf_default_pred <- predict(rf_default, x_2_test)

rf_default_rmse = (rf_default_pred - x_2_test$Y)^2 %>% mean() %>% sqrt()

# plot.getTree(rf_default, k = 1)

# Most optimal (according to tuning)
rf_opt <- randomForest(Y ~ X, data = x_2_train, ntree = 500, nodesize = 50)
rf_opt_pred <- predict(rf_opt, x_2_test)

rf_opt_rmse = (rf_opt_pred - x_2_test$Y)^2 %>% mean() %>% sqrt()

# plot.getTree(rf_opt, k = 1)

# Too simple/underfit (and highest rmse)
rf_uf <- randomForest(Y ~ X, data = x_2_train, ntree = 500, nodesize = 500)
rf_uf_pred <- predict(rf_uf, x_2_test)

rf_uf_rmse = (rf_uf_pred - x_2_test$Y)^2 %>% mean() %>% sqrt()

# plot.getTree(rf_uf, k = 1)

# Too complex/overfit
rf_complex <- randomForest(Y ~ X, data = x_2_train, ntree = 1, nodesize = 1)
rf_complex_pred <- predict(rf_complex, x_2_test)

rf_complex_rmse = (rf_complex_pred - x_2_test$Y)^2 %>% mean() %>% sqrt()

# plot.getTree(rf_complex, k = 1)

# Plotting
title2 <- expression(paste("x"^2, " curve with Random Forest fits"))

ggplot(data = x_2, mapping = aes(x = X, y = Y)) + 
  geom_point(alpha = .25) +
  theme_classic() +
  labs(title = title2) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = rf_default_pred, color = "Default")) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = rf_opt_pred, color = "Tuned")) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = rf_uf_pred, color = "UnderFit")) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = rf_complex_pred, color = "OverFit")) +
  labs(color = "Model Type") +
  scale_color_manual(values = c("Default" = "blue", "Tuned" = "green4", "UnderFit" = "red3", "OverFit" = "gray10"))
  
plt_rf1 <- ggplot(data = x_2, mapping = aes(x = X, y = Y)) + 
  geom_point(alpha = .25) +
  theme_classic() +
  labs(title = "RF Default Fit", subtitle = "ntrees = 500, nodesize = 5", caption = paste("RMSE: ", round(rf_default_rmse, 2))) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = rf_default_pred), color = "blue")

plt_rf2 <- ggplot(data = x_2, mapping = aes(x = X, y = Y)) + 
  geom_point(alpha = .25) +
  theme_classic() +
  labs(title = "RF Best Fit", subtitle = "ntrees = 500, nodesize = 50", caption = paste("RMSE: ", round(rf_opt_rmse, 2))) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = rf_opt_pred), color = "green4")

plt_rf3 <- ggplot(data = x_2, mapping = aes(x = X, y = Y)) + 
  geom_point(alpha = .25) +
  theme_classic() +
  labs(title = "RF Underfit", subtitle = "ntrees = 500, nodesize = 500", caption = paste("RMSE: ", round(rf_uf_rmse, 2))) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = rf_uf_pred), color = "red3")

plt_rf4 <- ggplot(data = x_2, mapping = aes(x = X, y = Y)) + 
  geom_point(alpha = .25) +
  theme_classic() +
  labs(title = "RF Overfit", subtitle = "ntrees = 5, nodesize = 1", caption = paste("RMSE: ", round(rf_complex_rmse, 2))) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = rf_complex_pred), color = "gray10")

rf_preds_plot <- ggpubr::ggarrange(plt_rf1, plt_rf2, plt_rf3, plt_rf4)
ggsave(filename = "~/General_Resarch/RF-BART-Comparison/ex0-simulateddata/graphics/3-rfpreds.png",
       device = "png", plot = rf_preds_plot, width = 9, height = 5.5, units = "in")

### BART ---------

# Regarding priors:
# π(θ) = π((T1, M1),(T2, M2), . . . ,(Tm, Mm), σ).
# π wants:
# Each T small.
# Each µ small.
# A “nice” σ (smaller than least squares estimate).

# T
base <- c(.05, .25, .5, .75, .95) # a ~ (0,1) - initial probability of split, default = .95
power = c(.5,1,2,3) # b ~ (0, inf) - decreasing probability as depth increases, default = 2
# base/(1+d)^power = probability of split, d = depth of node

# µ
k <- c(1,2,3)
k <- 2
# mu ~ N(0, sigma^2) and sigma = .5/(k*sqrt(m)), m = # of trees; default k = 2
# higher k means small variance  

# σ
sigquant <- c(.05,.25,.5,.75,.9,1) # prior variance scale
sigquant <- .9
# or
# sigdf = 3 # v , default = 3
# sigest = sd(x_2$Y) # sigma hat, default = sd(y)
# sigquant = .9 # q, default = .9

# sigma^2 = v*lambda/chi^2_v

# Other tuning
ntrees <- c(1, 10, 50, 100, 200, 300)
ndpost = 1000
nskip = 100
keepevery = 1

# gonna do randomized search because of how many there are
param_grid_bart <- expand.grid('k' = k, 'base' = base, 'power' = power, 'sigquant' = sigquant, 'ntree' = ntrees) 
# param_grid_bart <- param_grid_bart[ sample(1:nrow(param_grid_bart), size = 84),]
results_df_bart <- param_grid_bart %>% mutate(rmse = NA, rsq = NA, mae = NA, comp_time = NA) 

# Approximately 30 minutes with 4 cores with all params
# Approximately 2 minutes with 4 cores with just ntree, base, and power
for (i in 1:nrow(param_grid_bart)) {
  
  # Tree-based priors
  base <- param_grid_bart$base[i]
  power <- param_grid_bart$power[i]

  # Node-based priors
  k <- param_grid_bart$k[i]

  # Sigma-based priors
  sigquant <- param_grid_bart$sigquant[i]
  
  # Other parameters
  ntrees <- param_grid_bart$ntree[i]

  # Fit model, calculate computation time
  start.time <- Sys.time()
  curr_bart <- mc.wbart(x.train = x_2_train$X, y.train = x_2_train$Y, 
                     # params
                     ntree = ntrees, 
                     # iterations
                     ndpost = ndpost, nskip = nskip, 
                     keepevery = keepevery, 
                     # priors
                     power = power, 
                     base = base, # tree complexity
                     k = k, # spread of node values
                     sigquant = sigquant,  # prior y-variance scale
                     printevery = 0,
                     # parallel
                     mc.cores = cores
    )
  end.time <- Sys.time()
  
  preds_bart <- predict(curr_bart, data.frame(X = x_2_test$X))
  preds_bart <- colMeans(preds_bart)
  
  results <- cbind(x_2_test, preds_bart) %>% metrics(truth = Y, estimate = preds_bart)
  results_df_bart[i,c('rmse','rsq','mae')] <- results$.estimate
  results_df_bart[i, c('comp_time')] <- end.time - start.time
}

# write.csv(x = results_df_bart, file = "~/General_Resarch/SRC2024/ex0-simulateddata/bart_results.csv")

# Tuning Grid on Predictions
for (sg in unique(param_grid_bart$sigquant)) {
  
  print(ggplot() +
    geom_raster(data = results_df_bart %>% 
                  filter(sigquant == sg), 
                mapping = aes(x = as.factor(ntree), y = as.factor(k), fill = rmse)) +
    labs(title = 'BART Tuning Results', subtitle = paste("Sigquant = ", sg)) +
      facet_nested_wrap(~ power + base, labeller = "label_both"))
}

# Tuning Grid on computation time
for (sg in unique(param_grid_bart$sigquant)) {
  print(ggplot(data = results_df_bart %>% 
           filter(sigquant == sg), aes(x = as.factor(ntree), y = comp_time, color = as.factor(base))) +
    facet_nested_wrap(~ power + k, labeller = "label_both", ncol = 3) +
    geom_point() + geom_line(aes(group = base)) +
    labs(title = "BART on Computation Time", 
         subtitle = paste("Sigquant = ", sg),
         color = "Base") +
    xlab("Number of Trees") + ylab("Computation Time (s)"))
}

bart_ct <- ggplot(data = results_df_bart %>% filter(k == 2, sigquant == .9), 
       aes(x = ntree, y = comp_time, color = as.factor(base))) +
  geom_point() + 
  geom_line(mapping = aes(group = base)) +
  facet_nested_wrap(~ power, labeller = "label_both")
ggsave(filename = "~/General_Resarch/RF-BART-Comparison/ex0-simulateddata/graphics/4-bartcomptime.png",
       device = "png", plot = bart_ct, width = 6, height = 4, units = "in")

# Comparing all models on predictions

# Default
bart_default <- wbart(x.train = x_2_train$X, y.train = x_2_train$Y)
bart_default_pred <- predict(bart_default, data.frame(X = x_2_test$X))
bart_default_pred <- colMeans(bart_default_pred)

bart_default_rmse = (bart_default_pred - x_2_test$Y)^2 %>% mean() %>% sqrt()

# Best fit
bart_opt <- wbart(x.train = x_2_train$X, y.train = x_2_train$Y, ntree = 1, k = 1, base = .95, power = 1, sigquant = .05) 
bart_opt_pred <- predict(bart_opt, data.frame(X = x_2_test$X))
bart_opt_pred <- colMeans(bart_opt_pred)

bart_opt_rmse = (bart_opt_pred - x_2_test$Y)^2 %>% mean() %>% sqrt()

# Underfit
bart_uf <- wbart(x.train = x_2_train$X, y.train = x_2_train$Y, base = .01, power = 1000, k = 10, ntree = 1)
bart_uf_pred <- predict(bart_uf, data.frame(X = x_2_test$X))
bart_uf_pred <- colMeans(bart_uf_pred)

bart_uf_rmse = (bart_uf_pred - x_2_test$Y)^2 %>% mean() %>% sqrt()

# Overfit 
bart_of <- wbart(x.train = x_2_train$X, y.train = x_2_train$Y, base = 2, power = .0000001, k = 2, ntree = 1000)
bart_of_pred <- predict(bart_of, data.frame(X = x_2_test$X))
bart_of_pred <- colMeans(bart_of_pred)

bart_of_rmse = (bart_of_pred - x_2_test$Y)^2 %>% mean() %>% sqrt()

title3 <- expression(paste("x"^2, " curve with BART fit"))

ggplot(data = x_2, mapping = aes(x = X, y = Y)) + 
  geom_point(alpha = .25) +
  theme_classic() +
  labs(title = title3) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = bart_default_pred, color = "Default")) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = bart_opt_pred, color = "Tuned")) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = bart_uf_pred, color = "UnderFit")) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = bart_of_pred, color = "OverFit")) +
  labs(color = "Model Type") +
  scale_color_manual(values = c("Default" = "blue", "Tuned" = "green4", "UnderFit" = "red3", "OverFit" = "gray10"))

plt_bart1 <- ggplot(data = x_2, mapping = aes(x = X, y = Y)) + 
  geom_point(alpha = .25) +
  theme_classic() +
  labs(title = "BART Default Fit", subtitle = "ntree = 200, base = .95, power = 2, k = 2, sigquant = .9", caption = paste("RMSE: ", round(bart_default_rmse, 2))) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = bart_default_pred), color = "blue")

plt_bart2 <- ggplot(data = x_2, mapping = aes(x = X, y = Y)) + 
  geom_point(alpha = .25) +
  theme_classic() +
  labs(title = "BART Best Fit", subtitle = "ntree = 1, k = 1, base = .95, power = 1, sigquant = .05", caption = paste("RMSE: ", round(bart_opt_rmse, 2))) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = bart_opt_pred), color = "green4")

plt_bart3 <- ggplot(data = x_2, mapping = aes(x = X, y = Y)) + 
  geom_point(alpha = .25) +
  theme_classic() +
  labs(title = "BART Underfit", subtitle = "ntree = 1, base = .01, power = 1000, k = 10, sigquant = .9", caption = paste("RMSE: ", round(bart_uf_rmse, 2))) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = bart_uf_pred), color = "red3")

plt_bart4 <- ggplot(data = x_2, mapping = aes(x = X, y = Y)) + 
  geom_point(alpha = .25) +
  theme_classic() +
  labs(title = "BART Overfit", subtitle = "ntree = 1000, base = 2, power = ~ 0, k = 2, sigquant = .9", caption = paste("RMSE: ", round(bart_of_rmse, 2))) +
  geom_step(data = x_2_test, mapping = aes(x = X, y = bart_of_pred), color = "gray10")

bart_preds_plot <- ggpubr::ggarrange(plt_bart1, plt_bart2, plt_bart3, plt_bart4)
ggsave(filename = "~/General_Resarch/RF-BART-Comparison/ex0-simulateddata/graphics/5-bartpreds.png",
       device = "png", plot = bart_preds_plot, width = 9, height = 5.5, units = "in")

