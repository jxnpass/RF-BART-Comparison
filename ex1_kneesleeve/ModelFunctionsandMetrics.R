# libraries
library(tidyverse)
library(randomForest)
library(BART)
library(dplR)
library(prospectr)
library(ddpcr)
library(gridExtra)

### Omit sensors -------

eliminateSensors <- function(calib, flex, omitCriteria = c(1,2)) {
  
  omitSensorCalib <- c()
  omitSensorFlex <- c()
  
  # criteria 1 (decent variation) 
  if (1 %in% omitCriteria) {
    
    cv.min <- .01
    
    for (i in 2:17) {
      cv.c <- sd(calib[,i])/mean(calib[,i])
      if (cv.c < cv.min) {
        omitSensorCalib <- append(omitSensorCalib, colnames(calib[i]))
      }
    }
    
    for (i in 2:17) {
      cv.f <- sd(flex[,i])/mean(flex[,i])
      if (cv.f < cv.min) {
        omitSensorFlex <- append(omitSensorFlex, colnames(flex[i]))
      }
    }
  }
  
  # criteria 2 (low diffs) 
  if (2 %in% omitCriteria) {
    for (i in 2:17) {
      avg_diffs = sd(diff(calib[,i]))
      if (avg_diffs > 500) {
        omitSensorCalib <- append(omitSensorCalib, colnames(calib[i]))
      }
    }
    
    for (i in 2:17) {
      avg_diffs = sd(diff(flex[,i]))
      if (avg_diffs > 500) {
        omitSensorFlex <- append(omitSensorFlex, colnames(flex[i]))
      }
    }
  }
  
  omitSensor <- unique(c(omitSensorFlex, omitSensorCalib))
  return(omitSensor)
  
}

### correlation (with higher angle specified) function -----

high.corr <- function(flex, maxAngle = 60, type = "RF") {
  
  filtered.flex <- flex %>% 
    filter(angle >= maxAngle)
  
  if (type == "RF") {
    r2 <- cor(filtered.flex$rf.preds, filtered.flex$angle)^2
  }
  else if (type == "BART") {
    r2 <- cor(filtered.flex$bart.preds, filtered.flex$angle)^2
  }
  
  return(r2)
}

### max preds versus max actual comparisons  -----

pred.Maxes <- function(flex, numPeaks = 50, maxAngle = 60, epsilon = 1, type = "None", print.out = "maxes") {
  
  filtered.flex <- flex %>% 
    filter(angle >= maxAngle)
  
  truePeaks <- matrix(NA, ncol = 2, nrow = numPeaks)
  predPeaks <- matrix(NA, ncol = 2, nrow = numPeaks)
  
  currMax <- 1
  i = 1
  j = 1
  
  truePeaks[, 1] <- -100000
  predPeaks[, 1] <- -100000
  
  while(currMax <= numPeaks) {

    while(i < nrow(filtered.flex) && filtered.flex$time[i+1] - filtered.flex$time[i] < epsilon) {
      
      # if the angle at i is greater than the current set max, replace the angle and time stamp
      if (filtered.flex$angle[i] > truePeaks[currMax, 1]) {
        truePeaks[currMax, 1] <- filtered.flex$angle[i]
        truePeaks[currMax, 2] <- filtered.flex$time[i]
      }
      i <- i + 1
    }
    
    if (type != "None") {
      if (type == "RF") {
        while (j < nrow(filtered.flex) && filtered.flex$time[j+1] - filtered.flex$time[j] < epsilon) {
          
          if (filtered.flex$rf.preds[j] > predPeaks[currMax, 1]) {
            predPeaks[currMax, 1] <- filtered.flex$rf.preds[j]
            predPeaks[currMax, 2] <- filtered.flex$time[j]
          }
          j <- j + 1
        }
      }
      
      else if (type == "BART") {
        while (j < nrow(filtered.flex) && filtered.flex$time[j+1] - filtered.flex$time[j] < epsilon) {
          
          if (filtered.flex$bart.preds[j] > predPeaks[currMax, 1]) {
            predPeaks[currMax, 1] <- filtered.flex$bart.preds[j]
            predPeaks[currMax, 2] <- filtered.flex$time[j]
          }
          j <- j + 1
        }
      }
    }
      i <- i + 1
      j <- j + 1
      currMax <- currMax + 1
      
     # while currMax <= numPeaks
  }
  
  truePeaks <- na.omit(as.data.frame(truePeaks))
  predPeaks <- na.omit(as.data.frame(predPeaks))
  
  truePeaks <- truePeaks %>% 
    rename(maxes = V1, time = V2)
  if (type == "RF") {
    predPeaks <- predPeaks %>% 
      rename(RF.maxes = V1, RF.time = V2)
  }
  if (type == "BART") {
    predPeaks <- predPeaks %>% 
      rename(BART.maxes = V1, BART.time = V2)
  }
  
  if (print.out == "summary") {
    max.RMSE <- sqrt(mean((truePeaks[,1] - predPeaks[,1])^2))
    max.BIAS <- mean(predPeaks[,1] - truePeaks[,1])
    
    time.RMSE <- sqrt(mean((truePeaks[,2] - predPeaks[,2])^2))
    time.BIAS <- mean(predPeaks[,2] - truePeaks[,2])
    
    summary <- cbind(max.RMSE, max.BIAS, time.RMSE, time.BIAS) %>% as_tibble()
    return(summary)
  }
  
  else if (print.out == "maxes") {
    if (type != "None"){
      maxes <- cbind(truePeaks, predPeaks)
    }
    else {
      maxes <- truePeaks 
    }
    return(maxes) 
  }
}

TareData <- function(HighPointOld, LowPointOld, ChangeData, HighPointNew, LowPointNew) {
  
  NewData <- ChangeData
  
  for (i in 1:length(HighPointOld)) {
    for (j in 1:nrow(NewData)) {
      NewData[j,i+1] <- LowPointOld[i] + ( (NewData[j,i+1] - LowPointNew[i])  / (HighPointNew[i] - LowPointNew[i]) ) * (HighPointOld[i] - LowPointOld[i])
    }
  }
  
  return(NewData)
}

Recalibrate <- function(OldData, NewData) {
  
  # Old (model training) Data
  {
    HighPointOld <- OldData %>% 
      summarize(across(contains('sensor'), max))
    
    LowPointOld <- OldData %>% 
      summarize(across(contains('sensor'), min))
    
  }
  
  # New (model testing) Data
  
  {
    HighPointNew <- NewData %>% 
      summarize(across(contains('sensor'), max))
    
    LowPointNew <- NewData %>% 
      summarize(across(contains('sensor'), min))
    
  }
  
  # Taring processes
  
  RecalibratedData <- TareData(HighPointOld, LowPointOld, NewData, HighPointNew, LowPointNew)
  
  return(RecalibratedData)
  
}

ScaleAngle <- function(calib, flex) {
  # Compute scaling parameters
  scaled_min <- min(calib$angle)
  scaled_max <- max(calib$angle)
  original_min <- min(flex$angle)
  original_max <- max(flex$angle)
  
  # Scale 'angle' column in data_to_scale to match reference_data
  scaled_angle <- ((flex$angle - original_min) / (original_max - original_min)) * (scaled_max - scaled_min) + scaled_min
  
  # Update 'angle' column in data_to_scale
  flex$angle <- scaled_angle
  
  return(flex)
}

TransformData <- function(calib, flex, omitCrit = c(1,2), recalib = F, angle_shift = F) {
  
  ### Omissions
  omitSensor <- eliminateSensors(calib, flex, omitCriteria = omitCrit)

  ### Taring/recalibrating
  if (recalib) {
    flex <- Recalibrate(calib, flex)
    
    omitSensor = append(omitSensor, 
                        flex %>% 
                          select_if(~ any(is.na(.))) %>% 
                          colnames())
  }
  
  flex <- flex %>% select(-all_of(omitSensor))
  
  if (angle_shift) {
    flex <- ScaleAngle(calib, flex)
  }
  
  return(flex)
  
}


KneeGridCV <- function(X, y, X_valid, y_valid, df, param_grid, cv = 1, model = "RF") {
  
  n <- nrow(X)
  valid_results <- param_grid %>% mutate(rmse = NA, rsq = NA, mae = NA, 
                                         max_rmse = NA, high_rsq = NA, 
                                         comp_time = NA) # for cross validation
  
  print(paste("Performing Cross-Validation model fits", cv, "times for each unique set of parameters..."))
  for (i in 1:nrow(param_grid)) {
    
    if (model == "RF") {
      # identify params on grid
      ntree <- param_grid$ntree[i]
      mtry <- param_grid$mtry[i]
      node <- param_grid$node[i]
      
      
      print(paste("RandomForest Fit: Ntree = ", ntree, ", Mtry = ", mtry, ", Node = ", node, sep = ""))
    }
    
    else if (model == "BART") {
      # identify params on grid
      ntree <- param_grid$ntree[i]
      base <- param_grid$base[i]
      power <- param_grid$power[i]
      k <- param_grid$k[i]
      sigquant <- param_grid$sigquant[i]
      
      print(paste("BART Fit: Ntree = ", ntree, ", base = ", base, ", power = ", 
                  power, ", k = ", k, ", sigquant = ", sigquant, sep = ""))
      
    }
    # starting with cross validation
    results_cv <- data.frame(cv = 1:cv, rmse = NA, rsq = NA, mae = NA, max_rmse = NA, high_rsq = NA, comp_time = NA)
    
    for (n_cv in 1:cv) {
      # randomForest fit
      start_time <- Sys.time()
      if (model == "RF") {
        my_model <- randomForest(X, y, ntree = ntree, mtry = mtry, nodesize = node)
      }
      else if (model == "BART") {
        my_model <- wbart(X, y, ntree = ntree, base = base, power = power, k = k, sigquant = sigquant,
                          printevery = 0)
      }
      end_time <- Sys.time()
      results_cv[n_cv, c('comp_time')] <- end_time - start_time
      
      # check preds on validation set
      if (model == "RF") {
        y_pred_cv <- predict(my_model, X_valid) 
      }
      else if (model == "BART") {
        y_pred_cv <- predict(my_model, X_valid) %>% colMeans()
      }
      results <- cbind(y_valid, y_pred_cv) %>% 
        data.frame() %>% 
        metrics(., truth = y_valid, estimate = y_pred_cv)
      
      results_cv[n_cv, c('rmse','rsq','mae')] <- results$.estimate
      
      # calculating max_rmse and high_corr on entire model
      if (model == "RF") {
        df$rf.preds <- predict(my_model, X_valid)
        max_rmse <- pred.Maxes(df, type = "RF", print.out = "summary")$max.RMSE
        high_corr <- high.corr(df, type = "RF")
      }
      else if (model == "BART") {
        df$bart.preds <- predict(my_model, X_valid) %>% colMeans()
        max_rmse <- pred.Maxes(df, type = "BART", print.out = "summary")$max.RMSE
        high_corr <- high.corr(df, type = "BART")
      }
      
      results_cv[n_cv, c('max_rmse','high_rsq')] <- c(max_rmse, high_corr)
    }
    
    valid_results[i, c('rmse','rsq','mae','max_rmse','high_rsq', 'comp_time')] <- results_cv %>% 
      summarise(rmse = mean(rmse), rsq = mean(rsq), mae = mean(mae),
                max_rmse = mean(max_rmse), high_rsq = mean(high_rsq),
                comp_time = mean(comp_time))
  }
  
  # Finish CV  
  print("Cross-Validation Complete")
  
  return(valid_results)
}

AnalyzeRF <- function(X_train, y_train, X_valid, y_valid, X_test, y_test, 
                      df_valid, df_test, 
                      best_params = data.frame("ntree" = 500, 'mtry' = ncol(X)/3, 'nodesize' = 5)) {
  

# Fit Models --------------------------------------------------------------
  # best rf
  start_time <- Sys.time()
  best_rf <- randomForest(X_train, y_train, 
                          ntree = best_params$ntree, 
                          mtry = best_params$mtry, 
                          node = best_params$nodesize)
  end_time <- Sys.time()
  best_time <- end_time - start_time
  # default rf
  start_time <- Sys.time()
  default_rf <- randomForest(X2, y)
  end_time <- Sys.time()
  default_time <- end_time - start_time
  

# Predict on Tune --------------------------------------------------------

  # preds
  preds <- predict(best_rf, X_valid, predict.all = T) 
  y_best_preds_tune <- preds$aggregate
  y_best_preds_tune_ci <- apply(preds$individual, 1, quantile, probs = c(.025,.975))
  
  preds <- predict(default_rf, X_valid, predict.all = T)
  y_default_preds_tune <- preds$aggregate
  y_default_preds_tune_ci <- apply(preds$individual, 1, quantile, probs = c(.025,.975))
  
  # metrics from both fits
  best_results_tune <- cbind(y_valid, y_best_preds_tune) %>% 
    data.frame() %>% 
    metrics(., truth = y_valid, estimate = y_best_preds_tune) 
  
  default_results_tune <- cbind(y_valid, y_default_preds_tune) %>% 
    data.frame() %>% 
    metrics(., truth = y_valid, estimate = y_default_preds_tune) 
  
  df_valid$rf.preds <- y_best_preds_tune
  best_maxes_tune <- pred.Maxes(df_valid, type = "RF", print.out = "maxes")
  
  max_rmse <- pred.Maxes(df_valid, type = "RF", print.out = "summary")$max.RMSE
  high_corr <- high.corr(df_valid, type = "RF")
  
  df_valid$rf.preds <- y_best_preds_tune_ci[1,]
  best_maxes_tune.025 <- pred.Maxes(df_valid, type = "RF", print.out = "maxes")
  df_valid$rf.preds <- y_best_preds_tune_ci[2,]
  best_maxes_tune.975 <- pred.Maxes(df_valid, type = "RF", print.out = "maxes")
  
  best_results_tune <- data.frame('rmse' = best_results_tune$.estimate[1],
                                  'rsq' = best_results_tune$.estimate[2],
                                  'mae' = best_results_tune$.estimate[3],
                                  'max_rmse' = max_rmse,
                                  'high_corr' = high_corr)
  
  df_valid$rf.preds <- y_default_preds_tune
  default_maxes_tune <- pred.Maxes(df_valid, type = "RF", print.out = "maxes")
  
  max_rmse <- pred.Maxes(df_valid, type = "RF", print.out = "summary")$max.RMSE
  high_corr <- high.corr(df_valid, type = "RF")
  
  df_valid$rf.preds <- y_default_preds_tune_ci[1,]
  default_maxes_tune.025 <- pred.Maxes(df_valid, type = "RF", print.out = "maxes")
  df_valid$rf.preds <- y_default_preds_tune_ci[2,]
  default_maxes_tune.975 <- pred.Maxes(df_valid, type = "RF", print.out = "maxes")
  
  default_results_tune <- data.frame('rmse' = default_results_tune$.estimate[1],
                                     'rsq' = default_results_tune$.estimate[2],
                                     'mae' = default_results_tune$.estimate[3],
                                     'max_rmse' = max_rmse,
                                     'high_corr' = high_corr)
  
  tune_tuned_plot <- ggplot(data = df_valid, aes(x = time)) +
    geom_line(aes(y = angle), color = "black") +
    geom_point(data = best_maxes_tune, aes(x = time, y = maxes), color = "black", size = 3, shape = 17) +
    geom_line(aes(y = y_best_preds_tune), color = "red4") + 
    geom_point(data = best_maxes_tune, aes(x = RF.time, y = RF.maxes), color = "red4", size = 3, shape = 17) +
    geom_errorbar(data = best_maxes_tune.025, aes(x = RF.time, y = RF.maxes, ymin = RF.maxes, ymax = RF.maxes),
                  color = "red4", size = 1, width = 1) +
    geom_errorbar(data = best_maxes_tune.975, aes(x = RF.time, y = RF.maxes, ymin = RF.maxes, ymax = RF.maxes),
                  color = "red4", size = 1, width = 1) +
    labs(title = "Tuned RF Model using Sensors to Predict Knee Movement on Tuning Set",
         subtitle = paste("ntree =", best_params$ntree, ", mtry =", floor(best_params$mtry), ", min nodesize = ", best_params$nodesize),
         caption = paste("Max RMSE: ", round(best_results_tune$max_rmse, 2), 
                         "\n Correlation: ", round(best_results_tune$high_corr, 2),
                         "\nComputation Time: ", round(best_time, 2)))
  
  tune_default_plot <- ggplot(data = df_valid, aes(x = time)) +
    geom_line(aes(y = angle), color = "black") +
    geom_point(data = default_maxes_tune, aes(x = time, y = maxes), color = "black", size = 3, shape = 17) +
    geom_line(aes(y = y_default_preds_tune), color = "red4") + 
    geom_errorbar(data = default_maxes_tune.025, aes(x = RF.time, y = RF.maxes, ymin = RF.maxes, ymax = RF.maxes),
                  color = "red4", size = 1, width = 1) +
    geom_errorbar(data = default_maxes_tune.975, aes(x = RF.time, y = RF.maxes, ymin = RF.maxes, ymax = RF.maxes),
                  color = "red4", size = 1, width = 1) +
    geom_point(data = default_maxes_tune, aes(x = RF.time, y = RF.maxes), color = "red4", size = 3, shape = 17) +
    labs(title = "Default RF Model using Sensors to Predict Knee Movement on Tuning Set",
         subtitle = "ntree = 500, mtry = 3, min nodesize = 5",
         caption = paste("Max RMSE: ", round(default_results_tune$max_rmse, 2), 
                         "\n Correlation: ", round(default_results_tune$high_corr, 2),
                         "\nComputation Time: ", round(default_time, 2)))
  
  print("Tuned Model on Tune Set")
  print(best_results_tune)
  
  print("Default Model on Tune Set")
  print(default_results_tune)
  
  
  
  
# Predict on Test --------------------------------------------------------
  # preds
  preds <- predict(best_rf, X_test, predict.all = T) 
  y_best_preds_test <- preds$aggregate
  y_best_preds_test_ci <- apply(preds$individual, 1, quantile, probs = c(.025,.975))
  
  preds <- predict(default_rf, X_test, predict.all = T)
  y_default_preds_test <- preds$aggregate
  y_default_preds_test_ci <- apply(preds$individual, 1, quantile, probs = c(.025,.975))
  
  # metrics from both fits
  best_results_test <- cbind(y_test, y_best_preds_test) %>% 
    data.frame() %>% 
    metrics(., truth = y_test, estimate = y_best_preds_test) 
  
  default_results_test <- cbind(y_test, y_default_preds_test) %>% 
    data.frame() %>% 
    metrics(., truth = y_test, estimate = y_default_preds_test) 
  
  df_test$rf.preds <- y_best_preds_test
  best_maxes_test <- pred.Maxes(df_test, type = "RF", print.out = "maxes")
  
  max_rmse <- pred.Maxes(df_test, type = "RF", print.out = "summary")$max.RMSE
  high_corr <- high.corr(df_test, type = "RF")
  
  df_test$rf.preds <- y_best_preds_test_ci[1,]
  best_maxes_test.025 <- pred.Maxes(df_test, type = "RF", print.out = "maxes")
  df_test$rf.preds <- y_best_preds_test_ci[2,]
  best_maxes_test.975 <- pred.Maxes(df_test, type = "RF", print.out = "maxes")
  
  best_results_test <- data.frame('rmse' = best_results_test$.estimate[1],
                                'rsq' = best_results_test$.estimate[2],
                                'mae' = best_results_test$.estimate[3],
                                'max_rmse' = max_rmse,
                                'high_corr' = high_corr)
  
  df_test$rf.preds <- y_default_preds_test
  default_maxes_test <- pred.Maxes(df_test, type = "RF", print.out = "maxes")
  
  max_rmse <- pred.Maxes(df_test, type = "RF", print.out = "summary")$max.RMSE
  high_corr <- high.corr(df_test, type = "RF")
  
  df_test$rf.preds <- y_default_preds_test_ci[1,]
  default_maxes_test.025 <- pred.Maxes(df_test, type = "RF", print.out = "maxes")
  df_test$rf.preds <- y_default_preds_test_ci[2,]
  default_maxes_test.975 <- pred.Maxes(df_test, type = "RF", print.out = "maxes")
  
  default_results_test <- data.frame('rmse' = default_results_test$.estimate[1],
                                   'rsq' = default_results_test$.estimate[2],
                                   'mae' = default_results_test$.estimate[3],
                                   'max_rmse' = max_rmse,
                                   'high_corr' = high_corr)
  
  test_tuned_plot <- ggplot(data = df_test, aes(x = time)) +
    geom_line(aes(y = angle), color = "black") +
    geom_point(data = best_maxes_test, aes(x = time, y = maxes), color = "black", size = 3, shape = 17) +
    geom_line(aes(y = y_best_preds_test), color = "red4") + 
    geom_point(data = best_maxes_test, aes(x = RF.time, y = RF.maxes), color = "red4", size = 3, shape = 17) +
    geom_errorbar(data = best_maxes_test.025, aes(x = RF.time, y = RF.maxes, ymin = RF.maxes, ymax = RF.maxes),
                  color = "red4", size = 1, width = 1) +
    geom_errorbar(data = best_maxes_test.975, aes(x = RF.time, y = RF.maxes, ymin = RF.maxes, ymax = RF.maxes),
                  color = "red4", size = 1, width = 1) +
    labs(title = "Tuned RF Model using Sensors to Predict Knee Movement on Testing Set",
         subtitle = paste("ntree =", best_params$ntree, ", mtry =", floor(best_params$mtry), ", min nodesize = ", best_params$nodesize),
         caption = paste("Max RMSE: ", round(best_results_test$max_rmse, 2), 
                         "\n Correlation: ", round(best_results_test$high_corr, 2),
                         "\nComputation Time: ", round(best_time, 2)))
  
  test_default_plot <- ggplot(data = df_test, aes(x = time)) +
    geom_line(aes(y = angle), color = "black") +
    geom_point(data = default_maxes_test, aes(x = time, y = maxes), color = "black", size = 3, shape = 17) +
    geom_line(aes(y = y_default_preds_test), color = "red4") + 
    geom_point(data = default_maxes_test, aes(x = RF.time, y = RF.maxes), color = "red4", size = 3, shape = 17) +
    geom_errorbar(data = default_maxes_test.025, aes(x = RF.time, y = RF.maxes, ymin = RF.maxes, ymax = RF.maxes),
                  color = "red4", size = 1, width = 1) +
    geom_errorbar(data = default_maxes_test.975, aes(x = RF.time, y = RF.maxes, ymin = RF.maxes, ymax = RF.maxes),
                  color = "red4", size = 1, width = 1) +
    labs(title = "Default RF Model using Sensors to Predict Knee Movement on Testing Set",
         subtitle = "ntree = 500, mtry = 3, min nodesize = 5",
         caption = paste("Max RMSE: ", round(default_results_test$max_rmse, 2), 
                         "\n Correlation: ", round(default_results_test$high_corr, 2),
                         "\nComputation Time: ", round(default_time, 2)))
  
  print("Tuned Model on Test Set")
  print(best_results_test)
  
  print("Default Model on Test Set")
  print(default_results_test)
  
  return(list(tune_tuned_plot, tune_default_plot, test_tuned_plot, test_default_plot))
  
}


AnalyzeBART <- function(X_train, y_train, X_valid, y_valid, X_test, y_test, 
                      df_valid, df_test, 
                      best_params = data.frame("ntree" = 200, 'base' = .95, 'power' = 2, 'k' = 2)) {
  
  
  # Fit Models --------------------------------------------------------------
  # best bart
  start_time <- Sys.time()
  best_bart <- wbart(X_train, y_train, 
                     ntree = best_params$ntree, 
                     base = best_params$base,
                     power = best_params$power,
                     k = best_params$k, 
                     sigquant = best_params$sigquant)
  
  end_time <- Sys.time()
  best_time <- end_time - start_time
  # default bart
  start_time <- Sys.time()
  default_bart <- wbart(X_train, y_train)
  end_time <- Sys.time()
  default_time <- end_time - start_time
  
  
  # Predict on Tune --------------------------------------------------------
  
  # preds
  preds <- predict(best_bart, X_valid) 
  y_best_preds_tune <- preds %>% colMeans()
  y_best_preds_tune_ci <- apply(preds, 2, quantile, probs = c(.025,.975))

  preds <- predict(default_bart, X_valid)
  y_default_preds_tune <- preds %>% colMeans()
  y_default_preds_tune_ci <- apply(preds, 2, quantile, probs = c(.025,.975))
  
  # metrics from both fits
  best_results_tune <- cbind(y_valid, y_best_preds_tune) %>% 
    data.frame() %>% 
    metrics(., truth = y_valid, estimate = y_best_preds_tune) 
  
  default_results_tune <- cbind(y_valid, y_default_preds_tune) %>% 
    data.frame() %>% 
    metrics(., truth = y_valid, estimate = y_default_preds_tune) 
  
  df_valid$bart.preds <- y_best_preds_tune
  best_maxes_tune <- pred.Maxes(df_valid, type = "BART", print.out = "maxes")
  
  max_rmse <- pred.Maxes(df_valid, type = "BART", print.out = "summary")$max.RMSE
  high_corr <- high.corr(df_valid, type = "BART")
  
  best_results_tune <- data.frame('rmse' = best_results_tune$.estimate[1],
                                  'rsq' = best_results_tune$.estimate[2],
                                  'mae' = best_results_tune$.estimate[3],
                                  'max_rmse' = max_rmse,
                                  'high_corr' = high_corr)
  
  df_valid$bart.preds <- y_best_preds_tune_ci[1,]
  best_maxes_tune.025 <- pred.Maxes(df_valid, type = "BART", print.out = "maxes")
  df_valid$bart.preds <- y_best_preds_tune_ci[2,]
  best_maxes_tune.975 <- pred.Maxes(df_valid, type = "BART", print.out = "maxes")
  
  df_valid$bart.preds <- y_default_preds_tune
  default_maxes_tune <- pred.Maxes(df_valid, type = "BART", print.out = "maxes")
  
  max_rmse <- pred.Maxes(df_valid, type = "BART", print.out = "summary")$max.RMSE
  high_corr <- high.corr(df_valid, type = "BART")
  
  default_results_tune <- data.frame('rmse' = default_results_tune$.estimate[1],
                                     'rsq' = default_results_tune$.estimate[2],
                                     'mae' = default_results_tune$.estimate[3],
                                     'max_rmse' = max_rmse,
                                     'high_corr' = high_corr)
  
  df_valid$bart.preds <- y_default_preds_tune_ci[1,]
  default_maxes_tune.025 <- pred.Maxes(df_valid, type = "BART", print.out = "maxes")
  df_valid$bart.preds <- y_default_preds_tune_ci[2,]
  default_maxes_tune.975 <- pred.Maxes(df_valid, type = "BART", print.out = "maxes")
  
  tune_tuned_plot <- ggplot(data = df_valid, aes(x = time)) +
    geom_line(aes(y = angle), color = "black") +
    geom_point(data = best_maxes_tune, aes(x = time, y = maxes), color = "black", size = 3, shape = 17) +
    geom_line(aes(y = y_best_preds_tune), color = "pink3") + 
    geom_point(data = best_maxes_tune, aes(x = BART.time, y = BART.maxes), color = "pink3", size = 3, shape = 17) +
    geom_errorbar(data = best_maxes_tune.025, aes(x = BART.time, y = BART.maxes, ymin = BART.maxes, ymax = BART.maxes),
                  color = "pink4", size = 1, width = 1) +
    geom_errorbar(data = best_maxes_tune.975, aes(x = BART.time, y = BART.maxes, ymin = BART.maxes, ymax = BART.maxes),
                  color = "pink4", size = 1, width = 1) +
    labs(title = "Tuned BART Model using Sensors to Predict Knee Movement on Tuning Set",
         subtitle = paste("ntree =", best_params$ntree, ", base =", best_params$base, ", power = ", best_params$power, ", k = ", best_params$k, ", sigquant = ", best_params$sigquant),
         caption = paste("Max RMSE: ", round(best_results_tune$max_rmse, 2), 
                         "\n Correlation: ", round(best_results_tune$high_corr, 2),
                         "\nComputation Time: ", round(best_time, 2))) +
    ylim(-20, NA)
  
  
  tune_default_plot <- ggplot(data = df_valid, aes(x = time)) +
    geom_line(aes(y = angle), color = "black") +
    geom_point(data = default_maxes_tune, aes(x = time, y = maxes), color = "black", size = 3, shape = 17) +
    geom_line(aes(y = y_default_preds_tune), color = "pink3") + 
    geom_point(data = default_maxes_tune, aes(x = BART.time, y = BART.maxes), color = "pink3", size = 3, shape = 17) +
    geom_errorbar(data = default_maxes_tune.025, aes(x = BART.time, y = BART.maxes, ymin = BART.maxes, ymax = BART.maxes),
                  color = "pink4", size = 1, width = 1) +
    geom_errorbar(data = default_maxes_tune.975, aes(x = BART.time, y = BART.maxes, ymin = BART.maxes, ymax = BART.maxes),
                  color = "pink4", size = 1, width = 1) +    
    labs(title = "Default BART Model using Sensors to Predict Knee Movement on Tuning Set",
         subtitle = "ntree = 200, base = .95, power = 2, k = 2, sigquant = .9",
         caption = paste("Max RMSE: ", round(default_results_tune$max_rmse, 2), 
                         "\n Correlation: ", round(default_results_tune$high_corr, 2),
                         "\nComputation Time: ", round(default_time, 2))) +
    ylim(-20,NA)
  
  # Predict on Test --------------------------------------------------------
  # preds
  
  preds <- predict(best_bart, X_test) 
  y_best_preds_test <- preds %>% colMeans()
  y_best_preds_test_ci <- apply(preds, 2, quantile, probs = c(.025,.975))
  
  preds <- predict(default_bart, X_test)
  y_default_preds_test <- preds %>% colMeans()
  y_default_preds_test_ci <- apply(preds, 2, quantile, probs = c(.025,.975))
  
  # metrics from both fits
  best_results_test <- cbind(y_test, y_best_preds_test) %>% 
    data.frame() %>% 
    metrics(., truth = y_test, estimate = y_best_preds_test) 
  
  default_results_test <- cbind(y_test, y_default_preds_test) %>% 
    data.frame() %>% 
    metrics(., truth = y_test, estimate = y_default_preds_test) 
  
  df_test$bart.preds <- y_best_preds_test
  best_maxes_test <- pred.Maxes(df_test, type = "BART", print.out = "maxes")
  
  max_rmse <- pred.Maxes(df_test, type = "BART", print.out = "summary")$max.RMSE
  high_corr <- high.corr(df_test, type = "BART")
  
  best_results_test <- data.frame('rmse' = best_results_test$.estimate[1],
                                  'rsq' = best_results_test$.estimate[2],
                                  'mae' = best_results_test$.estimate[3],
                                  'max_rmse' = max_rmse,
                                  'high_corr' = high_corr)
  
  df_test$bart.preds <- y_best_preds_test_ci[1,]
  best_maxes_test.025 <- pred.Maxes(df_test, type = "BART", print.out = "maxes")
  df_test$bart.preds <- y_best_preds_test_ci[2,]
  best_maxes_test.975 <- pred.Maxes(df_test, type = "BART", print.out = "maxes")
  
  df_test$bart.preds <- y_default_preds_test
  default_maxes_test <- pred.Maxes(df_test, type = "BART", print.out = "maxes")
  
  max_rmse <- pred.Maxes(df_test, type = "BART", print.out = "summary")$max.RMSE
  high_corr <- high.corr(df_test, type = "BART")
  
  default_results_test <- data.frame('rmse' = default_results_test$.estimate[1],
                                     'rsq' = default_results_test$.estimate[2],
                                     'mae' = default_results_test$.estimate[3],
                                     'max_rmse' = max_rmse,
                                     'high_corr' = high_corr)
  
  df_test$bart.preds <- y_default_preds_test_ci[1,]
  default_maxes_test.025 <- pred.Maxes(df_test, type = "BART", print.out = "maxes")
  df_test$bart.preds <- y_default_preds_test_ci[2,]
  default_maxes_test.975 <- pred.Maxes(df_test, type = "BART", print.out = "maxes")
  
  test_tuned_plot <- ggplot(data = df_test, aes(x = time)) +
    geom_line(aes(y = angle), color = "black") +
    geom_point(data = best_maxes_test, aes(x = time, y = maxes), color = "black", size = 3, shape = 17) +
    geom_line(aes(y = y_best_preds_test), color = "pink3") + 
    geom_point(data = best_maxes_test, aes(x = BART.time, y = BART.maxes), color = "pink3", size = 3, shape = 17) +
    geom_errorbar(data = best_maxes_test.025, aes(x = BART.time, y = BART.maxes, ymin = BART.maxes, ymax = BART.maxes),
                  color = "pink4", size = 1, width = 1) +
    geom_errorbar(data = best_maxes_test.975, aes(x = BART.time, y = BART.maxes, ymin = BART.maxes, ymax = BART.maxes),
                  color = "pink4", size = 1, width = 1) +
    labs(title = "Tuned BART Model using Sensors to Predict Knee Movement on Testing Set",
         subtitle = paste("ntree =", best_params$ntree, ", base =", best_params$base, ", power = ", best_params$power, ", k = ", best_params$k, ", sigquant = ", best_params$sigquant),
         caption = paste("Max RMSE: ", round(best_results_test$max_rmse, 2), 
                         "\n Correlation: ", round(best_results_test$high_corr, 2),
                         "\nComputation Time: ", round(best_time, 2))) +
    ylim(-20,NA)
  
  test_default_plot <- ggplot(data = df_test, aes(x = time)) +
    geom_line(aes(y = angle), color = "black") +
    geom_point(data = default_maxes_test, aes(x = time, y = maxes), color = "black", size = 3, shape = 17) +
    geom_line(aes(y = y_default_preds_test), color = "pink3") + 
    geom_point(data = default_maxes_test, aes(x = BART.time, y = BART.maxes), color = "pink3", size = 3, shape = 17) +
    geom_errorbar(data = default_maxes_test.025, aes(x = BART.time, y = BART.maxes, ymin = BART.maxes, ymax = BART.maxes),
                  color = "pink4", size = 1, width = 1) +
    geom_errorbar(data = default_maxes_test.975, aes(x = BART.time, y = BART.maxes, ymin = BART.maxes, ymax = BART.maxes),
                  color = "pink4", size = 1, width = 1) +
    labs(title = "Default BART Model using Sensors to Predict Knee Movement on Testing Set",
         subtitle = "ntree = 200, base = .95, power = 2, k = 2, sigquant = .9",
         caption = paste("Max RMSE: ", round(default_results_test$max_rmse, 2), 
                         "\n Correlation: ", round(default_results_test$high_corr, 2),
                         "\nComputation Time: ", round(default_time, 2))) +
    ylim(-20,NA)
  
  print("Tuned Model on Tune Set")
  print(best_results_tune)
  
  print("Default Model on Tune Set")
  print(default_results_tune)
  
  print("Tuned Model on Test Set")
  print(best_results_test)
  
  print("Default Model on Test Set")
  print(default_results_test)
  
  return(list(tune_tuned_plot, tune_default_plot, test_tuned_plot, test_default_plot))
  
}
