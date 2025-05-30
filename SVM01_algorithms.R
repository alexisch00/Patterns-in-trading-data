# Load packages safely
safe_library <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Random Forest
run_random_forest <- function(df, target_col, exclude_cols = NULL, top_n = 20) {
  safe_library("ranger")
  
  df[[target_col]] <- droplevels(as.factor(df[[target_col]]))
  names(df) <- make.names(names(df))
  if (!is.null(exclude_cols)) df <- df[, !names(df) %in% exclude_cols]
  
  set.seed(123)
  rf_model <- ranger(
    formula = as.formula(paste(target_col, "~ .")),
    data = df,
    importance = "impurity",
    probability = FALSE
  )
  
  cat("\nConfusion Matrix:\n")
  print(rf_model$confusion.matrix)
  
  oob_accuracy <- 1 - rf_model$prediction.error
  cat(paste("OOB Accuracy:", round(oob_accuracy * 100, 2), "%\n"))
  
  importance_scores <- rf_model$variable.importance
  sorted_importance <- sort(importance_scores, decreasing = TRUE)
  
  barplot(
    head(sorted_importance, top_n),
    las = 2,
    main = paste("Top", top_n, "Important Features")
  )
  
  barplot(
    sorted_importance,
    las = 2,
    cex.names = 0.7,
    main = "All Features Ranked by Importance",
    col = "gray"
  )
  
  return(sorted_importance)
}

run_one_column_accuracy <- function(df, target_col) {
  safe_library("ranger")
  
  # Ensure safe names
  names(df) <- make.names(names(df))
  target_col <- make.names(target_col)
  
  predictor_cols <- setdiff(names(df), target_col)
  accuracy_results <- numeric(length(predictor_cols))
  names(accuracy_results) <- predictor_cols
  
  for (col in predictor_cols) {
    # Use backticks (not quotes!)
    f <- as.formula(paste0("`", target_col, "` ~ `", col, "`"))
    model <- tryCatch(
      ranger(f, data = df, probability = FALSE),
      error = function(e) {
        cat("Skipping variable due to error in formula or model:", col, "\n")
        return(NULL)
      }
    )
    if (!is.null(model)) {
      accuracy_results[col] <- 1 - model$prediction.error
    }
  }
  
  accuracy_df <- data.frame(
    Feature = names(accuracy_results),
    Accuracy = round(accuracy_results, 4)
  )
  accuracy_df <- accuracy_df[order(-accuracy_df$Accuracy), ]
  print(accuracy_df)
  return(accuracy_df)
}



# SVM with iml importance
run_svm_iml <- function(df, target_col) {
  safe_library("e1071")
  safe_library("iml")
  
  df[[target_col]] <- as.factor(df[[target_col]])
  X <- df[, setdiff(names(df), target_col)]
  X <- X[sapply(X, is.numeric)]
  y <- ifelse(df[[target_col]] == levels(df[[target_col]])[2], 1, 0)
  
  svm_model <- svm(
    x = X, y = as.factor(df[[target_col]]),
    kernel = "radial", probability = TRUE, scale = TRUE
  )
  
  predict_function <- function(model, newdata) {
    probs <- attr(predict(model, newdata = newdata, probability = TRUE), "probabilities")
    return(data.frame(Class = probs[, "1"]))
  }
  
  predictor <- Predictor$new(
    model = svm_model,
    data = X,
    y = y,
    predict.function = predict_function,
    type = "prob"
  )
  
  imp <- FeatureImp$new(predictor, loss = "logLoss")
  plot(imp)
}

# XGBoost modeling
run_xgboost <- function(df, target_col, formula_rhs = NULL) {
  safe_library("xgboost")
  
  df[[target_col]] <- as.factor(df[[target_col]])
  df$Group_numeric <- as.numeric(df[[target_col]]) - 1
  
  if (is.null(formula_rhs)) {
    formula_rhs <- paste(setdiff(names(df), c(target_col, "Group_numeric")), collapse = " + ")
  }
  formula_str <- paste("Group_numeric ~", formula_rhs)
  X <- model.matrix(as.formula(formula_str), data = df)[, -1]
  y <- df$Group_numeric
  
  xgb_model <- xgboost(data = X, label = y, objective = "binary:logistic", nrounds = 300, verbose = 0)
  importance_matrix <- xgb.importance(model = xgb_model)
  print(head(importance_matrix, 10))
  xgb.plot.importance(importance_matrix, top_n = 20)
  
  preds <- predict(xgb_model, newdata = X)
  pred_class <- ifelse(preds > 0.5, 1, 0)
  accuracy <- mean(pred_class == y)
  cat("XGBoost Accuracy:", round(accuracy * 100, 2), "%\n")
}

# Wilcoxon rank-sum test
run_wilcoxon_tests <- function(df, target_col, exclude = NULL) {
  features <- setdiff(names(df), c(target_col, exclude))
  for (feature in features) {
    cat("\nFeature:", feature, "\n")
    print(wilcox.test(df[[feature]] ~ df[[target_col]]))
  }
}

# Random Forest dataset A
df_a <- dataset_a[dataset_a$Group %in% c(16, 18), ]
df_a$Group <- droplevels(as.factor(df_a$Group))
df_a <- df_a[, !names(df_a) %in% c("70")]
names(df_a) <- make.names(names(df_a))
#df_a <- df_a[sample(nrow(df_a), min(nrow(df_a), 10000)), ]
sorted_importance_a <- run_random_forest(df_a, target_col = "Group")
accuracy_df_a <- run_one_column_accuracy(df_a, target_col = "Group")

# Wilcoxon test
run_wilcoxon_tests(df_a, target_col = "Group", exclude = "X70")

# Random Forest dataset B
df_b <- dataset_b
df_b$Group <- droplevels(as.factor(df_b$Group))
df_b <- df_b[, !names(df_b) %in% c("70")]
names(df_b) <- make.names(names(df_b))
#df_b <- df_b[sample(nrow(df_b), min(nrow(df_b), 10000)), ]
sorted_importance_b <- run_random_forest(df_b, target_col = "Group")
accuracy_df_b <- run_one_column_accuracy(df_b, target_col = "Group")

# Wilcoxon test
run_wilcoxon_tests(df_b, target_col = "Group", exclude = "X70")

# Random Forest material
df_mat <- svm_for_material
df_mat$X218 <- droplevels(as.factor(df_mat$X218))
names(df_mat) <- make.names(names(df_mat))
#df_mat <- df_mat[sample(nrow(df_mat), min(nrow(df_mat), 10000)), ]
sorted_importance_mat <- run_random_forest(df_mat, target_col = "X218")
accuracy_df_mat <- run_one_column_accuracy(df_mat, target_col = "X218")

# Random Forest market
df_mkt <- svm_for_market
df_mkt$X194 <- droplevels(as.factor(df_mkt$X194))
names(df_mkt) <- make.names(names(df_mkt))
#df_mkt <- df_mkt[sample(nrow(df_mkt), min(nrow(df_mkt), 10000)), ]
sorted_importance_mkt <- run_random_forest(df_mkt, target_col = "X194")
accuracy_df_mkt <- run_one_column_accuracy(df_mkt, target_col = "X194")

# XGBoost dataset A
df_a <- dataset_a
df_a$Group <- droplevels(as.factor(df_a$Group))
df_a <- df_a[df_a$Group %in% c(16, 18), ]
df_a <- df_a[, !names(df_a) %in% c("70")]
names(df_a) <- make.names(names(df_a))
#df_a <- df_a[sample(nrow(df_a), min(nrow(df_a), 10000)), ]
run_xgboost(df_a, target_col = "Group")

# XGBoost dataset B
df_b <- dataset_b
df_b$Group <- droplevels(as.factor(df_b$Group))
df_b <- df_b[, !names(df_b) %in% c("70")]
names(df_b) <- make.names(names(df_b))
#df_b <- df_b[sample(nrow(df_b), min(nrow(df_b), 10000)), ]
run_xgboost(df_b, target_col = "Group")

# XGBoost material dataset
df_mat <- svm_for_material
df_mat$X218 <- droplevels(as.factor(df_mat$X218))
names(df_mat) <- make.names(names(df_mat))
#df_mat <- df_mat[sample(nrow(df_mat), min(nrow(df_mat), 10000)), ]
run_xgboost(df_mat, target_col = "X218")

# XGBoost market dataset
df_mkt <- svm_for_market
df_mkt$X194 <- droplevels(as.factor(df_mkt$X194))
names(df_mkt) <- make.names(names(df_mkt))
#df_mkt <- df_mkt[sample(nrow(df_mkt), min(nrow(df_mkt), 10000)), ]
run_xgboost(df_mkt, target_col = "X194")

# SVM sampled dataset A
df_a <- dataset_a
df_a$Group <- droplevels(as.factor(df_a$Group))
df_a <- df_a[df_a$Group %in% c(16, 18), ]
df_a <- df_a[, !names(df_a) %in% c("70")]
names(df_a) <- make.names(names(df_a))
sampled_a <- df_a[sample(nrow(df_a), min(nrow(df_a), 10000)), ] 
run_svm_iml(sampled_a, target_col = "Group")

# SVM sampled dataset B
df_b <- dataset_b
df_b$Group <- droplevels(as.factor(df_b$Group))
df_b <- df_b[, !names(df_b) %in% c("70")]
names(df_b) <- make.names(names(df_b))
sampled_b <- df_b[sample(nrow(df_b), min(nrow(df_b), 10000)), ]
run_svm_iml(sampled_b, target_col = "Group")

# SVM sampled material dataset 
df_mat <- svm_for_material
df_mat$X218 <- droplevels(as.factor(df_mat$X218))
names(df_mat) <- make.names(names(df_mat))
sampled_mat <- df_mat[sample(nrow(df_mat), min(nrow(df_mat), 10000)), ]
run_svm_iml(sampled_mat, target_col = "X218")

# SVM sampled market dataset
df_mkt <- svm_for_market
df_mkt$X194 <- droplevels(as.factor(df_mkt$X194))
names(df_mkt) <- make.names(names(df_mkt))
sampled_mkt <- df_mkt[sample(nrow(df_mkt), min(nrow(df_mkt), 10000)), ]
run_svm_iml(sampled_mkt, target_col = "X194")

# Logistic Regression dataset A
df_a$Group <- droplevels(df_a$Group)
features <- setdiff(names(df_a), c("Group", "X70"))
formula <- as.formula(paste("Group ~", paste(features, collapse = " + ")))
logit_model_all <- glm(formula, data = df_a, family = "binomial")
summary(logit_model_all)
