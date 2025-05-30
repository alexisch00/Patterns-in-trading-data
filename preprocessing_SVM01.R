library(dplyr)
library(caret)

find_na_columns <- function(df, threshold = 0.85) {
  na_prop <- vapply(df, function(col) mean(is.na(col)), numeric(1))
  na_cols <- names(na_prop[na_prop >= threshold])
  return(na_cols)
}
                    
remove_na_columns <- function(df, threshold = 0.85) {
  na_prop <- vapply(df, function(col) mean(is.na(col)), numeric(1))
  df[, na_prop < threshold]
}

find_constant_columns <- function(df) {
  constant_cols <- sapply(df, function(x) length(unique(na.omit(x))) == 1)
  return(names(constant_cols[constant_cols]))
}
                    
remove_constant_columns <- function(df) {
  constant_cols <- sapply(df, function(x) length(unique(na.omit(x))) == 1)
  df[, !constant_cols]
}

# ----------------------------
# Sample, clean, and reduce dimensionality

process_and_sample_data <- function(df, sample_frac = 0.03) {
  df <- filter_na_columns(df)
  df <- remove_constant_columns(df)
  sampled <- df[sample(nrow(df), size = sample_frac * nrow(df)), ]
  nzv <- nearZeroVar(sampled, freqCut = 95/5, saveMetrics = TRUE)
  return(sampled[, !nzv$nzv])
}

impute_numeric_means <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) {
      col[is.na(col)] <- mean(col, na.rm = TRUE)
    }
    col
  })
  return(df)
}

find_highly_correlated_pairs <- function(df, cutoff = 0.9) {
  cor_matrix <- cor(df, use = "pairwise.complete.obs")
  cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA

  high_pairs <- which(abs(cor_matrix) > cutoff, arr.ind = TRUE)
  
  results <- data.frame(
    var1 = colnames(cor_matrix)[high_pairs[, 1]],
    var2 = colnames(cor_matrix)[high_pairs[, 2]],
    correlation = cor_matrix[high_pairs]
  )
  
  results <- results[order(-abs(results$correlation)), ]  # sort by absolute correlation
  return(results)
}
                          
# ----------------------------
# Remove one of each pair of highly correlated variables

# keeps variables in the exclude list
remove_highly_correlated <- function(df, exclude = c("70", "71"), cutoff = 0.9) {
  cor_matrix <- cor(df, use = "pairwise.complete.obs")
  cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA  # only upper triangle
  
  high_pairs <- which(abs(cor_matrix) > cutoff, arr.ind = TRUE)
  to_remove <- character()
  dynamic_exclude <- as.character(exclude)  # will grow to include kept variables
  
  for (i in seq_len(nrow(high_pairs))) {
    var1 <- colnames(cor_matrix)[high_pairs[i, 1]]
    var2 <- colnames(cor_matrix)[high_pairs[i, 2]]
    
    # Skip if already removed
    if (var1 %in% to_remove || var2 %in% to_remove) next
    
    # If one is in dynamic exclude, remove the other
    if (var1 %in% dynamic_exclude && !(var2 %in% dynamic_exclude)) {
      to_remove <- c(to_remove, var2)
      dynamic_exclude <- c(dynamic_exclude, var1) # Add in exclude list so that it is not to be removed later
    } else if (var2 %in% dynamic_exclude && !(var1 %in% dynamic_exclude)) {
      to_remove <- c(to_remove, var1)
      dynamic_exclude <- c(dynamic_exclude, var2)
    } else {
      # Neither is in exclude, keep var1, remove var2
      to_remove <- c(to_remove, var2)
      dynamic_exclude <- c(dynamic_exclude, var1)
    }
  }
  
  df_filtered <- df[, !(colnames(df) %in% to_remove)]
  return(df_filtered)
}
