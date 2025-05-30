prepare_model_datasets <- function(final_data_rvm01, svm01_clean, prod_hier) {
  # Combine cleaned dataset and set of columns with many NAs which we were instructed to preserve
  common_cols <- intersect(names(final_data_rvm01), names(svm01_clean))
  extra_cols <- as.character(316:365)
  cols_to_keep <- unique(c(common_cols, extra_cols))
  final_subset <- final_data_rvm01[, cols_to_keep]
  
  # Extract relevant rows to companies 16, 18, 93, 98
  filtered_rows <- prod_hier[grep("^16|^18|^93|^98", prod_hier$GSDHPROHI), ]
  filter_ids <- as.numeric(filtered_rows[["70"]])
  
  # Create another column containing the company ID
  filtered_rows$Group <- ifelse(grepl("^16", filtered_rows$GSDHPROHI), "16",
                                ifelse(grepl("^18", filtered_rows$GSDHPROHI), "18",
                                       ifelse(grepl("^93", filtered_rows$GSDHPROHI), "93", "98")))
  
  id_to_group <- setNames(filtered_rows$Group, filtered_rows[["70"]])
  final_subset <- final_subset[final_subset$`70` %in% filter_ids, ]
  
  # Create columns derived from 316-365, 347 excluded (many NAs columns)
  fee_cols <- as.character(316:365)
  fee_cols <- setdiff(fee_cols, "347")
  
  # Count of non-NA, non-zero values per row
  final_subset$fee_code_signal_count <- rowSums(!is.na(final_subset[, fee_cols]) & final_subset[, fee_cols] != 0)
  
  # Sum of values per row (ignore NAs)
  final_subset$fee_code_signal_sum <- rowSums(final_subset[, fee_cols], na.rm = TRUE)
  
  # Binary indicator: TRUE if any value is non-NA and non-zero
  final_subset$fee_code_signal_any <- apply(final_subset[, fee_cols], 1, function(x) any(!is.na(x) & x != 0))
  
  # check if the column fee_code_signal_any is TRUE only when column 347 is NA
  all(final_subset$fee_code_signal_any == TRUE & is.na(final_subset$'347'))
  
  # check if every time fee_code_signal_any is TRUE, column 347 is NA
  all(is.na(final_subset$'347'[final_subset$fee_code_signal_any == TRUE]))
  
  subset_na_347 <- final_subset[is.na(final_subset$'347'), ]
  summary(subset_na_347)
  
  # Total number of rows where fee_code_signal_any is TRUE
  total_true <- sum(final_subset$fee_code_signal_any == TRUE, na.rm = TRUE)
  
  # Number of those where column 347 is NA
  true_and_na <- sum(is.na(final_subset$`347`) & final_subset$fee_code_signal_any == TRUE, na.rm = TRUE)
  
  percentage <- (true_and_na / total_true) * 100
  percentage
  
  problematic_rows <- final_subset[final_subset$fee_code_signal_any == TRUE & !is.na(final_subset$'347'), ]
  hist(problematic_rows$"70")
  
  # CREATE GROUPS OF HIGHLY CORRELATED COLUMNS IN THE RANGE 316-365, 347 EXCLUDED
  cols <- final_subset[, as.character(316:365)]
  not_na <- !is.na(cols)
  cor_mat <- cor(not_na)
  
  # Find groups with correlation > 0.99
  groups <- list()
  visited <- c()
  
  for (i in colnames(cor_mat)) {
    if (i %in% visited) next
    group <- names(which(cor_mat[i, ] > 0.99))
    if (length(group) > 1) {
      groups[[length(groups) + 1]] <- group
      visited <- c(visited, group)
      cat("\nGroup", length(groups), "members:\n")
      print(group)
    }
  }
  
  groups
  
  for (i in seq_along(groups)) {
    group_cols <- groups[[i]]
    new_col <- paste0("group", i, "_sum")
    final_subset[[new_col]] <- rowSums(final_subset[, group_cols], na.rm = TRUE)
  }
  all_group_columns <- unique(unlist(groups))
  final_subset <- final_subset[, !(names(final_subset) %in% all_group_columns)]
  # columns that don't end up in any group
  leftover_cols <- c("346", "349", "350", "355", "356", "358", "359", "360", "361", "362", "364")
  variances <- sapply(final_subset[, leftover_cols], var, na.rm = TRUE)
  print(variances)
  correlations <- sapply(final_subset[, leftover_cols], function(col) cor(col, final_subset$"70", use = "complete.obs"))
  print(correlations)
  # some the columns with no group only have NAs, we delete them
  final_subset <- Filter(function(x) !all(is.na(x)), final_subset)
  # these columns are not only NA and they don't belong in a group
  leftover_cols <- c("349", "350", "362", "364")
  variances <- sapply(final_subset[, leftover_cols], var, na.rm = TRUE)
  print(variances)
  correlations <- sapply(final_subset[, leftover_cols], function(col) cor(col, final_subset$"70", use = "complete.obs"))
  print(correlations)
  
  # CREATE 2 DATASETS FOR COMPANY PREDICTION BASED ON 347: ONE WITH THE NAs ONE WITH NO NAs
  filtered_rows <- prod_hier[grep("^16|^18|^93|^98", prod_hier$GSDHPROHI), ]
  filter_ids <- as.numeric(filtered_rows[["70"]])
  filtered_rows$Group <- ifelse(grepl("^16", filtered_rows$GSDHPROHI), "16",
                                ifelse(grepl("^18", filtered_rows$GSDHPROHI), "18",
                                       ifelse(grepl("^93", filtered_rows$GSDHPROHI), "93", "98")))
  id_to_group <- setNames(filtered_rows$Group, filtered_rows[["70"]])
  final_subset$Group <- id_to_group[as.character(final_subset$`70`)]
  final_subset$Group <- as.factor(final_subset$Group)
  threshold <- 98  
  na_percentage <- colSums(is.na(final_subset)) / nrow(final_subset) * 100
  final_subset <- final_subset[, na_percentage <= threshold]
  count_with_na <- sum(apply(final_subset, 1, function(x) any(is.na(x))))
  print(count_with_na)
  
  final_subset_no_NA <- na.omit(final_subset)
  final_subset_NA <- final_subset[is.na(final_subset$"347"), ]
  # Dataset A
  dataset_a <- final_subset_no_NA[, !names(final_subset_no_NA) %in% c("50", "55", "104", "110", "119", "121", "123", "125", "135", "151", "162", "166")]
  # Dataset B
  dataset_b <- final_subset_NA[, !names(final_subset_NA) %in% c("110", "119", "121", "123", "125", "135", "166")]
  
  # CREATE MATERIAL DATASET
  svm_for_material <- dataset_a
  # column 218: Material column
  svm_for_material$"X218" <- final_data_rvm01[rownames(svm_for_material), "218"]
  # Remove columns "70" and "Group", highly correlated with material
  svm_for_material <- svm_for_material[, !names(svm_for_material) %in% c("70", "Group")]
  
  # CREATE MARKET DATASET
  svm_for_market <- dataset_a
  # Remove columns "70" and "Group", highly correlated with market
  svm_for_market <- svm_for_market[, !names(svm_for_market) %in% c("70", "Group")]
  
  return(list(
    dataset_a = dataset_a,
    dataset_b = dataset_b,
    svm_for_material = svm_for_material,
    svm_for_market = svm_for_market
  ))
}
