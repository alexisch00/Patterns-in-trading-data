library(caret)

map_column_names <- function(data, column_info) {
  colnames(data) <- as.character(colnames(data))
  column_info$column_number <- as.character(column_info$column_number)
  column_info$column_name   <- as.character(column_info$column_name)
  
  col_ids   <- sub("Col_", "", colnames(data))
  new_names <- column_info$column_name[match(col_ids, column_info$column_number)]
  colnames(data) <- ifelse(is.na(new_names), colnames(data), new_names)
  
  data
}

filter_low_variance <- function(data, freq_cut = 95/5, unique_cut = 10) {
  get_nzv <- function(df) {
    cols <- character()
    for (nm in names(df)) {
      vec <- df[[nm]]
      if (!is.numeric(vec) && !is.factor(vec)) next
      vec <- na.omit(vec)
      if (length(vec) < 2) next
      ftbl      <- sort(table(vec), decreasing = TRUE)
      freq_ratio <- if (length(ftbl) > 1) ftbl[1] / ftbl[2] else Inf
      unique_pct <- length(unique(vec)) / length(vec) * 100
      if (freq_ratio > freq_cut && unique_pct < unique_cut) {
        cols <- c(cols, nm)
      }
    }
    cols
  }
  
  rem_cols <- get_nzv(data)
  data[, setdiff(names(data), rem_cols)]
}

remove_suffix_cols <- function(data, suffix) {
  drop <- grep(paste0(suffix, "$"), names(data), value = TRUE)
  data[, setdiff(names(data), drop)]
}

convert_tmst_to_numeric <- function(data) {
  tmst_cols <- grep("TMST$", names(data), value = TRUE)
  for (nm in tmst_cols) {
    dt <- suppressWarnings(as.POSIXct(data[[nm]], format = "%Y-%m-%d %H:%M:%S"))
    data[[nm]] <- as.numeric(format(dt, "%Y%m%d"))
  }
  data
}

convert_date_string_cols <- function(data) {
  non_num <- names(data)[!sapply(data, is.numeric)]
  for (nm in non_num) {
    if (all(grepl("^\\d{8}$", data[[nm]]))) {
      data[[nm]] <- as.numeric(data[[nm]])
    }
  }
  data
}

remove_high_correlation <- function(data, cutoff = 0.9) {
  num_data   <- data[sapply(data, is.numeric)]
  corr_mat   <- cor(num_data, use = "pairwise.complete.obs")
  na_cols    <- which(apply(is.na(corr_mat), 2, all))
  
  if (length(na_cols) > 0) {
    corr_mat <- corr_mat[-na_cols, -na_cols]
    num_data <- num_data[, -na_cols]
  }
  
  to_drop   <- findCorrelation(corr_mat, cutoff = cutoff)
  num_data[, -to_drop]
}

preprocess_fds <- function(raw_data, column_info, save_path = "~/Documents/DataScienceLab/fds_final_cleaned.RData") {
  data <- map_column_names(raw_data, column_info)
  data <- filter_low_variance(data)
  data <- remove_suffix_cols(data, "__T")
  data <- convert_tmst_to_numeric(data)
  data <- convert_date_string_cols(data)
  final <- remove_high_correlation(data)
  
  save(final, file = save_path)
  final
}

fds_final_cleaned <- preprocess_fds(fds_final_data_trans, column_info_fds)
