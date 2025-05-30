# OFD & FDS & SVM01 Analysis Suite

This repository contains preprocessing, feature-engineering, clustering, and modeling scripts for three datasets: **OFD**, **FDS**, and **SVM01**. The code is organized into R and Python files, each encapsulating specific workflows.

## Files

- **preprocessing_OFD_FDS.R**  
  Implements data cleaning and transformation steps for both OFD and FDS datasets:
  - Column name mapping
  - Low-variance filtering
  - Suffix-based column removal
  - Timestamp and date string conversion
  - Correlation-based feature reduction
  - Saves the cleaned dataset to `fds_final_cleaned.RData`

- **functions_OFD_FDS.R**  
  Defines modular R functions to apply each preprocessing step independently and a wrapper `preprocess_fds()` that runs the full pipeline on raw data and saves the result.

- **SVM01_algorithms.R**  
  Demonstrates classification workflows using several algorithms on all three datasets:
  - Random Forest  
  - XGBoost  
  - Support Vector Machine (SVM)  
  - Prints performance metrics and variable importance

- **Pythonwork.py**  
  Contains Python implementations for advanced feature analysis:
  - `compute_rf_importances()`: trains a Random Forest on `sampled_data_louvain.csv`, prints & plots the top-N feature importances  
  - `feature_clock_pipeline()`: builds a UMAP embedding, fits a NonLinearClock visualization on selected features, and saves a feature-clock plot

## Getting Started

1. **Preprocessing (R)**
   ```r
   source("preprocessing_OFD_FDS.R")
   cleaned <- preprocess_fds(fds_raw, column_info_fds)
