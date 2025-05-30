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
  Provides modular R functions for exploratory analysis and clustering on OFD and FDS datasets, including:  
  - `perform_umap()`: UMAP projection of sampled & scaled data  
  - `perform_pca()`: PCA projection scatter plot  
  - `perform_hdbscan()`: HDBSCAN clustering visualization on UMAP embedding  
  - `perform_louvain()`: Louvain community detection on k-NN graph + UMAP plot  
  - `perform_tda()`: Topological data analysis via persistence diagrams for specified clusters  
  - `perform_kmeans()`: K-means clustering on scaled data with UMAP visualization  

- **SVM01_algorithms.R**  
  Demonstrates classification workflows using several algorithms on the third dataset:
  - Random Forest  
  - XGBoost  
  - Support Vector Machine (SVM)  
  - Prints performance metrics and variable importance

- **Pythonwork.py**  
  Contains Python implementations for advanced feature analysis:
  - `compute_rf_importances()`: trains a Random Forest on the clustered data to detect the top-N important features  
  - `feature_clock_pipeline()`: builds a UMAP embedding, fits a NonLinearClock visualization on selected features, and saves a feature-clock plot

- **preprocessing_SVM01.R**  
  Implements data cleaning and transformation steps for the SVM01 dataset:  
  - NA-based column removal
  - Constant-value column removal 
  - Random sampling and near-zero variance filtering 
  - Mean imputation for remaining numeric NAs 
  - Correlation-based feature reduction

 - **svm01_datasets_for_algorithms.R**  
  Prepares specialized SVM01 subsets for modeling and evaluation:
  - Merges cleaned SVM01 with final_data_rvm01, preserving a specified set of high-NA columns
  - Filters rows to four target company groups and creates a Group factor
  - Engineers fee-code signal metrics (non-NA/non-zero count, sum, any-nonzero indicator) from columns 316–365
  - Detects highly correlated fee columns, aggregates each group into a new summed feature, and removes the originals
  - Splits data into dataset_a (347 non-NA) and dataset_b (347 NA) for separate analyses
  - Constructs model-ready outputs: dataset_a, dataset_b, svm_for_material, and svm_for_market

## Getting Started
### OFD - FDS
1. **Preprocessing (R)**
   ```r
   source("preprocessing_OFD_FDS.R")
   cleaned <- preprocess_fds(fds_raw, column_info_fds)

2. **Example function**
   ```r
    result_ofd <- perform_louvain(
    data       = ofd_subset_data,
    sample_size= 5000,
    seed       = 123,
    k          = 30,
    alpha      = 0.7,
    point_size = 1.5,
    ylim       = c(-10, 15))
  
### SVM01
1. **Preprocessing**
   ```r
    source("preprocessing_SVM01.R")
    df_clean <- remove_na_columns(df, threshold = na_threshold)
    df_clean <- remove_constant_columns(df_clean)
    df_sampled <- process_and_sample_data(df_clean, sample_frac = sample_frac)
    df_decorrelated <- remove_highly_correlated(df_sampled, cutoff = cor_cutoff)
    df_to_use <- impute_numeric_means(df_decorrelated)
2. **Prepare Datasets for testing**
    ```r
    source("svm01_datasets_for_algorithms.R")
    <- 
    
