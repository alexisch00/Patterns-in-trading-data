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

## Getting Started

1. **Preprocessing (R)**
   ```r
   source("preprocessing_OFD_FDS.R")
   cleaned <- preprocess_fds(fds_raw, column_info_fds)
