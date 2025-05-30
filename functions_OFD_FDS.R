library(ggplot2)
library(umap)
library(randomForest)
library(dbscan)
library(cluster)
library(RANN)
library(igraph)
library(viridis)
library(TDA)

perform_umap <- function(data,
                         sample_size = 5000,
                         seed = 123,
                         color = "royalblue",
                         alpha = 0.7,
                         point_size = 1.5,
                         ylim = c(-10, 15)) {
  set.seed(seed)
  idx          <- sample(nrow(data), min(sample_size, nrow(data)))
  sampled_data <- data[idx, ]
  scaled_data  <- scale(sampled_data)
  umap_res     <- umap(scaled_data)
  umap_df      <- data.frame(UMAP1 = umap_res$layout[,1],
                             UMAP2 = umap_res$layout[,2])
  
  p <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2)) +
    geom_point(color = color, alpha = alpha, size = point_size) +
    labs(title = "UMAP Projection", x = "UMAP 1", y = "UMAP 2") +
    coord_cartesian(ylim = ylim) +
    theme_minimal(base_size = 14)
  
  print(p)
  invisible(list(umap       = umap_res,
                 plot       = p))
}

perform_pca <- function(data,
                        sample_size = 5000,
                        seed        = 123,
                        alpha       = 0.6,
                        color       = "steelblue") {
  set.seed(seed)
  
  idx          <- sample(nrow(data), min(sample_size, nrow(data)))
  numeric_data <- data[idx, sapply(data, is.numeric)]
  scaled_data  <- scale(numeric_data)
  pca_res      <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
  
  pca_df       <- data.frame(PC1 = pca_res$x[,1], PC2 = pca_res$x[,2])
  
  p <- ggplot(pca_df, aes(x = PC1, y = PC2)) +
    geom_point(alpha = alpha, color = color) +
    labs(title = "PCA Projection", x = "PC1", y = "PC2") +
    coord_cartesian(xlim = range(pca_df$PC1) * 0.8,
                    ylim = range(pca_df$PC2) * 0.8) +
    theme_minimal(base_size = 14)
  
  print(p)
  invisible(list(pca     = pca_res,
                 scatter = p))
}

perform_hdbscan <- function(data,
                            sample_size = 5000,
                            seed        = 123,
                            minPts      = 20,
                            alpha       = 0.7,
                            point_size  = 1.5,
                            ylim        = c(-10, 15)) {
  set.seed(seed)
  idx     <- sample(nrow(data), min(sample_size, nrow(data)))
  sampled <- data[idx, ]
  scaled  <- scale(sampled)
  umap_res <- umap(scaled)
  hdb      <- hdbscan(scaled, minPts = minPts)
  umap_df  <- data.frame(UMAP1 = umap_res$layout[,1],
                         UMAP2 = umap_res$layout[,2],
                         cluster = as.factor(hdb$cluster))
  
  p <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
    geom_point(alpha = alpha, size = point_size) +
    labs(title = "UMAP Projection Colored by HDBSCAN Clusters", color = "Cluster") +
    coord_cartesian(ylim = ylim) +
    theme_minimal(base_size = 14) +
    scale_color_viridis_d()
  
  print(p)
  invisible(list(umap = umap_res,
                 clusters = hdb$cluster,
                 plot = p))
}

perform_louvain <- function(data,
                            sample_size = 5000,
                            seed        = 123,
                            k           = 15,
                            alpha       = 0.7,
                            point_size  = 1.5,
                            ylim        = c(-10, 15)) {
  set.seed(seed)
  idx     <- sample(nrow(data), min(sample_size, nrow(data)))
  sampled <- data[idx, ]
  scaled  <- scale(sampled)
  umap_res <- umap(scaled)
  nn      <- nn2(scaled, k = k)
  edges   <- do.call(rbind, lapply(seq_len(nrow(nn$nn.idx)), function(i) {
    data.frame(from = i, to = nn$nn.idx[i, ])
  }))
  edges   <- edges[edges$from != edges$to, ]
  g       <- graph_from_data_frame(edges, directed = FALSE)
  comm    <- cluster_louvain(g)
  clusters <- comm$membership
  umap_df  <- data.frame(UMAP1 = umap_res$layout[,1],
                         UMAP2 = umap_res$layout[,2],
                         cluster = as.factor(clusters))
  
  p <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
    geom_point(alpha = alpha, size = point_size) +
    labs(title = "UMAP Projection Colored by Louvain Clusters", color = "Cluster") +
    coord_cartesian(ylim = ylim) +
    theme_minimal(base_size = 14) +
    scale_color_viridis_d()
  
  print(p)
  invisible(list(umap = umap_res,
                 clusters = clusters,
                 plot = p))
}

perform_tda <- function(data,
                        cluster_col,
                        target_clusters = NULL,
                        sample_size = 5000,
                        seed = 123) {
  set.seed(seed)
  idx     <- sample(nrow(data), min(sample_size, nrow(data)))
  sampled <- data[idx, ]
  
  if (is.null(target_clusters)) {
    target_clusters <- unique(sampled[[cluster_col]])
  }
  
  num_cols <- sapply(sampled, is.numeric)
  X_all    <- scale(as.matrix(sampled[, num_cols]))
  dists    <- as.vector(dist(X_all))
  maxscale <- quantile(dists, 0.90)
  
  pd_list <- lapply(target_clusters, function(k) {
    ids <- which(sampled[[cluster_col]] == k)
    ripsDiag(X = X_all[ids, , drop = FALSE],
             maxdimension = 1,
             maxscale = maxscale,
             dist = "euclidean",
             library = "GUDHI",
             printProgress = FALSE)$diagram
  })
  names(pd_list) <- paste0("Cluster_", target_clusters)
  
  op <- par(mfrow = c(1, length(pd_list)), mar = c(4, 4, 2, 1))
  for (nm in names(pd_list)) {
    plot(pd_list[[nm]], main = nm, diagLim = c(0, maxscale), cex = 0.6, legend = FALSE)
  }
  par(op)
  
  invisible(list(diagrams = pd_list, maxscale = maxscale))
}

perform_kmeans <- function(data,
                           sample_size = 5000,
                           seed        = 123,
                           centers     = 15,
                           nstart      = 25,
                           alpha       = 0.7,
                           point_size  = 1.5,
                           ylim        = c(-10, 15)) {
  set.seed(seed)
  idx     <- sample(nrow(data), min(sample_size, nrow(data)))
  sampled <- data[idx, ]
  numeric <- sampled[sapply(sampled, is.numeric)]
  scaled  <- scale(numeric)
  km      <- kmeans(scaled, centers = centers, nstart = nstart)
  umap_res <- umap(scaled)
  umap_df  <- data.frame(UMAP1 = umap_res$layout[,1],
                         UMAP2 = umap_res$layout[,2],
                         cluster = as.factor(km$cluster))
  
  p <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
    geom_point(alpha = alpha, size = point_size) +
    labs(title = "K-Means Clusters Visualized with UMAP", color = "Cluster") +
    coord_cartesian(ylim = ylim) +
    theme_minimal(base_size = 14) +
    scale_color_viridis_d()
  
  print(p)
  invisible(list(umap   = umap_res,
                 kmeans = km,
                 plot   = p))
}

