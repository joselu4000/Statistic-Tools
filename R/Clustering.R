#### Clustering ####
#' This function is used to calculate and represent clusters based on the 
#' standard or Euclidean distance
#'
#' @param data Your dataset with the variables from which you want to obtain clusters by columns
#' @param data_form Indicate where are your variables: 'r' is row and default is column
#' @param numb_cluster Number of clusters that you want
#' @param elbow_rule You can decide whether to use (TRUE) or not (FALSE) the associated elbow rule
#' @param optimal_k You can decide whether to use (TRUE) or not (FALSE) the associated optimal elbow rule
#' @param Graph You can decide whether to use (TRUE) or not (FALSE) print all associated plots
#'
#' @return Data+clusters, elbow-associated k, optimal elbow-associated k, and plots
#'
#' @examples
#' # Previously install and read: "mlbench"
#' data("iris")
#' data <- iris[,-5]
#' CK <- clustering(data, 'r', numb_cluster = 3, elbow_rule = TRUE, optimal_k = TRUE, Graph = TRUE)
#' 
#' @export
# Data must contain its variables by column
clustering <- function(data, data_form, numb_cluster, elbow_rule, optimal_k, Graph){
    memory <- list()
    # Data lecture and clear
    data <- na.omit(data)
    lim <- ncol(data)
    if (data_form == 'r'){
      data <- t(data)
      lim <- nrow(data)-1
    }
    # Cluster analysis with a pre-established number 
    k <- numb_cluster
    cluster_kmeans <- kmeans(data, centers = k)
    data_cluster <- cbind(data, as.matrix(cluster_kmeans$cluster))
    name <- "Clusters"
    colnames(data_cluster)[ncol(data_cluster)] <- name
    # Save in memory
    memory[['clusters']] <- data_cluster

    # Elbow rule
    if (elbow_rule == TRUE){
      Ks <- c(1:lim)
      h <- c()
      for (i in Ks){
        cluster_kmeans_2 <- kmeans(data, centers = i)
        h <- c(h,cluster_kmeans_2$tot.withinss[1])
      }
      possible <- h[1:9]-h[2:10]
      max <- which.max(possible)
      elbow <- max+1
      if (Graph == TRUE){
      par(mfrow = c(1,1))
      plot(Ks, h, type = "o", col = "blue", pch = 19, lwd = 2,
          xlab = "Number cluster", ylab = "Withinss", 
          main = "Elbow rule")
      text(Ks, h, labels = Ks, pos = 2, col = "red")
      abline(v = elbow, col = "red", lty = 2)
      }
      # Save in memory
      memory[['classic_elbow']] <- elbow
    }
    
    # Optimal Elbow rule
    if (optimal_k == TRUE){
    inertia <- vector("numeric", length = lim)
    for (i in 1:lim) {
        kmeans_result <- kmeans(data, centers = i)
        inertia[i] <- kmeans_result$tot.withinss
    }
    centroides <- kmeans(data, centers = 1)$centers
    # Dispersion inter-cluster
    inter_cluster_dispersion <- vector("numeric", length = lim)
    for (i in 1:lim) {
        kmeans_result <- kmeans(data, centers = i)
        cluster_centers <- kmeans_result$centers
        inter_cluster_distances <- matrix(0, nrow = i, ncol = i)
        for (j in 1:i) {
        for (k in 1:i) {
            inter_cluster_distances[j, k] <- sqrt(sum((cluster_centers[j,] - cluster_centers[k,])^2))
        }
        }
        inter_cluster_dispersion[i] <- mean(inter_cluster_distances)
    }
    # Pseudo F-statistic
    pseudo_f_statistic <- (inertia / inter_cluster_dispersion) / (length(inertia) - 1)
    # Find optimal number of cluster
    optimal_k <- which.max(pseudo_f_statistic)
    memory[['F-statistic']] <- pseudo_f_statistic
    memory[['optimal_number_k']] <- optimal_k
    
    # Graphic 
    if (Graph == TRUE){
    plot(1:lim, pseudo_f_statistic, type = "b", pch = 19, frame.plot = FALSE,
        col = "blue", xlim = c(0,lim), ylim = c(0,max(pseudo_f_statistic[2:lim])), 
        main = "Optimal elbow rule",
        xlab = "Number of Clusters (k)", ylab = "Pseudo F-statistic")
    text(1:10, pseudo_f_statistic, labels = Ks, pos = 2, col = "red")
    abline(v = optimal_k, col = "red", lty = 2)
    }
    }

    #
    if (Graph == TRUE){
      n <- length(data[1,])
      x <- 1:n
      means <- colMeans(data_cluster[,-1])
      m <- min(means)
      M <- max(means)
      plot(x, means, col = cluster_kmeans$cluster, pch = 19, 
          main = "Clusters",
          xlab = "Data evolution", ylab = "Means",
          ylim = c(m,M))
      legend("topright", legend = unique(cluster_kmeans$cluster), 
              col = 1:length(unique(cluster_kmeans$cluste)), 
              pch = 19, title = "Clustering")
    }
    return (memory)
}