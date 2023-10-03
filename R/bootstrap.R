#### Bootstrap ####
#' This function give you and method Bootstrap to simulate dataset with a previously data.
#'
#' @param data Your dataset with the variables by columns
#' @param numsim Number of simulation with de number of your data that you want
#' @param Graph You can decide whether to use (TRUE) or not (FALSE) print all associated plots
#' @param solution_data Optional param: You can decide if sustitute yours NAs data by means of the column or omit then is NULL default
#'
#'
#' @return clean data, simulations, and some statistic
#'
#' @examples
#' # Previously install and read: "mlbench"
#' data("iris")
#' data <- iris[,-5]
#' BS <- bootstrap(data, numsim = 3, Graph = TRUE, solution_data = NULL)
#' 
#' @export
bootstrap <- function(data, numsim, Graph, solution_data = NULL){
  # Memory initial
  memory <- list()
  
  # Optional process for improve data
  if (!is.null(solution_data)) {
    if (sol == 'na_omit'){
      datos <- na.omit(data)
    }
    if (sol == 'means'){
      nc <- ncol(data)
      nr <- nrow(data)
      datos <- data
      for (i in 1:nr){
        for (j in 1:nc){
          if (is.na(data[i,j])){
            datos[i,j] <- mean(datos[,j],na.rm = TRUE)
          }
        }
      }
    }
  }
  else{
    datos <- data
  }
  
  # Introduce clean data in memory 
  memory[['clean_data']] <- datos 
  
  # Bootstrap itself
  datos_t <- t(datos)
  nc <- ncol(datos_t)
  nr <- nrow(datos_t)
  r = nc*numsim
  matriz <- matrix(nrow = nr, ncol = r)
  #
  for (i in 1:nr){
    aux_vector = c()
    k = 1
    while (k <= numsim) {
      aux_vector <- c(aux_vector,
                      sample(c(datos_t[i,]),
                             size = nc,
                             replace = TRUE)
      )
      k <- k+1
    }
    matriz[i,] <- unlist(aux_vector)
  }
  name <- colnames(data)
  simulation <- t(matriz)
  colnames(simulation) <- name
  
  # Introduce simulation in memory
  memory[['simulation']] <- simulation
  
  # Plots
  if (Graph == TRUE){
    {
      color <- c(1:ncol(datos))
      x <- 1:ncol(matriz)
      M <- max(matriz)
      titulo_bootstrap <- paste("Data + Bootstrap nº simulation:",r)
      plot(x,matriz[1,], col = color[1],
           ylim = c(0,M), main = titulo_bootstrap,
           xlab = "Evolution time",
           ylab = "Data+Bootstrap",
           pch = 1
      )
      for (i in 2:4){
        points(x,matriz[i,], col = color[i], pch = 10+i)
      }
      legend("topleft", legend = name, col = color, pch = 19)
    }
  }
  
  # Statistic
  # Mean
  means_initial <- colMeans(data)
  means_simulation <- colMeans(simulation)
  abs_difference_means <- abs(means_initial-means_simulation)
  MEANS <- rbind(means_initial,means_simulation,abs_difference_means)
  # Var
  var_initial <- apply(data, 2, function(x) var(x))
  var_simulation <- apply(simulation, 2, function(x) var(x))
  abs_difference_var <- abs(var_initial-var_simulation)
  VAR <- rbind(var_initial, var_simulation, abs_difference_var)
  # Desviation Coeficient
  coef_desviation_initial <- 100*sqrt(var_initial)/means_initial
  coef_desviation_simulation <- 100*sqrt(var_simulation)/means_simulation
  abs_difference_coef_desviation <- abs(coef_desviation_initial-coef_desviation_simulation)
  Coef_Desviation <- rbind(coef_desviation_initial,
                           coef_desviation_simulation,
                           abs_difference_coef_desviation)
  
  # Introduce statistics in memory
  memory[['means']] <- MEANS
  memory[['variance']] <- VAR
  memory[['desviation_coeficient']] <- Coef_Desviation
  
  return (memory)
}
