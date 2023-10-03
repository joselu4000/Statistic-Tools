#### Initial Tests ####
#' This function give you some basic statistic tests
#' 
#' @param data Your dataset with the variables from which you want to analyze
#' @param two_by_two Optional param to calculate some test two by two
#' @param alpha Optional param to change alpha for all tests which have that option
#'
#' @return t.test, shapiro.test, var.test, kurtosis, skrewness
#'
#' @examples
#' # Previously install and read: "mlbench"
#' data("iris")
#' data <- iris[,-5]
#' test <- initial_tests(data, two_by_two = TRUE)
#' 
#' @export
#
initial_tests <- function(data, two_by_two = NULL, alpha = NULL){
  # Necessary library
  library(MASS)
  library(nortest)
  library(moments) # to kurtosis
  library(e1071) # to skewness
  
  # Initial memory
  memory <- list()
  
  # Initial text
  text <- paste('Reference text for tests lecture: In H0 columns',0,'is FALSE and',1,'is TRUE')
  cat(text,'\n \n')
  
  # Initial param for function
  nc <- ncol(data)
  if (is.null(alpha)){alpha = 0.05}
  conflevel <- 1-alpha
  
  # Mean equal (student test)
  mean_equal <- t.test(data, conf.level = conflevel)
  memory[['mean equal']] <- mean_equal
  cat('Test for equality of means was performed \n')
  
  if (!is.null(two_by_two)){
    m <- nc*(nc-1)/2
    nom <- c('(i,j)','statistic t','parameter df','p.value','H0',
             'conf.int.low','conf.int.hig','estimate i','estimate j')
    means_equal <- matrix(nrow = m, ncol = 9)
    colnames(means_equal) <- nom
    s = 1
    for (i in 1:nc){
      for (j in i:nc){
        if (i != j){
          aux <- t.test(data[,i],data[,j], conf.level = conflevel)
          means_equal[s,1] <- paste(i,',',j)
          means_equal[s,2] <- aux$statistic
          means_equal[s,3] <- aux$parameter
          means_equal[s,4] <- aux$p.value
          if (aux$p.value <= alpha){
            means_equal[s,5] <- 0
          } else {means_equal[s,5] <- 1}
          conf_aux <- aux$conf.int
          means_equal[s,6] <- conf_aux[1]
          means_equal[s,7] <- conf_aux[2]
          estim_mean <- aux$estimate
          means_equal[s,8] <- estim_mean[1]
          means_equal[s,9] <- estim_mean[2]
          s = s+1
        }
      }
    }
    memory[['mean equal two by two']] <- means_equal
    cat('(Optional 1) 2 by 2 tests for equality of means was performed \n')
  }
  #----------------------------------------------------------------------------#
  # Normal test (shapiro-wilk)
  nom <- c('statistic W','p.value','H0')
  shapiro_test <- matrix(nrow = nc, ncol = 3)
  colnames(shapiro_test) <- nom
  for (i in 1:nc){
    aux <- shapiro.test(data[,i])
    shapiro_test[i,1] <- aux$statistic
    shapiro_test[i,2] <- aux$p.value
    if (aux$p.value <= alpha){
      shapiro_test[i,3] <- 0
    } else {shapiro_test[i,3] <- 1}
  }
  ind_norm <- if (prod(shapiro_test[,3])==1){1}else{0}
  memory[['shapiro tests']] <- shapiro_test
  memory[['normality indicator']] <- ind_norm
  cat('Normality test was conducted \n')
  
  #==> Kurtosis studio if normality is FALSE
  if (ind_norm == 0){
    nom <- c('k','Form')
    kurtosis_test <- matrix(nrow = nc, ncol = 2)
    colnames(kurtosis_test) <- nom
    for (i in 1:nc){
      k <- kurtosis(data[,i])
      kurtosis_test[i,1] <- k
      if (k == 3) {
        text_aux <- 'Similar to Normal'
      } else if (k < 3) {
        text_aux <- 'Platicurtica'
      } else {
        text_aux <- 'Colas mas pesadas'
      }
      kurtosis_test[i,2] <- text_aux
    }
    memory[['kurtosis test']] <- kurtosis_test
    cat('--> Given the non-normal distribution, a kurtosis test was performed \n')
  }
  
  #==> Symmetry studio if normality is FALSE
  if (ind_norm == 0){
    nom <- c('k','Form')
    asym_test <- matrix(nrow = nc, ncol = 2)
    colnames(asym_test) <- nom
    for (i in 1:nc){
      k <- skewness(data[,i])
      asym_test[i,1] <- k
      if (k == 0) {
        text_aux <- 'Symmetric'
      } else if (k < 0) {
        text_aux <- 'Skewed to the left'
      } else {
        text_aux <- 'Skewed to the right'
      }
      asym_test[i,2] <- text_aux
    }
    memory[['asymmetry test']] <- asym_test
    cat('--> Given the non-normal distribution, a asym test was performed \n')
  }
  
  #----------------------------------------------------------------------------#
  # Homoscedasticity (var.test)
  m <- nc*(nc-1)/2
  nom <- c('(i,j)','statistic F','parameter df','p.value','H0',
           'conf.int.low','conf.int.hig','estimate i','estimate j')
  vars_equal <- matrix(nrow = m, ncol = 9)
  colnames(vars_equal) <- nom
  s = 1
  for (i in 1:nc){
    for (j in i:nc){
      if (i != j){
        aux <- var.test(data[,i],data[,j],conf.level = conflevel)
        vars_equal[s,1] <- paste(i,',',j)
        vars_equal[s,2] <- aux$statistic
        vars_equal[s,3] <- aux$parameter[1]
        vars_equal[s,4] <- aux$p.value
        if (aux$p.value <= alpha){
          vars_equal[s,5] <- 0
        } else {vars_equal[s,5] <- 1}
        conf_aux <- aux$conf.int
        vars_equal[s,6] <- conf_aux[1]
        vars_equal[s,7] <- conf_aux[2]
        estim_var <- aux$estimate
        vars_equal[s,8] <- estim_var[1]
        vars_equal[s,9] <- estim_var[2]
        s = s+1
      }
    }
  }
  ind_homocedasticity <- if (prod(as.numeric(vars_equal[,5]))==1){1}else{0}
  memory[['var equal two by two']] <- vars_equal
  memory[['homoscedasticity indicator']] <- ind_homocedasticity
  cat('Homoscedasticity test (2 by 2) was conducted \n')
  
  return (memory)
}