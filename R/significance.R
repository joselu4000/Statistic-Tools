#### Significance Tests ####
#' This function give you significance tests (maybe 2 by 2)
#' 
#' @param data Your dataset with yours models by column
#' @param anova_forced You can forced anova tests (and 2 by 2)
#' @param alpha Optional param to change alpha for all tests
#'
#' @return anova, mann-whitney, kruskal-wallis
#'
#' @examples
#' # Previously install and read: "mlbench"
#' data("iris")
#' data <- iris[,-5]
#' sig <- significance_models_tests(data, anova_forced = TRUE)
#' 
#' @export
#
significance_models_tests <- function(data_models,  anova_forced = NULL, alpha = NULL){
  # Initial library
  library(stats)
  
  # Initial memory
  memory <- list()
  
  # Initial param for function
  nc <- ncol(data_models)
  if (is.null(alpha)){alpha = 0.05}
  conflevel <- 1-alpha
  if (is.null(anova_forced)){f <- 0}else{f <- 1}
  
  # Initial call to initial_tests
  test <- initial_tests(data_models, two_by_two = TRUE, alpha = alpha)
  memory[['test']] <- test
  
  norm <- test$`normality indicator`
  homo <- test$`homoscedasticity indicator`
  cat('\n Significance tests: \n')
  if (norm == 0 && homo == 0){
    cat('Non applicable anova test \n')
    
    #--------------------------------------------------------------------------#
    # Test de Mann-Whitney
    ind_t_test <- if (test$`mean equal`$p.value <= alpha){0}else{1}
    if (ind_t_test == 0){
      m <- nc*(nc-1)/2
      nom <- c('(i,j)','statistic W','p.value','H0')
      mw <- matrix(nrow = m, ncol = 4)
      colnames(mw) <- nom
      s = 1
      for (i in 1:nc){
        for (j in i:nc){
          if (i != j){
            aux <- wilcox.test(data_models[,i],data_models[,j], conf.level = conflevel)
            mw[s,1] <- paste(i,',',j)
            mw[s,2] <- aux$statistic
            mw[s,3] <- aux$p.value
            if (aux$p.value <= alpha){
              mw[s,4] <- 0
            } else {mw[s,4] <- 1}
            s = s+1
          }
        }
      }
      memory[['mann-whitney 2 by 2']] <- mw
      cat('--> 2 by 2 tests Mann-Whitney was performed \n')
    }
    
    #--------------------------------------------------------------------------#
    # Kruskal-Wallis Simple
    kw_tot <- kruskal.test(data_models)
    memory[['krustal-wallis total']] <- kw_tot
    cat('--> Krustal-Wallis total was performed \n')
    
    #--------------------------------------------------------------------------#
    # Kruskal-Wallis 2 by 2
    m <- nc*(nc-1)/2
    nom <- c('(i,j)','statistic Chi-squared','parameter df','p.value','H0','Confirm')
    kw <- matrix(nrow = m, ncol = 6)
    colnames(kw) <- nom
    s = 1
    for (i in 1:nc){
      for (j in i:nc){
        if (i != j){
          aux <- kruskal.test(data_models[,c(i,j)])
          kw[s,1] <- paste(i,',',j)
          kw[s,2] <- aux$statistic
          kw[s,3] <- aux$parameter
          kw[s,4] <- aux$p.value
          if (aux$p.value <= alpha){
            kw[s,5] <- 0
          } else {kw[s,5] <- 1}
          value_aux <- qchisq(1-alpha,aux$parameter)
          if (value_aux <= aux$statistic){
            text_aux <- 'There are evidences of association'
          } else {
            text_aux <- 'Non evidences of association'
          }
          kw[s,6] <- text_aux
          s = s+1
        }
      }
    }
    memory[['krustal-wallis 2 by 2']] <- kw
    cat('--> 2 by 2 tests Krustal-Wallis was performed \n')
    
    #--------------------------------------------------------------------------#
    if(f == 1){
      # Anova total 
      anova_tot <- aov(data_models[,1] ~ .,data = data_models)
      memory[['anova tot']] <- anova_tot
      cat('Anova test was performed \n')
      
      # Anova 2 by 2
      cat('\n Do you want anova 2 by 2? \n')
      answer_str <- readline("Yes or No: ")
      if (answer_str == "Yes"){
        cat('\n Anova 2 by 2 \n')
        for (i in 1:nc){
          for (j in i:nc){
            if (i != j){
              cat('Column',i,'Column',j,'\n')
              aux <- aov(data_models[,i] ~ data_models[,j], data = data_models)
              print(aux)
            }
          }
        }
        cat('2 by 2 anovas was print \n')
      }
    }
  }
  else{
    #--------------------------------------------------------------------------#
    # Anova total 
    anova_tot <- aov(data_models[,1] ~ .,data = data_models)
    memory[['anova tot']] <- anova_tot
    cat('Anova test was performed \n')
    
    # Anova 2 by 2
    cat('\n Do you want anova 2 by 2? \n')
    answer_str <- readline("Yes or No: ")
    if (answer_str == "Yes"){
      cat('\n Anova 2 by 2 \n')
      for (i in 1:nc){
        for (j in i:nc){
          if (i != j){
            cat('Column',i,'Column',j,'\n')
            aux <- aov(data_models[,i] ~ data_models[,j], data = data_models)
            print(aux)
          }
        }
      }
      cat('2 by 2 anovas was print \n')
    }
    
  }
  return (memory)
}