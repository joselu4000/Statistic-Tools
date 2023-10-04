#### Sustitute by median ####
#' This function is used to improve your NAs data by a median value of its measure
#'
#' @param data Your data set
#' @param data_form Indicate where are your variables: 'r' is row and default is 
#' column but you have to write something
#' 
#' @return Data improve by median values
#'
#' @examples
#' matriz <- matrix(c(1, NA, 3, 4, 5, 6, NA, 8, 9), nrow = 3, ncol = 3, byrow = TRUE)
#' colnames(matriz) <- c("Variable1", "Variable2", "Variable3")
#' susmedian(matriz,data_form = 'c')
#'  
#' @export
susmedian <- function(data, data_form){
  memory <- list()
  if (data_form == 'c'){datos <- data} else{datos <- t(data)}
  for (i in 1:ncol(datos)){
    aux <- datos[,i]
    NAs <- which(is.na(aux))
    for (j in NAs){
        aux[j] <- mean(datos[,i],na.rm = TRUE) 
    }
    datos[,i] <- aux
  }
  if (data_form != 'c'){datos <- t(datos)}
  memory[['data non NAs']] <- datos
  return (memory)
}
