#' get correlation matrix with significance stars
#'
#' this function gets correlation matrix with sig stars
#' @param df dataframe for correlations
#' @param digits number of digits to print
#' @return print of correlation matrix with sig stars

#' @examples
#' \dontrun{
#' cor_matrix(df, digits = 4)
#' }
#' @export

corz <- function(df, digits = 4){

  correlation <- psych::corr.test(df)

  cor <- correlation$r
  ps <- correlation$p

  cor_table <- data.frame(matrix(vector(), nrow = ncol(df), ncol = ncol(df)))
  colnames(cor_table) <- colnames(df)
  rownames(cor_table) <- colnames(df)

  for (j in 1:ncol(cor_table)){
    for (i in 1:ncol(cor_table)){
      if (i != j & i >= j){
        if (ps[i, j] <= 0.05 & ps[i, j] > 0.01){
          cor_table[i, j] <- paste0(round(cor[i,j],digits), '*')
        } else if(ps[i, j] <= 0.01 & ps[i, j] > 0.001){
          cor_table[i, j] <- paste0(round(cor[i,j],digits), '**')
        } else if(ps[i, j] <= 0.001){
          cor_table[i, j] <- paste0(round(cor[i,j],digits), '***')
        } else {
          cor_table[i, j] <- round(cor[i,j],digits)
        }
      } else if (i!= j & i <= j){
        cor_table[i, j] <- cor_table[j, i]
      }
      else if (i == j) {
        cor_table[i, j] <- round(cor[i, j], digits)
      }
    }
  }

  return(print(cor_table, right = F))
}

