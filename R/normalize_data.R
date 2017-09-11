
#  ------------------------------------------------------------------------
#' df_norm_0_1(df, column = 1)
#' @description normalize data_frame column to interval [0,1]
#' @param df data frame name
#' @param column column numeric specification which will be normalized
#'
#' @return data frame with normalize column
#' @examples
#'           df <- data.frame(x= 1:10, y = 21:30)
#'           df_norm_0_1(df, column = 2)
df_norm_0_1 <- function(df, column = 1) {
  df_min <- min(df[,column], na.rm = TRUE)
  df_max <- max(df[,column], na.rm = TRUE)
  df[,column] <- (df[,column] - df_min) / (df_max - df_min)
  return(df)
}
