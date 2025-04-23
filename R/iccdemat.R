#' Computation of Double-Entry Intraclass Correlation
#'
#' Test for matrices comprising of correlations
#'
#' @param data A data frame with participants in rows and variables in columns.
#' @param digit Number of digits in the output. The default is 3.
#'
#' @return A matrix of double-entry intraclass correlations in which redundant
#' entries are removed.
#' @export
#'
#' @examples
#' df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100),
#'                  x = rnorm(100), y = rnorm(100), z = rnorm(100))
#' icc.de.mat(df)

icc.de.mat <- function(data,
                       use = "pairwise",
                       digit = 3){

  mat <- cor(data, use = use)

  r <- matrix(0, nrow = ncol(mat), ncol = ncol(mat))

  for (i in 1:ncol(mat)) {
    for (j in 1:ncol(mat)) {
      r[i, j] <- icc.de(mat[-c(i, j), i], mat[-c(i, j), j],
                        digits = digit)
    }
  }

  colnames(r) <- colnames(mat)
  rownames(r) <- rownames(mat)

  return(r)
}
