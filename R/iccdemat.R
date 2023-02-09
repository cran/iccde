#' Computation of Double-Entry Intraclass Correlation
#'
#' Test for matrices comprising of correlations
#'
#' @param dat A symmetric c x c correlation matrix. It can be easily computed
#' with the basic R function 'cor(data)'. Assymetric matrices with different
#' numbers of columns and rows are not supported.
#' @return A matrix of double-entry intraclass correlations in which redundant
#' entries are removed.
#' @export
#'
#' @examples
#' df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100),
#' x = rnorm(100), y = rnorm(100), z = rnorm(100))
#' dat <- cor(df)
#' icc.de.mat(dat)

icc.de.mat <- function(dat){
  r <- matrix(0, nrow = ncol(dat), ncol = ncol(dat))

  for (i in 1:ncol(dat)) {
    for (j in 1:ncol(dat)) {
      r[i, j] <- icc.de(dat[-c(i, j), i], dat[-c(i, j), j])
      if(i <= j){r[i, j] <- ""}
    }
  }
  colnames(r) <- colnames(dat)
  rownames(r) <- rownames(dat)

  return(r)
}
