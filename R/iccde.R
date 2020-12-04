#' Computation of Double-Entry Intraclass Correlation
#'
#' Test for correlations
#'
#' @param prof1 vector of components of the nomological network of the
#' first trait (input = "cor") or vector of components of the first profile
#' (input = "score")
#' @param prof2 vector of components of the nomological network of the
#' second trait (input = "cor") or vector of components of the second profile
#' (input = "score")
#' @param digits number of digits in the output of the function
#' @return Double-Entry Intraclass Correlation
#' @export
#'
#' @examples
#' icc.de(prof1 = c(.59, .48, .23), prof2 = c(.52, .76, .22), input = "cor",
#' digits = 3)
#'
icc.de <- function(prof1, prof2, input = c("cor", "score"), digits = 3){
  input <- match.arg(input)
  kind <- switch(input, cor = 1, score = 2)
  if(kind == 1){
    x.cor <- atanh(prof1)
    y.cor <- atanh(prof2)
    xy.cor <- c(x.cor, y.cor)
    yx.cor <- c(y.cor, x.cor)
    coeff = round(cor(xy.cor, yx.cor), digits)}

  if(kind == 2){
    xy <- c(prof1, prof2)
    yx <- c(prof2, prof1);
    coeff = round(cor(xy, yx), digits)}

  out <- data.frame("ICC_DE" = coeff);
  return(out)
}
