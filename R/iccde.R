#' Computation of Double-Entry Intraclass Correlation
#'
#' Test for correlations or test scores
#'
#' @param prof1 Vector of components of the correlation profile of the
#' first trait (input = "cor") or vector of components of the first profile
#' comprising test scores (input = "score")
#' @param prof2 Vector of components of the correlation profile of the
#' second trait (input = "cor") or vector of components of the second profile
#' comprising test scores (input = "score")
#' @param input Defines which kind of data are entered into the function:
#' Correlations ("cor", DEFAULT) or individual test scores ("score")
#' @param digits Number of digits in the output of the function
#' @return Double-Entry Intraclass Correlation
#' @export
#'
#' @examples
#' icc.de(prof1 = c(.59, .48, .23), prof2 = c(.52, .76, .22), input = "cor",
#' digits = 2)
#'
#' icc.de(prof1 = c(-1, -0.85, 2), prof2 = c(-0.93, 1, 1.26), input = "score",
#' digits = 4)

icc.de <- function(prof1, prof2, input = c("cor", "score"), digits = 2){

  if(length(prof1) != length(prof2)){
    warning("The profiles have different lengths. Please double-check!")}

  choice <- match.arg(input)
  input.kind <- switch(choice, cor = 1, score = 2)

  if(input.kind == 1){
    x.cor <- atanh(prof1);
    y.cor <- atanh(prof2);
    xy <- c(x.cor, rev(y.cor));
    yx <- c(y.cor, rev(x.cor))
    }

  if(input.kind == 2){
    xy <- c(prof1, rev(prof2));
    yx <- c(prof2, rev(prof1))
    }

  iccde <- round(cor(xy, yx), digits);
  return(iccde)
}
