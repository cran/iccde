#' Computation of Double-Entry Intraclass Correlation
#'
#' Test for correlations
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
#' digits = 3)
#'
icc.de<- function(prof1, prof2, input = c("cor", "score"), digits = 2){

  if(input == "cor"){
    x.cor <- atanh(prof1);
    y.cor <- atanh(prof2);
    xy <- c(x.cor, y.cor);
    yx <- c(y.cor, x.cor)}

  if(input == "score"){
    xy <- c(prof1, prof2);
    yx <- c(prof2, prof1)}
  
  iccde <- round(cor(xy, yx, use = "pairwise"), digits);
  NAs_1 <- sum(is.na(prof1));
  NAs_2 <- sum(is.na(prof2));
  
  if(NAs_1 >= 1 & NAs_2 == 0){
    coeff <- data.frame(iccde, NAs_1)};
  
  if(NAs_2 >= 1 & NAs_1 == 0){
    coeff <- data.frame(iccde, NAs_2)};
  
  if(NAs_1 >= 1 & NAs_2 >= 1){
    coeff <- data.frame(iccde, NAs_1, NAs_2)};
  
  if(NAs_1 == 0 & NAs_2 == 0){
    coeff <- iccde}
  
  return(coeff)
}