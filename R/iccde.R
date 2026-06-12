#' Computation of Double-Entry Intraclass Correlation
#'
#' Test for correlations or test scores
#'
#' @param prof1 Vector of components of the correlation profile of the
#' first variable (input = "cor") or vector of components of the first profile
#' comprising test scores (input = "score")
#' @param prof2 Vector of components of the correlation profile of the
#' second variable (input = "cor") or vector of components of the second profile
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

icc.de <- function(prof1,
                   prof2,
                   input = c("cor", "score"),
                   use = "pairwise",
                   digits = 2,
                   plot = TRUE,
                   legend = TRUE,
                   pos.legend = "topleft",
                   main = NA,
                   name_prof1 = "prof1",
                   name_prof2 = "prof2"){

  if(length(prof1) != length(prof2)){
    warning("The profiles have different lengths. Please double-check!")}

  choice <- match.arg(input)
  input.kind <- switch(choice,
                       cor = 1,
                       score = 2)

  if(input.kind == 1){
    x.cor <- atanh(prof1)
    y.cor <- atanh(prof2)
    xy <- c(x.cor,
            rev(y.cor))
    yx <- c(y.cor,
            rev(x.cor))

    Mean_prof1 <- tanh(mean(atanh(prof1)))
    Mean_prof2 <- tanh(mean(atanh(prof2)))

    SD_prof1 <- tanh(sd(atanh(prof1)))
    SD_prof2 <- tanh(sd(atanh(prof2)))

    }

  if(input.kind == 2){
    xy <- c(prof1,
            rev(prof2))
    yx <- c(prof2,
            rev(prof1))

    Mean_prof1 <- mean(prof1)
    Mean_prof2 <- mean(prof2)

    SD_prof1 <- sd(prof1)
    SD_prof2 <- sd(prof2)

    }

  iccde <- round(cor(xy,
                     yx,
                     use = use),
                 digits)

  df <- data.frame(iccde,
                   Mean_prof1 = round(Mean_prof1,
                                      digits),
                   Mean_prof2 = round(Mean_prof2,
                                      digits),
                   SD_prof1 = round(SD_prof1,
                                    digits),
                   SD_prof2 = round(SD_prof2,
                                    digits))

  if(plot == TRUE && input.kind == 1){
    plot(NA,
         ylim = c(min(c(prof1,
                        prof2)) - .10,
                  max(c(prof1,
                        prof2)) + .10),
         xlim = c(0,
                  length(prof1) + 1),
         ylab = substitute(paste(italic("r"))),
         xlab = "Criteria",
         frame.plot = FALSE,
         main = main,
         xaxt = "none",
         yaxt = "none",
         xaxs = "i",
         yaxs = "i")

    axis(1,
         at = 1:length(prof1))
    axis(2,
         at = seq(-1,
                  1,
                  .10))

    lines(1:length(prof1),
          prof1,
          col = "red",
          pch = 0,
          lty = "solid",
          type = "b")

    lines(1:length(prof2),
          prof2,
          col = "blue",
          pch = 3,
          lty = "dotted",
          type = "b")

    if(legend){

      legend(pos.legend,
             legend = c(name_prof1, name_prof2),
             col = c("red", "blue"),
             pch = c(0, 3),
             bty = "n")

    }
  }

  if(plot && input.kind == 2){
    plot(NA,
         ylim = c(min(c(prof1,
                        prof2)) - .10,
                  max(c(prof1,
                        prof2)) + .10),
         xlim = c(0,
                  length(prof1) + 1),
         ylab = "Score",
         xlab = "Criteria",
         frame.plot = FALSE,
         main = main,
         xaxt = "none",
         yaxt = "none",
         xaxs = "i",
         yaxs = "i")

    axis(1,
         at = 1:length(prof1))
    axis(2,
         at = seq(round(min(c(prof1,
                              prof2),
                            3) - .10),
                  round(max(c(prof1,
                              prof2),
                            3) + .10),
                  .10))

    lines(1:length(prof1),
          prof1,
          col = "red",
          pch = 0,
          lty = "solid",
          type = "b")

    lines(1:length(prof2),
          prof2,
          col = "blue",
          pch = 3,
          lty = "dotted",
          type = "b")

    if(legend){

      legend(pos.legend,
             legend = c(name_prof1, name_prof2),
             col = c("red", "blue"),
             pch = c(0, 3),
             bty = "n")

    }
  }

  return(df)

}
