#' Computation of Double-Entry Intraclass Correlation
#'
#' Test for matrices comprising of correlations
#'
#' @param data A data frame with participants in rows and variables in columns.
#' Users should restrict the data set to the variables of concrete interest
#' because the all available information in the data frame will be used to
#' compute bootstrapped confidence intervals. Thus, it is advisable to create a
#' new data frame that entails only the variables on which the matrix of ICCDEs
#' should be based.
#' @param n.sim The number of iterations to be carried out. Default is 1,000.
#' @param alpha Type I error. Default is .05.
#' @param digit Number of digits in the output. Default is 3.
#'
#' @return The output provides a list of matrices of double-entry intraclass
#' correlations. The first matrix comprises the bootstrapped point estimates of
#' the bootstrapped double-entry intraclass correlations, the second contains
#' the lower limits of the bootstrap confidence intervals, given the desired
#' alpha level, the third one contains the upper limits of the bootstrap
#' confidence intervals, given the desired alpha level.
#'
#' @export
#'
#' @examples
#' df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100),
#'                  x = rnorm(100), y = rnorm(100), z = rnorm(100))
#' icc.de.boot(data = df,
#'             n.sim = 1000,
#'             alpha = .01,
#'             digit = 2)

icc.de.boot <- function(data,
                        n.sim = 1000,
                        alpha = .05,
                        use = "pairwise",
                        digit = 3){

  mat <- cor(data,
             use = use)

  m <- rep(NA,
           nrow(mat))

  sd <- rep(NA,
            nrow(mat))

  array <- array(0,
                 dim = c(ncol(data),
                         ncol(data),
                         n.sim))

  for(m in 1:n.sim){
    index <- sample(1:nrow(data),
                    replace = TRUE)

    cormat <- cor(data[index, ],
                  use = use)

    r <- matrix(0,
                nrow = nrow(cormat),
                ncol = ncol(cormat))

    for(i in 1:nrow(cormat)){
      for(j in 1:ncol(cormat)){

        r[i, j] <- icc.de(prof1 = cormat[-c(i, j), i],
                          prof2 = cormat[-c(i, j), j],
                          plot = FALSE)$iccde
      }
    }

    colnames(r) <- colnames(data)
    rownames(r) <- colnames(data)

    array[1:nrow(cormat),
          1:ncol(cormat),
          m] <- r
  }

  dimnames(array) <- list(colnames(r),
                          colnames(r),
                          1:n.sim)

  for(i in 1:nrow(mat)){

    m[i] <- tanh(mean(atanh(mat[i, -i])))

    sd[i] <- tanh(sd(atanh(mat[i, -i])))

  }

  out <- list(M = round(apply(array,
                              c(1, 2),
                              function(x) mean(x,
                                               na.rm = TRUE)),
                        digit),
              LL = round(apply(array,
                               c(1, 2),
                               function(x) quantile(x,
                                                    probs = (alpha / 2),
                                                    na.rm = TRUE)),
                         digit),
              UL = round(apply(array,
                               c(1, 2),
                               function(x) quantile(x,
                                                    probs = 1 - (alpha / 2),
                                                    na.rm = TRUE)),
                         digit),
              Mean = round(m,
                           digit),
              SD = round(sd,
                         digit))

  return(out)
}
