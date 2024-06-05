\name{icc.de.boot}
\alias{icc.de.boot}
\title{Bootstrap Confidence Intervals for Double-Entry Intraclass Correlations)
}
\description{For different applications, confidence intervals for the double-
entry intraclass correlation can be useful. Bootstrap confidence intervals are
computed by means of repeated resampling from the original data at hand.
}
\usage{
icc.de.boot(data, n.sim = 1000, alpha = .05, digit = 3)
}

\arguments{
\item{data}{A data frame with participants in rows and variables in columns.
Users should restrict the data set to the variables of concrete interest
because the all available information in the data frame will be used to
compute bootstrapped confidence intervals. Thus, it is advisable to create a
new data frame that entails only the variables on which the matrix of ICCDEs
should be based. Computations in a data set with 56 variables on a Windows
device (i5 CPU, 8 GB RAM) and a Macbook Pro (M3 Pro CPU, 36 GB RAM), for
instance, took about two minutes, given 1,000 iterations.}
\item{n.sim}{The number of iterations to be carried out. Default is 1,000.}
\item{alpha}{Type I error. Default is .05.}
\item{digit}{Number of digits in the output. Default is 3.}
}

\value{The output provides a list of matrices of double-entry intraclass
correlations.
\item{Mean}{A matrix of the bootstrapped point estimates of the bootstrapped
double-entry intraclass correlations.}
\item{LL}{A matrix providing the lower limits of the bootstrap confidence
intervals, given the desired alpha level.}
\item{UL}{A matrix providing the upper limits of the bootstrap confidence
intervals, given the selected alpha level.}
}

\author{
Christian Blötner, Michael Paul Grosz
\email{c.bloetner@gmail.com}}
\keyword{&htest}
\references{
Furr, R. M. (2010). The Double-Entry Intraclass Correlation as an index of
profile similarity: Meaning, limitations, and alternatives. Journal of
Personality Assessment, 92(1), 1--15.
<https://doi.org/10.1080/00223890903379134>

McCrae, R. R. (2008). A note on some measures of profile agreement. Journal of
Personality Assessment, 90(2), 105--109.
<https://doi.org/10.1080/00223890701845104>
}

\examples{
df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100),
                 x = rnorm(100), y = rnorm(100), z = rnorm(100))
icc.de.boot(data = df,
            n.sim = 1000,
            alpha = .01,
            digit = 2)
}
