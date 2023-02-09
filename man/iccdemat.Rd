\name{icc.de.mat}
\alias{icc.de.mat}
\title{Computation of Double-Entry Intraclass Correlations for correlation
matrices}
\usage{
icc.de.mat(dat)
}

\arguments{
\item{dat}{A symmetric c x c correlation matrix. It can be easily computed with
the basic R function 'cor()'. Assymetric matrices with different numbers of
columns and rows are not supported.}
}

\description{
This function computes pairwise double-entry intraclass correlations among
all entries of a symmetric correlation matrix. Correlations between the compared
variables as well as correlations of the constructs with themselves are thereby
excluded (i.e., correlations between X and Y, correlation between X and X, and
between correlation between Y and Y).}

\value{
\item{iccde}{Double-Entry Intraclass Correlation}}

\examples{
\dontrun{

df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100),
                 x = rnorm(100), y = rnorm(100), z = rnorm(100))
dat <- cor(df)
icc.de.mat(dat)
}
}

\author{
Christian Blötner, Michael Paul Grosz
\email{c.bloetner@gmail.com}}
\keyword{&htest}
\references{
Furr, R. M. (2010). The Double-Entry Intraclass Correlation as an Index of
Profile Similarity: Meaning, Limitations, and Alternatives. Journal of
Personality Assessment, 92(1), 1--15.
https://doi.org/10.1080/00223890903379134

McCrae, R. R. (2008). A Note on Some Measures of Profile Agreement. Journal of
Personality Assessment, 90(2), 105--109.
https://doi.org/10.1080/00223890701845104
}
