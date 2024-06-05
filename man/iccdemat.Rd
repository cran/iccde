\name{icc.de.mat}
\alias{icc.de.mat}
\title{Computation of Double-Entry Intraclass Correlations for correlation
matrices}
\usage{
icc.de.mat(data, digit)
}

\arguments{
\item{data}{A data frame with participants in rows and variables in columns.}
\item{digit}{Number of digits in the output. The default is 3.}
}

\description{
This function computes pairwise double-entry intraclass correlations among
all entries of a data frame. Correlations between the compared variables as well
as correlations of the constructs with themselves are thereby excluded from the
calculation (i.e., correlations between X and Y, correlation between X and X,
and correlation between Y and Y).}

\value{
\item{iccde}{Double-Entry Intraclass Correlation}}

\examples{

df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100),
                 x = rnorm(100), y = rnorm(100), z = rnorm(100))
icc.de.mat(df)
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
