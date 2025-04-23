\name{iccde}
\alias{icc.de}
\title{Computation of the Double-Entry Intraclass Correlation between two
profiles}
\usage{
icc.de(prof1, prof2, input = c("cor", "score"), use = "pairwise", digits = 2)
}

\arguments{
\item{prof1}{Vector of components of the nomological network of the first
variable (\code{input = "cor"}) or vector of components of the first profile
(\code{input = "score"}).}
\item{prof2}{Vector of components of the nomological network of the second
variable (\code{input = "cor"}) or vector of components of the second profile
(\code{input = "score"}).}
\item{input}{Do the profiles contain correlations (e.g., from nomological
network; \code{input = "cor"}) or scores from different scales (e.g., sum scores
from diverse personality tests; \code{input = "score"})? The default is
\code{input = "cor"}.}
\item{use}{Optional character string specifying how to deal with missing values.
The input will be forwarded to the base \code{cor} function and can be one of
the following strings, including abbreviation: "everything", "all.obs",
"complete.obs", "na.or.complete", or "pairwise.complete.obs". The default is
\code{use = "pairwise"}.}
\item{digits}{Number of digits in the output. The default is digits = 2.}
}

\description{
The function computes the double-entry intraclass correlation,
which is an index of profile similarity (Furr, 2010; McCrae, 2008).
The double-entry intraclass correlation is a more precise index of the agreement
of two empirically observed profiles than the often-used intraclass correlation
(McCrae, 2008). The function transforms profiles comprising correlations
according to the Fisher z-transformation before the double-entry intraclass
correlation is calculated. If the profiles comprise scores such as sum scores
from various personality scales, it is recommended to standardize each
individual score prior to computation of the double-entry intraclass correlation
(McCrae, 2008). See Furr (2010) <doi:10.1080/00223890903379134> or
McCrae (2008) <doi:10.1080/00223890701845104> for details.
}

\value{
\item{iccde}{Double-Entry Intraclass Correlation for two given profiles}}

\examples{
icc.de(prof1 = c(.59, .48, .23), prof2 = c(.52, .76, .22), input = "cor")

icc.de(prof1 = c(-1, -0.85, 2), prof2 = c(-0.93, 1, 1.26), input = "score",
digits = 4)
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
