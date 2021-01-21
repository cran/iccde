\name{iccde}
\alias{icc.de}
\alias{iccde}
\title{Computation of Double-Entry Intraclass Correlation}
\usage{
icc.de(prof1, prof2, input = c("cor", "score"), digits = 3)
}

\arguments{
\item{prof1}{Vector of components of the nomological network of the first trait
(input = "cor") or vector of components of the first profile (input = "score")}
\item{prof2}{Vector of components of the nomological network of the second trait
(input = "cor") or vector of components of the second profile (input = "score")}
\item{digits}{Number of digits in the output of the function, DEFAULT = 3}
\item{input}{Do the profiles contain correlations (e.g., from construct
validation) or test scores on different scales (e.g., from diverse personality
tests)?}
}

\description{The function computes the profile similarity measure double-entry
intraclass correlation. It is a more precise index of the agreement of two
empirically observed profiles than the often used intraclass correlation or
profile correlation. Profiles comprising correlations are transformed according
to the Fisher z transformation before the index is calculated. In the case of
score data with different scale formats (e.g., if the profile contains
different IQ or a variety of personality tests), it is recommended to
z-standardize the scores before entering into the function. For details, see
Furr (2010) <doi:10.1080/00223890903379134> or McCrae (2008)
<doi:10.1080/00223890701845104>.}

\value{
\item{ICC_DE}{Double-Entry Intraclass Correlation for two given profiles}
}

\examples{
icc.de(prof1 = c(.59, .48, .23), prof2 = c(.52, .76, .22), input = "cor")

icc.de(prof1 = c(59, 48, 23), prof2 = c(52, 76, 22), input = "score",
digits = 2)
}

\author{
Christian Blötner, Michael Paul Grosz,
\email{c.bloetner@gmail.com, grosz@wwu.de}}
\keyword{&htest}
\references{
Furr, R. M. (2010). The Double-Entry Intraclass Correlation as an Index of
Profile Similarity: Meaning, Limitations, and Alternatives. Journal of
Personality Assessment, 92(1), 1-15. https://doi.org/10.1080/00223890903379134

McCrae, R. R. (2008). A Note on Some Measures of Profile Agreement. Journal of
Personality Assessment, 90(2), 105-109.
https://doi.org/10.1080/00223890701845104
}
