\name{ec.epiCurve}
\alias{ec.epiCurve}

\title{Plot an epidemic curve}
\description{
This function plot an epidemic curve  (need ggplot2)}
\usage{
ec.epiCurve(x, date = NULL,freq = NULL, cutvar = NULL, 
   period = NULL, cutorder = NULL, colors = NULL,
   title = NULL, xlabel = NULL, ylabel=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data.frame with at least one column with Date type}
  \item{date}{character, name of Date column}
  \item{freq}{character, name of a column with a value to display}
  \item{cutvar}{character, name of a column with factors}
  \item{period}{character, c("day","week")}
  \item{cutorder}{character vector of factors}
  \item{colors}{character vector of colors}
  \item{title}{character, title of the plot}
  \item{xlabel}{character, label for x axis}
  \item{ylabel}{character, label for y axis}
}

\references{https://fr.wikipedia.org/wiki/Courbe_épidémique }
\author{<jp.decorps@epiconcept.fr> & <m.vallee@epiconcept.fr>}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
