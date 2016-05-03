\name{ec.cc}
\alias{ec.cc}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{EPIDEMIOLOGY : Tables for epidemiologists - Case-control odds ratio}

\description{
cc is used with case-control and cross-sectional data.  Point estimates
    and confidence intervals for the odds ratio are calculated, along with
    attributable or prevented fractions for the exposed and total population.}

\usage{
ec.cc(x, exposure = "", exact = FALSE, where = vector(), title = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{STRING Cases variable}
  \item{exposure}{STRING - stratify on varname}
  \item{exact}{NOT USED}
  \item{where}{VECTOR - Conditional statement}
  \item{title}{STRING - Title of table}
}

\value{A dataframe}

\references{Stata 13: cc}
\author{jp.decorps@epiconcept.fr}


\examples{
ec.use("Tiramitsu", "df");
ec.cc("ill", "tira");
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
