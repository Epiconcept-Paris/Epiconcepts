\name{ec.min}
\alias{ec.min}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Descriptive Statistiques : Minima
}
\description{
Return the min value of a column/variable of the Global Dataset (GDS)
}

\usage{ec.min(x)}
\arguments{
  \item{x}{- STRING - the column/variable name}
}

\value{
the min value
}

\author{jp.decorps@epiconcept.fr}
\note{
Missing values are removed
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
ec.Use("Tiramitsu", "df");
ec.min('age');
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
