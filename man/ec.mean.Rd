\name{ec.mean}
\alias{ec.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Descriptive Statistiques : Arithmetic Mean}

\description{
Return the arithmetic mean value of a column/variable of the Global Dataset (GDS)
}
\usage{
ec.mean(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{- STRING - the column/variable name}
}

\value{
the arithmetic mean
}

\author{jp.decorps@epiconcept.fr
}

\note{
Missing values are removed
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# == Dataframe is loaded with data()
ec.Use("Tiramitsu", "df");
ec.mean('age');

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
