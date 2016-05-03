\name{ec.median}
\alias{ec.median}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Descriptive Statistiques : Median Value
}
\description{
Return the median value of a column/variable of the Global Dataset (GDS)
}
\usage{
ec.median(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{- STRING - the column/variable name}
}


\value{
the median value
}

\author{jp.decorps@epiconcept.fr}
\note{
Missing values are removed
}


\examples{
# == Dataframe is loaded with data()
ec.Use("Tiramitsu", "df");
ec.median('age');
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
