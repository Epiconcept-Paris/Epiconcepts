\name{ec.Use}
\alias{ec.Use}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Loading a dataframe in the Global Dataset (GDS)}
\description{
This method permit to load a dataframe in the Global Dataset (GDS). The dataframe can be loaded from existing data(), a CSV file or from an Rds file.
}
\usage{
ec.Use(df, extension)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{name of the dataframe or filename without extension}
  \item{extension}{"df" for local data, "csv" from a text file, "Rds" from a native R formated file}
}

\value{
GDS contains the requested data
}

\author{jp.decorps@epiconcept.fr}


\examples{
# == Dataframe is loaded with data()
ec.Use("Tiramitsu", "df");
ec.variance('age');
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
