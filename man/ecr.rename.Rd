\name{ecr.rename}
\alias{ecr.rename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DataFrame Tools : Rename a column
}
\description{ Rename a column of a data.frame }
\usage{
ecr.rename(df, colname, newname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{- data.frame }
  \item{colname}{- STRING - the column/variable name}
  \item{newname}{- STRING - the new column/variable name}
}

\value{
A data.frame with the column renamed
}

\author{jp.decorps@epiconcept.fr}

\note{
Memory consumption?
}

\examples{
# == Dataframe is loaded with data()
data(iris);
iris <- ecr.rename(iris, 'Sepal.Length','SL');
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
