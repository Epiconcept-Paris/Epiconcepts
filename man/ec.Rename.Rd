\name{ec.Rename}
\alias{ec.Rename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DataFrame Tools : Renaming a column}

\description{
  Rename a column (variable) of GDS.
  GDS is the Global DataSet.
}
\usage{
ec.Rename(str1, str2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str1}{ - a string value, the current name of the column (variable)}
  \item{str2}{ - a string value, the new name of the column (variable)}
}
\details{
TODO
}

\value{Return nothing. GDS the Global DataSet is modified}

\references{
TODO
}
\author{jp.decorps@epiconcept.fr}
\note{
TODO
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
TODO
}
\examples{
data(mtcars)
ec.Use(mtcars)
colnames(GDS)
ec.Rename("cyl", "Cylinders")
colnames(GDS)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
