\name{ec.Labels}

\alias{ec.Labels}

\title{DataFrame Tools : (Re)Set labels}

\description{Set or reset labels for one or more columns}

\usage{
ec.Labels(x, to = "", labels = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ - a string or a list of string of column(s) name(s)}
  \item{to}{ - a string, column name when a range of contiguous columns is used (range x - to)}
  \item{labels}{ - a vector of "current value"="new value" see examples}
}
\details{
TODO
}
\value{TRUE. GDS the Global DataSet is modified}

\author{jp.decorps@epiconcept.fr}

\examples{
library(Epiconcepts)
# -----------------------------------------------------------
# Set labels of a column
# -----------------------------------------------------------
data(mtcars)
ec.Use(mtcars)
ec.Labels("am", labels=c("0"="No", "1"="Yes"))

# -----------------------------------------------------------
# Set labels for a list of columns
# -----------------------------------------------------------
data(mtcars)
ec.Use(mtcars)
ec.Labels(list("vs","am"), labels=c("0"="No", "1"="Yes"))

# -----------------------------------------------------------
# Set labels for a range of contiguous columns
# -----------------------------------------------------------
data(mtcars)
ec.Use(mtcars)
ec.Labels("vs", to="am", labels=c("0"="No", "1"="Yes"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
