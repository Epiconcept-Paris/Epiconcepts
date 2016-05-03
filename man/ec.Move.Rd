\name{ec.Move}
\alias{ec.Move}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DataFrame Tools : Move a column}
\description{
Move a column to the first column, or at the end of the dataset or before or after a specified column
}
\usage{
ec.Move(tomove, where = "last", target = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tomove}{ - a string, the name of the column to be moved}
  \item{where}{ - a string, can be "first", "last" (default), "before", "after".}
  \item{target}{ - an optional string, the name of the relative column when "before" or "after" are used}
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
This function is a derivative work from an answer than you can see here :

http://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe

}
\examples{
data(mtcars)
ec.Use(mtcars)
colnames(GDS);
ec.Move("mpg", "last")
colnames(GDS)
ec.Move("mpg", "first")
colnames(GDS)
ec.Move("mpg", "before", "am")
colnames(GDS)
ec.Move("mpg", "after", "am")
colnames(GDS)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
