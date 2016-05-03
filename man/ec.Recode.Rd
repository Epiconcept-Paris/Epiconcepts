\name{ec.Recode}
\alias{ec.Recode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DataFrame Tools : Recode the content of a column}

\description{
  Recode the content of a column (variable) of GDS.
  GDS is the Global DataSet.
}
\usage{
ec.Recode(x, where, by, gen = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ - a string value, the name of the column to be recoded}
  \item{where}{ - a list of logical expressions}
  \item{by}{ - a list of values matching the list of logical expressions. Originals values will be replaced by these}
  \item{gen}{ - optional string, the name of the new column}
}
\details{
TODO
}

\value{Return nothing. GDS the Global DataSet is modified}

\author{jp.decorps@epiconcept.fr}

\examples{
# -----------------------------------------------------------
# Recode the 'carb' column with 0 if carb is lower than 3
# and with 1 if 'carb' higher or equal than 3
# -----------------------------------------------------------
data(mtcars)
ec.Use(mtcars);
str(GDS)
ec.Recode("carb", where = list(VAL("carb") < 3, VAL("carb") >= 3), by=list(0,1))
str(GDS)

# -----------------------------------------------------------
# Create a new column (variable) named 'carGroup' which contains the
# recoding of the 'carb' column. The 'carb' column is not modified.
# -----------------------------------------------------------
data(mtcars)
ec.Use(mtcars);
str(GDS)
ec.Recode("carb", where = list(VAL("carb") < 3, VAL("carb") >= 3), by=list(0,1), gen="carbGroup")
str(GDS)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
