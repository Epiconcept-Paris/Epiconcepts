\name{ecr.csinter}
\alias{ecr.csinter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{EPIDEMIOLOGY: Stratified analysis for Cohort studies}
\description{Do a multivariate stratified analysis, presenting results (RR and 95 percent CI) for each stratum and adjusted result.
 In addition an additive interaction table is constructed
}
\usage{
ecr.csinter(df, x, exposure = "", by = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{ data.frame: Control-Cases data}
  \item{x}{ string: illness binary variable (0 / 1) }
  \item{exposure}{ string: exposure binary variable (0 / 1) }
  \item{by}{  string: strata variable (a factor) }
}
\details{This function is same as ec.csinter, but you have to provide a data.frame.}
\value{
Object of class ecr.csinter
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% TODO
}
\author{jp.decorps@epiconcept.fr}
\note{
%% TODO
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
ecr.csinter
}

\examples{
%% TODO
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
