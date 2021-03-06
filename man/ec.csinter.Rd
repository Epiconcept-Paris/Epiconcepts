\name{ec.csinter}
\alias{ec.csinter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{EPIDEMIOLOGY: Stratified analysis for Cohort studies}
\description{Do a multivariate stratified analysis, presenting results (RR and 95 percent CI) for each stratum and adjusted result.
 In addition an additive interaction table is constructed
}
\usage{
ec.csinter(x, exposure = "", by = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ string: illness binary variable (0 / 1) }
  \item{exposure}{ string: exposure binary variable (0 / 1) }
  \item{by}{  string: strata variable (a factor) }
}
\details{You don't have to provide a data.frame name as parameter.
The data.frame used is GDS which is build by ec.use.
}
\value{
Object of class ec.csinter
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
