library(methods)

setClass("ecr.summary",
   # ==== Inheritance
   # ==== Properties
   representation (
     varname      = "character",
     detail       = "logical",
     type         = "character",
     by           = "character",
     to           = "character",
     summary      = "data.frame"
   )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ecr.summary",
  function(.Object, x, namevar="x", detail=FALSE, ...)
  {
    
    .Object@detail  <- detail;
    .Object@varname <- namevar;
    
    .Object@type <- class(x);
    
    if (.Object@type == "factor") {
      .Object@summary <- p.factorSummary(x, namevar);
      return(.Object);
    }
    
    if (detail == FALSE) {
      .Object@summary <- p.simpleSummary(x, namevar);
    }
    else {
      .Object@summary <- p.detailSummary(x, namevar);
    }
    .Object;
  }
);

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"ecr.summary" ,
  function(object) {
    
    if (object@type == "factor") {
      #digits =  c(0,0,0);
      align  =  c("l","r","c","c");
      ec.xtable(object@summary, align=align);
    }
    else {
      if (object@detail == TRUE) {
        #digits =  c(0,0,5);
        #align  =  c("l","r","c");
        ec.xtable(object@summary, align=c("l","r","c"), digits=c(0,0,5));
      }
      else {
        #digits =  c(0,0,4,4,4,4,4);
        align  =  c("l","r","c","r","r","c","c");
        ec.xtable(object@summary, align=align);
      }
    }
  }
)

# -----------------------------------------------------------------------------
# function: ecr.summary (call real constructor)
# Return: an object of type ecr.summary
# -----------------------------------------------------------------------------
ecr.summary <- function(x, ...)
{
  return(new("ecr.summary", x=x, ...));
}
