library(methods)

setClass("ec.summary",
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
setMethod("initialize", "ec.summary",
  function(.Object, x, detail=FALSE, ...)
  {
    
    .Object@detail = detail;
    
    
    T <- sapply(GDS, class);
    .Object@type <- T[x];
    
    if (.Object@type == "factor") {
      .Object@summary <- p.factorSummary(GDS[,x], x);
      return(.Object);
    }
    
    if (detail == FALSE) {
      .Object@summary <- p.simpleSummary(GDS[,x], x);
    }
    else {
      .Object@summary <- p.detailSummary(GDS[,x], x);
    }
    .Object;
  }
);

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"ec.summary" ,
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
# function: ec.summary (call real constructor)
# Return: an object of type ec.summary
# -----------------------------------------------------------------------------
ec.summary <- function(x, detail=FALSE)
{
  return(new("ec.summary", x=x, detail=detail));
}
