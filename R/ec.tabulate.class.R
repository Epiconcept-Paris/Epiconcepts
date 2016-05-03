library(methods)

setClass("ec.tabulate",
         # ==== Inheritance
         # ==== Properties
         representation (
           varname      = "character",
           missing      = "logical",
           by           = "character",
           to           = "character",
           tabulate     = "data.frame"
         )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ec.tabulate",
  function(.Object, x, nolabel, missing)
  {
  
#   getNames <- function(x, nolabel)
#   {
#     if (nolabel == FALSE) {
#       return(names(x));
#     }
#     return(seq(0, length(names(x))-1, by=1));
#   }
  
  frequencies <- function(miss)
  {
    .MIS = sum(is.na(GDS[, x]));
    .OBS = nrow(GDS) - .MIS;
    
    if (miss == TRUE) {
      .OBS = .OBS + .MIS;
    }
    
    .freq = table(GDS[, x]);
    DF <- p.frequencies(.freq, .OBS, nolabel, x)
    DF;
  }

  .Object@tabulate <- frequencies(missing)
  .Object;
});


# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"ec.tabulate" ,
  function(object) {
    #digits =  c(0,0,0,0,1,0,0,1,2,3,3,4);
    align  =  c("l","r","c","r","r");
#     df <- xtable(object@tabulate, align=align);
#     print(df, type = "html", include.rownames = F);
    ec.xtable(object@tabulate, align = align)
  }
)

# -----------------------------------------------------------------------------
# function: ec.tabulate (call real constructor)
# Return: an object of type ec.tabulate
# -----------------------------------------------------------------------------
ec.tabulate <- function(x, nolabel=FALSE, missing=TRUE)
{
  return(new("ec.tabulate", x=x, nolabel=nolabel, missing=missing));
}
