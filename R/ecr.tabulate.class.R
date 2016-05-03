library(methods)

setClass("ecr.tabulate",
         # ==== Inheritance
         # ==== Properties
         representation (
           varname      = "character",
           missing      = "logical",
           caption      = "character",
           tabulate     = "data.frame"
         )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ecr.tabulate",
          function(.Object, data, x, nolabel, missing, varname, caption)
          {

            .Object@varname <- varname;
            .Object@caption <- caption;
            
            frequencies <- function(data, x, miss, namevar)
            {
              .MIS = sum(is.na(x));
              .OBS = nrow(data) - .MIS;
              
              if (miss == TRUE) {
                .OBS = .OBS + .MIS;
              }
              
              .freq = table(x);
              DF <- p.frequencies(.freq, .OBS, nolabel, namevar)
              DF;
            }
            .Object@tabulate <- frequencies(data, x, missing, varname)
            .Object;
          });


# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"ecr.tabulate" ,
          function(object) {
            #digits =  c(0,0,0,0,1,0,0,1,2,3,3,4);
            align  =  c("l","r","c","r","r");
            ec.xtable(object@tabulate, align = align, caption=object@caption)
          }
)

# -----------------------------------------------------------------------------
# function: ecr.tabulate (call real constructor)
# Return: an object of type ecr.tabulate
# -----------------------------------------------------------------------------
ecr.tabulate <- function(df, x, nolabel=FALSE, missing=TRUE, varname="X", caption="")
{
  return(new("ecr.tabulate", data=df, x=x, nolabel=nolabel, missing=missing, varname=varname, caption=caption));
}
