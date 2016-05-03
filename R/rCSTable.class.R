library(methods)

setClass("rCSTable",
         # ==== Inheritance
         # ==== Properties
         representation (
           varname        = "character",
           detail         = "logical",
           by             = "character",
           to             = "character",
           ExposeFields   = "character",
           caption        = "character",
           Colnames       = "character",
           TotalExposed   = "numeric",
           TotalUnexposed = "vector",
           CasesExposed   = "vector",
           CasesUnexposed = "vector",
           ARExposed      = "vector",
           ARUnexposed    = "vector",
           RiskRatio      = "vector",
           RiskCILow      = "vector",
           RiskCIHight    = "vector",
           Pvalue         = "vector",
           rCSTable        = "data.frame"
         )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "rCSTable",
  function(.Object, df, x, exposure=list(), exact, rr, ar, pvalue, caption)
  {
#    .Object <-  .Object;
    .Object@caption <- caption;
    if (length(exposure) < 1) {
      stop("Exposure list is empty.");
    }
    
    PLabel = ifelse(exact == TRUE, "p-value (Fisher)", "p-value (chi2)")
    
    .Object@ExposeFields = as.vector(sapply(exposure, '[[', 1));
    .Object@Colnames = c("Exposure", "Tot.Exp.", "Exp.Cases", "AR%", "Tot.Unex.", "Unex.Cases", "AR%",
                         "Risk Ratio", "CI-Low", "CI-Hight", PLabel);

    computeKHI2Pvalue <- function(A, B, C, D)
    {
      t <- chisq.test(matrix(c(A,B,C,D),nc=2), correct=FALSE);
      #return(sprintf("%3.3f", t$p.value));
      return(t$p.value);
    }
    
    computeFisherPvalue <- function(A, B, C, D)
    {
      t <- fisher.test(matrix(c(A,B,C,D),nc=2));
      return(sprintf("%3.3f", t$p.value));
    }
    
    computeRiskCI <- function(risk, X1, N1, X2, N2)
    {
      A = ((N1-X1)/X1)/N1;
      B = ((N2-X2)/X2)/N2;
      R1 = log(risk) + (1.96*sqrt(A + B));
      R2 = log(risk) - (1.96*sqrt(A + B));
      E1 = exp(R1);
      E2 = exp(R2);
      
      return(c(E2, E1));
    }
    
    for (N in .Object@ExposeFields) {
#      df[,x]
      FR = table(x, df[,N]);
#      stop("Here!")
      #print(FR);
      TE = FR[1,2]+FR[2,2];
      TU = FR[1,1]+FR[2,1];
      P1 = (FR[2,2]/TE)*100;
      P0 = (FR[2,1]/TU)*100;
      
      .Object@TotalExposed = c(.Object@TotalExposed, TE);
      .Object@CasesExposed = c(.Object@CasesExposed, FR[2,2]);
      .Object@TotalUnexposed = c(.Object@TotalUnexposed, TU);
      .Object@CasesUnexposed = c(.Object@CasesUnexposed, FR[2,1]);
      .Object@ARExposed = c(.Object@ARExposed, as.numeric(sprintf("%3.2f", P1)));
      .Object@ARUnexposed = c(.Object@ARUnexposed, as.numeric(sprintf("%3.2f", P0)));
      RR = P1/P0;
      .Object@RiskRatio = c(.Object@RiskRatio, as.numeric(sprintf("%3.2f", RR)));
      CI = computeRiskCI(RR, FR[2,2], TE, FR[2,1], TU);
      .Object@RiskCILow   = c(.Object@RiskCILow, as.numeric(sprintf("%5.3f", CI[1])));
      .Object@RiskCIHight = c(.Object@RiskCIHight, as.numeric(sprintf("%5.3f", CI[2])));

      if (exact == FALSE) {
        PV = computeKHI2Pvalue(FR[1,1], FR[2,1], FR[1,2], FR[2,2]);
      } else {
        PV = computeFisherPvalue(FR[1,1], FR[2,1], FR[1,2], FR[2,2]);    
      }
      .Object@Pvalue = c(.Object@Pvalue, PV);
    }


    DF <- data.frame(cbind(.Object@ExposeFields));
    DF = cbind(DF,
      .Object@TotalExposed,
      .Object@CasesExposed,
      .Object@ARExposed,
      .Object@TotalUnexposed,
      .Object@CasesUnexposed,
      .Object@ARUnexposed,
      .Object@RiskRatio,
      .Object@RiskCILow,
      .Object@RiskCIHight,
      .Object@Pvalue
    );

    names(DF) <- .Object@Colnames;

    if (rr == TRUE) {
      DF <- DF[order(-DF[,8]),];
    } else if (ar == T) {
      DF <- DF[order(-DF[,4]),];
    } else if (pvalue == T) {
      DF <- DF[order(DF[,11]),];
    }
    
    .Object@rCSTable <- DF;
    .Object;
});

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"rCSTable" ,
  function(object) {
    digits =  c(0,0,0,0,1,0,0,1,2,3,3,4);
    align  =  c("l","r","c","c","r","c","c","r","r","r","r","r");
    ec.xtable(object@rCSTable,  digits=digits, align=align, caption=object@caption);
  }
)

# -----------------------------------------------------------------------------
# function: rCSTable (call real constructor)
# Return: an object of type rCSTable
# -----------------------------------------------------------------------------
rCSTable <- function(df, x, exposure=list(), exact=FALSE, rr=FALSE, ar=FALSE, pvalue=TRUE, caption="rCSTable")
{
  return(new("rCSTable",
             df=df,
             x=x,
             exposure=exposure,
             exact=exact,
             rr=rr,
             ar=ar,
             pvalue=pvalue,
             caption=caption));
}
