library(methods)

setClass("rCCTable",
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
     TotalCases     = "vector",
     CasesExposed   = "vector",
     CasesUnexposed = "vector",
     PCAExposed     = "vector",
     TotalCtrl      = "vector",
     CtrlExposed    = "vector",
     PCTExposed     = "vector",
     OddsRatio      = "vector",
     CILow          = "vector",
     CIHight        = "vector",
     Pvalue         = "vector",
     rCCTable        = "data.frame"
  )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "rCCTable",
  function(.Object, df, x, exposure=list(), exact, rr, ar, pvalue, caption)
  {
    .Object <-  .Object;
    .Object@caption <- caption;
    
    if (length(exposure) < 1) {
      stop("Exposure list is empty.");
    }
            
    PLabel = ifelse(exact == TRUE, "p-value (Fisher)", "p-value (chi2)")
            
    .Object@ExposeFields = as.vector(sapply(exposure, '[[', 1));
    .Object@Colnames = c("Exposure", "T.Cases.", "Exposed", "%", "Tot.Ctrls", "Exposed", "%",
                                 "Odds Ratio", "CI-Low", "CI-Hight", PLabel);
            
            computeKHI2Pvalue <- function(A, B, C, D)
            {
              t <- chisq.test(matrix(c(A,B,C,D),nc=2), correct=FALSE);
              return(sprintf("%3.3f", t$p.value));
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
              FR = table(df[,x], df[,N]);
#              print(FR);
              TE = FR[1,2]+FR[2,2];
              TU = FR[1,1]+FR[2,1];
              TCA = FR[2,1]+FR[2,2];
              TCT = FR[1,1]+FR[1,2];
              P1 = (FR[2,2]/TCA)*100;
              P0 = (FR[1,2]/TCT)*100;
              
              .Object@TotalCases = c(.Object@TotalCases, TCA);
              .Object@CasesExposed = c(.Object@CasesExposed, FR[2,2]);
              .Object@PCAExposed = c(.Object@PCAExposed, as.numeric(sprintf("%3.2f", P1)));
              .Object@TotalCtrl = c(.Object@TotalCtrl, TCT);
              .Object@CtrlExposed = c(.Object@CtrlExposed, FR[1,2]);
              .Object@PCTExposed = c(.Object@PCTExposed, sprintf("%3.2f", P0));
              ODR = (FR[2,2]/FR[2,1]) / (FR[1,2]/FR[1,1]);
              RR = ODR
              .Object@OddsRatio = c(.Object@OddsRatio, as.numeric(sprintf("%3.2f", ODR)));
#               CI = computeOddsRatioCI(ODR, FR[1,1],FR[1,2],FR[2,1],FR[2,2]);
              CI = computeExactORCI(FR[1,1],FR[1,2],FR[2,1],FR[2,2]);
              .Object@CILow   = c(.Object@CILow, sprintf("%3.2f", CI[1]))
              .Object@CIHight = c(.Object@CIHight, sprintf("%3.2f", CI[2]))
              PV = sprintf("%3.3f", CI[3]);
              if (exact == FALSE) {
                PV = computeKHI2Pvalue(FR[1,1], FR[2,1], FR[1,2], FR[2,2]);
              }
#               else {
#                 PV = computeFisherPvalue(FR[1,1], FR[2,1], FR[1,2], FR[2,2]);    
#               }
              .Object@Pvalue = c(.Object@Pvalue, PV);
            }
            
            
            DF <- data.frame(cbind(.Object@ExposeFields));
            DF = cbind(DF,
                       .Object@TotalCases,
                       .Object@CasesExposed,
                       .Object@PCAExposed,
                       .Object@TotalCtrl,
                       .Object@CtrlExposed,
                       .Object@PCTExposed,
                       .Object@OddsRatio,
                       .Object@CILow,
                       .Object@CIHight,
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
            
    .Object@rCCTable <- DF;
    .Object;
  }
);


# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"rCCTable" ,
  function(object){
    cat("OUTPUT ---> \n")
    digits =  c(0,0,0,0,1,0,0,1,2,3,3,4);
    align  =  c("l","r","c","c","r","c","c","r","r","r","r","r");
    ec.xtable(object@rCCTable,  digits=digits, align=align, caption=object@caption);
  }
)

# -----------------------------------------------------------------------------
# function: rCCTable (call real constructor)
# Return: an object of type rCCTable
# -----------------------------------------------------------------------------
rCCTable <- function(df, x, exposure=list(), exact=TRUE, rr=FALSE, ar=FALSE, pvalue=TRUE, caption="rCCTable" )
{
  return(new("rCCTable", df=df, x=x, exposure=exposure, exact=exact, rr=rr, ar=ar, pvalue=pvalue, caption=caption));
}
