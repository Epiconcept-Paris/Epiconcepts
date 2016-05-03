library(methods)

setClass("rCC",
         # ==== Inheritance
         # ==== Properties
         representation (
           varname        = "character",
           detail         = "logical",
           by             = "character",
           to             = "character",
           rCC             = "data.frame",
           ExposeField   = "character",
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
           Pvalue         = "vector"
         )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "rCC",
  function(.Object, x, exposure="", exact, title)
  {
    
    if (length(exposure) < 1) {
      stop("Exposure is empty.");
    }
            
    PLabel = ifelse(exact == T, "p-value (Fisher)", "p-value (chi2)")
            
    .Col1Label = ifelse(title=="", "rCC", title);
    .Object@Colnames = c( .Col1Label, "Exposed", "Unexposed", "Total", "Proportion Exp.");
    .Col1Values = c("Cases", "Controls", "Total", "", "", "Odds ratio",
                    "Attr. frac. ex.", "Attr. frac. pop", "chi2(1)", "Pr>chi2")

    computeKHI2 <- function(A, B, C, D)
    {
      t <- chisq.test(matrix(c(A,B,C,D),nc=2), correct=FALSE);
      return(c(t$statistic, t$p.value));
    }

    FR = table(x, exposure);
    I1E1 = FR[2,2]
    I1E0 = FR[2,1]
    I0E0 = FR[1,1]
    I0E1 = FR[1,2]

    CHI2 = computeKHI2(FR[1,1], FR[2,1], FR[1,2], FR[2,2]);
    TE = FR[1,2]+FR[2,2];
    TU = FR[1,1]+FR[2,1];
    TCA = I1E1 + I1E0;
    TNC = I0E1 + I0E0;

    ##########   Proportions  ##########
    PCAEX = sprintf("%3.5f", I1E1/TCA);
    PCTEX = sprintf("%3.5f", I0E1/TNC);
    PTOEX = sprintf("%3.5f", TE/(TCA+TNC));

    # Risk
    VAL_RE = I1E1/TE;
    VAL_RU = I1E0/TU;
    VAL_RT = TCA/(TE+TU);
    RE = sprintf("%3.5f", VAL_RE);
    RU = sprintf("%3.5f", VAL_RU);
    RTOT = sprintf("%3.5f", VAL_RT);

    ########## ESTIMATE ##########
    R = or(FR);
    OREST = sprintf("%3.5f", R[1]);
    ORCIL = sprintf("%3.5f", R[2]);
    ORCIH = sprintf("%3.5f", R[3]);
  
    if (R[1] > 1.0) {
      R = rCC_STATS(FR);
      AFEST = sprintf("%3.5f", R$AFest[1]);
      AFCIL = sprintf("%3.5f", R$AFest[2]);
      AFCIH = sprintf("%3.5f", R$AFest[3]);
   
      PAEST = sprintf("%3.5f", R$AFp[1]);
    }
    else {
      AFEST = sprintf("%3.5f", 1 - R[1]);
      AFCIL = sprintf("%3.5f", 1 - R[3]);
              AFCIH = sprintf("%3.5f", 1 - R[2]);
              
              Pe = TE / (TE + TU);
              PAEST = sprintf("%3.5f", Pe * (1 - R[1]));
              .Col1Values = c("Cases", "Controls", "Total", "", "", "Odds ratio",
                              "Prev. frac. ex.", "Prev. frac. pop", "chi2(1)", "Pr>chi2")
            }

    STR_CHI2 = sprintf("%3.6f", CHI2[1]);
    STR_PVAL = sprintf("%3.6f", CHI2[2]);
            
    COL2 = as.character(c(I1E1, I0E1, TE, "", "Point estimate", OREST, AFEST, PAEST, STR_CHI2, STR_PVAL));
    COL3 = as.character(c(I1E0, I0E0, TU, "",  "95%CILow", ORCIL, AFCIL, "", "", ""));
    COL4 = as.character(c(TCA, TNC, TE+TU, "", "95%CIHight", ORCIH, AFCIH, "", "", ""));
    COL5 = as.character(c(PCAEX, PCTEX, PTOEX, "", "", "", "", "", "", ""));
            
    DF <- data.frame(cbind(.Col1Values));
    DF = cbind(DF, COL2, COL3, COL4, COL5);
    names(DF) <- .Object@Colnames;
            
    .Object@rCC <- DF;
    .Object;
  }
);

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"rCC" ,
  function(object){
    align  =  c("l","r","r","r","r","r");
    ec.xtable(object@rCC, align=align);
  }
)

# -----------------------------------------------------------------------------
# function: rCC (call real constructor)
# Return: an object of type rCC
# -----------------------------------------------------------------------------
rCC <- function(x, exposure="", exact=FALSE, title="")
{
  return(new("rCC", x=x, exposure=exposure, exact=exact, title=title));
}
