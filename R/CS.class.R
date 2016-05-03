library(methods)

setClass("CS",
         # ==== Inheritance
         # ==== Properties
         representation (
           varname        = "character",
           detail         = "logical",
           by             = "character",
           to             = "character",
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
           Pvalue         = "vector",
           CS             = "data.frame"
         )           
)


# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "CS",
          function(.Object, x, exposure="", exact, where, title)
          {
            .Object <-  .Object;
            
            if (length(exposure) < 1) {
              stop("Exposure list is empty.");
            }
            
            PLabel = ifelse(exact == T, "p-value (Fisher)", "p-value (chi2)")
            
            .Object@ExposeField = exposure;
            
            .Col1Label = ifelse(title=="", sprintf("CS %s %s", x, exposure), title);
            
            .Object@Colnames = c( .Col1Label, "Exposed", "Unexposed", "Total", "Point estimate", "95%CILow", "95%CIHight");
            
            .Col1Values = c("Cases", "Non Cases", "Total", "Risk", "Risk difference", "Risk ratio",
                            "Attr. frac. ex.", "Attr. frac. pop", "chi2(1)", "Pr>chi2")

            computeKHI2 <- function(A, B, C, D)
            {
              t <- chisq.test(matrix(c(A,B,C,D),nc=2), correct=FALSE);
              return(c(t$statistic, t$p.value));
            }
#             
#             computeFisherPvalue <- function(A, B, C, D)
#             {
#               t <- fisher.test(matrix(c(A,B,C,D),nc=2));
#               return(sprintf("%3.3f", t$p.value));
#             }
#             
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
            
            computeDiffRiskCI <- function(RE, RU, NE, NU)
            {
              A = RE - RU;
              B = (RE * (1-RE))/NE;
              C = (RU * (1-RU))/NU;
              D = 1.96*sqrt(B + C);
              R1 = A + D;
              R2 = A - D;

              return(c(R2, R1));
            }
            
            if (length(where) > 0) {
              FR = table(GDS[where, x], GDS[where, exposure])
            } else {
              FR = table(VAL(x), VAL(exposure));
            }
              I1E1 = FR[2,2]
              I1E0 = FR[2,1]
              I0E0 = FR[1,1]
              I0E1 = FR[1,2]
            CHI2 = computeKHI2(FR[1,1], FR[2,1], FR[1,2], FR[2,2]);
              TE = FR[1,2]+FR[2,2];
              TU = FR[1,1]+FR[2,1];
              TCA = I1E1 + I1E0;
              TNC = I0E1 + I0E0;
              # Risk
              VAL_RE = I1E1/TE;
              VAL_RU = I1E0/TU;
              VAL_RT = TCA/(TE+TU);
              RE = sprintf("%3.5f", VAL_RE);
              RU = sprintf("%3.5f", VAL_RU);
            RTOT = sprintf("%3.5f", VAL_RT);

            ########## Point estimate ##########
            # ==========( Risk difference )==========
            VAL_RDIFF = VAL_RE - VAL_RU;
            CI = computeDiffRiskCI(VAL_RE, VAL_RU, TE, TU);
            RD_CILOW = sprintf("%3.6f", CI[1]);
            RD_CIHIG = sprintf("%3.6f", CI[2]);
            
            # ==========( Risk Ratio )==========
            VAL_RR = VAL_RE/VAL_RU;
            RR = sprintf("%3.6f", VAL_RR);
            CI = computeRiskCI(VAL_RR, FR[2,2], TE, FR[2,1], TU);
            VAL_RR_CILOW = CI[1];
            VAL_RR_CIHIG = CI[2];
            
            # ========== Attribuable / Preventive fraction exposed ==========
            if (VAL_RDIFF > 0) {
              VAL_AFE = VAL_RDIFF / VAL_RE;
              AFE = sprintf("%3.6f", VAL_AFE);
              VAL_AFP = (VAL_RT-VAL_RU)/VAL_RT;
              AFP = sprintf("%3.6f", VAL_AFP);
              VAL_AFE_CILOW = (VAL_RR_CILOW - 1) / VAL_RR_CILOW;
              VAL_AFE_CIHIG = (VAL_RR_CIHIG - 1) / VAL_RR_CIHIG;
            } else {
              # ==========( Prev. frac. ex. )==========
              VAL_AFE = 1 - VAL_RR;
              AFE = sprintf("%3.6f", VAL_AFE);
              VAL_AFE_CILOW = 1 - VAL_RR_CIHIG;
              VAL_AFE_CIHIG = 1 - VAL_RR_CILOW;
              # ==========( Prev. frac. pop )==========
              Pe = TE / (TE + TU);
              VAL_AFP = Pe * (1 - VAL_RR);
              AFP = sprintf("%3.6f", VAL_AFP);
              .Col1Values = c("Cases", "Non Cases", "Total", "Risk", "Risk difference", "Risk ratio",
                              "Prev. frac. ex.", "Prev. frac. pop", "chi2(1)", "Pr>chi2")
            }
            
            ########## FORMATAGE ##########
            RDIFF = sprintf("%3.6f", VAL_RDIFF);
            
            RR_CILOW = sprintf("%3.6f", VAL_RR_CILOW);
            RR_CIHIG = sprintf("%3.6f", VAL_RR_CIHIG);
            
            AFE_CILOW = sprintf("%3.6f", VAL_AFE_CILOW);
            AFE_CIHIG = sprintf("%3.6f", VAL_AFE_CIHIG);
            STR_CHI2 = sprintf("%3.6f", CHI2[1]);
            STR_PVAL = sprintf("%3.6f", CHI2[2]);
            
            COL2 = as.character(c(I1E1, I0E1, TE, RE, "", "", "", "", "", ""));
            COL3 = as.character(c(I1E0, I0E0, TU, RU,  "", "", "", "", "", ""));
            COL4 = as.character(c(TCA, TNC, TE+TU, RTOT, "", "", "", "", "", ""));
            COL5 = as.character(c("", "", "", "", RDIFF, RR, AFE, AFP, STR_CHI2, STR_PVAL));
            COL6 = as.character(c("", "", "", "", RD_CILOW, RR_CILOW, AFE_CILOW, "", "", ""));
            COL7 = as.character(c("", "", "", "", RD_CIHIG, RR_CIHIG, AFE_CIHIG, "", "", ""));
            
            DF <- data.frame(cbind(.Col1Values));
            DF = cbind(DF, COL2, COL3, COL4, COL5, COL6, COL7);
            names(DF) <- .Object@Colnames;
            
            .Object@CS <- DF;
            .Object;
          });

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"CS" ,
  function(object){
    align  =  c("l","r","r","r","r","r","r","r");
    ec.xtable(object@CS, align=align)
  }
)

# -----------------------------------------------------------------------------
# function: CS (call real constructor)
# Return: an object of type CS
# -----------------------------------------------------------------------------
CS <- function(x, exposure="", exact=FALSE, where=vector(), title="")
{
  return(new("CS", x=x, exposure=exposure, exact=exact, where=where, title=title));
}
