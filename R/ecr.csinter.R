library(methods)
library(Hmisc)
setClass("ecr.csinter",
         # ==== Inheritance
         # ==== Properties
         representation (
           varname        = "character",
           output        = "data.frame"
         )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ecr.csinter",
          function(.Object, df, x, exposure, by)
          {
#            .Object <-  .Object;
            
            if (length(exposure) < 1) {
              stop("Exposure is empty.");
            }

            GDS <- df
            
            .strate = GDS[,by]
            if(!is.factor(GDS[,by])) {
              .strate <- as.factor(GDS[,by])
            }

            L_LABELS1   <- vector()
            L_CASES     <- vector()
            L_TOTAL     <- vector()
            L_RISK      <- vector()
            L_ESTIMATE  = vector()
            L_STATS     = vector()
            L_CIL       = vector()
            L_CIH       = vector()
            L_TAB       = vector()
            NB_TOTAL    = 0
            NB_LEVELS   = 0
            
            # Return labels of columns of the output data.frame
            # -----------------------------------------------------------------
            getColnames <- function() {
              .Col1Label = sprintf("ecr.csinter %s / %s", x, exposure);
              c(.Col1Label, "Total", "Cases", "Risk %", "P.est.","Statistics", "95%CI-L", "95%CI-H");
            }
            
            getPestNames <- function() {
              c("Risk difference", "Risk Ratio", "Attrib.risk.exp", "Attrib.risk.pop")
            }
            
            # Returns labels for each level of 'by'
            getRisksLabels <- function(.level) {
              .label = sprintf("%s = %s", by, .level);
              c(.label, "Exposed", "Unexposed", "")
            }

            getMHLabels <- function() {
              label2 = sprintf("Crude RR for %s", exposure);
              label3 = sprintf("MH RR %s adjusted for %s", exposure, by);  
              c("Missing", "MH test of Homogeneity Chi2 / pvalue",
                              label2, label3, "Adjusted/crude relative change")
            }
            
            F2 <- function(N) {
              sn <- sprintf("%3.2f", N)
              sn
            }
            
            # Loop on all levels of 'by' (strates)
            # -----------------------------------------------------------------
            getRRStats <- function() {
              
              .loop = length(levels(.strate))
              NB_LEVELS = .loop
              for (i in 1:.loop) {
                .level <- levels(.strate)[i]
                .T = table(GDS[GDS[,by]==.level, exposure], GDS[GDS[,by]==.level, x])
                L_TAB <- c(L_TAB, GetStrateVector(.T))
                L_LABELS1 <- c(L_LABELS1, getRisksLabels(.level))
                TE = .T[2,1]+.T[2,2];
                TU = .T[1,1]+.T[1,2];
                CE = .T[2,2];
                CU = .T[1,2];
                TO = TE + TU;

                NB_TOTAL = NB_TOTAL + TO
                L_CASES <- c(L_CASES, "", CE, CU, "")
                L_TOTAL <- c(L_TOTAL, TO, TE, TU, "")

                # Risk %
                # -------------------------------------------------------------
                RE = CE / TE
                RU = CU / TU
                L_RISK <- c(L_RISK, "", F2(RE * 100), F2(RU * 100), "")
                
                # P.est.
                # -------------------------------------------------------------
                L_ESTIMATE <- c(L_ESTIMATE, getPestNames())

                # Statistics - 95%CI-L - 95%CI-H
                # -------------------------------------------------------------
                # RDF : Risk difference ---------------------------------------
                RDF = RE - RU
                CI <- computeDiffRiskCI(RE, RU, TE, TU)
                RDFCIL = CI[1]
                RDFCIH = CI[2]
                
                # RR : Risk Ratio ---------------------------------------------
                .R <- rr(.T);
                RR    = .R[1];
                RRCIL = .R[2];
                RRCIH = .R[3];

                if (RDF > 0) {
                  # ARE : Attrib.risk.exp -------------------------------------
                  AFE = RDF / RE;
                  AFECIL = (RRCIL - 1) / RRCIL
                  AFECIH = (RRCIH - 1) / RRCIH
                              
                  # AFP -------------------------------------------------------
                  .RT = (CE + CU)/TO
                  AFP = (.RT-RU)/.RT
                } else {
                  # Prev.frac.exp. --------------------------------------------
                  AFE = 1 - RR;
                  AFECIL = 1 - RRCIH
                  AFECIH = 1 - RRCIL
                  
                  # Prev.frac.pop ---------------------------------------------
                  Pe = TE / (TE + TU);
                  AFP = Pe * (1 - RR);
                }
                
                L_STATS <- c(L_STATS, F2(RDF), F2(RR), F2(AFE), F2(AFP))
                L_CIL <- c(L_CIL, F2(RDFCIL), F2(RRCIL), F2(AFECIL), "")
                L_CIH <- c(L_CIH, F2(RDFCIH), F2(RRCIH), F2(AFECIH), "")
              }

              # MISSING -------------------------------------------------------
              N_ROWS = nrow(GDS)
              MIS_TO = N_ROWS - NB_TOTAL
              MIS_PC = (MIS_TO / N_ROWS)*100
              L_TOTAL <- c(L_TOTAL, MIS_TO)
              L_CASES <- c(L_CASES, sprintf("%3.2f%%",MIS_PC))
              
              # MH test -------------------------------------------------------
              .T <- table(GDS[,x], GDS[,exposure], GDS[,by]);
              R = MH_HomogeneityTest(.T);
              CHI2 = R[1]
              PVAL = R[2]
              L_TOTAL <- c(L_TOTAL, sprintf("%3.3f",CHI2))
              L_CASES <- c(L_CASES, sprintf("%3.3f",PVAL))
              
              # Crude RR ------------------------------------------------------
              .T <- table(GDS[,x], GDS[,exposure])
              R <- rr(.T)
              #R <- CS_STATS(.T);
              CRRR  = R[1]
              CRCIL = R[2]
              CRCIH = R[3]
              L_STATS <- c(L_STATS, "", "", F2(CRRR))
              L_CIL   <- c(L_CIL,   "", "", F2(CRCIL))
              L_CIH   <- c(L_CIH,   "", "", F2(CRCIH))

              # MH RR ---------------------------------------------------------
              M <- matrix(L_TAB, NB_LEVELS, byrow = TRUE)
              R <- MANTEL_RR(M)
              MHRRSTAT = R[1]
              MHRRCIL  = R[2]
              MHRRCIH  = R[3]
              
              L_STATS <- c(L_STATS, F2(MHRRSTAT))
              L_CIL   <- c(L_CIL, F2(MHRRCIL))
              L_CIH   <- c(L_CIH, F2(MHRRCIH))

              # Adjusted/crude relative change
              # ------------------------------------------------------------
              RC = 100 * ((MHRRSTAT - CRRR)/CRRR)
              STAT = sprintf("%3.2f%%", RC);
              L_STATS <- c(L_STATS, STAT);
              
              COL2 = c(L_TOTAL, "","","")
              COL3 = c(L_CASES, "","","")
              COL4 = c(L_RISK,"","","","","")
              COL5 = c(L_ESTIMATE,"","","","","")
              COL6 = c(L_STATS)
              COL7 = c(L_CIL, "")
              COL8 = c(L_CIH, "")
              C1Labels <- c(L_LABELS1, getMHLabels())
              DF <- data.frame(cbind(C1Labels))
              DF = cbind(DF, COL2, COL3, COL4, COL5, COL6, COL7, COL8)
              names(DF) <- getColnames()
              DF
            }
            
            .Object@output <- getRRStats()
            .Object
          });

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"ecr.csinter" ,
          function(object) {
            align  =  c("l","r","r","r","r","r","r","r","r");
            ec.xtable(object@output, align=align);
          }
)

# -----------------------------------------------------------------------------
# function: ecr.csinter (call real constructor)
# Return: an object of type ecr.csinter
# -----------------------------------------------------------------------------
ecr.csinter <- function(df, x, exposure="", by="")
{
  return(new("ecr.csinter", df=df, x=x, exposure=exposure, by=by));
}
