library(methods)

setClass("ecr.ccinter",
         # ==== Inheritance
         # ==== Properties
         representation (
           varname        = "character",
           output         = "data.frame"
         )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ecr.ccinter",
          function(.Object, df, x, exposure, by)
          {
            
            GDS <- df
            
            .strate = GDS[,by]
            if(!is.factor(GDS[,by])) {
              .strate <- as.factor(GDS[,by])
            }
            
            L_LABELS1  <- vector();
            LABS_ESTIM <- vector()
            L_CASES     = vector();
            L_CONTROLS  = vector();
            L_ESTIMATE  = vector();
            L_STATS     = vector();
            L_CIL       = vector();
            L_CIH       = vector();
            NB_TOTAL    = 0;
            
            # Return labels of columns of the output data.frame
            # -----------------------------------------------------------------
            getColnames <- function() {
              C1Label = sprintf("CCInter %s / %s", x, exposure);
              return (c(C1Label,"Cases","Controls","P.est.","Statistics","95%CI-L","95%CI-H"))
            }
            
            getMHLabels <- function() {
              label2 = sprintf("Crude OR for %s", exposure);
              label3 = sprintf("MH OR %s adjusted for %s", exposure, by);  
              .Col1Values = c("Number of obs", "Missing", "MH test of Homogeneity pvalue",
                              label2, label3, "Adjusted/crude relative change %")
            }
            
            # Returns labels for each level of 'by'
            getOddsLabels <- function(.level) {
              .label = sprintf("%s = %s", by, .level);
              return(c(.label, "Exposed", "Unexposed", "Total", "Exposed %", ""))
            }
            
            getLabEstim <- function() {
              c("Odds Ratio", "Attrib.risk.exp", "Attrib.risk.pop", "", "","");
            }
            
            # Loop on all levels of 'by' (strates)
            # -----------------------------------------------------------------
            getOddsStats <- function() {
              
              .loop = length(levels(.strate))
              for (i in 1:.loop) {
                .level <- levels(.strate)[i]
                .T = table(GDS[GDS[,by]==.level, exposure], GDS[GDS[,by]==.level, x])

                L_LABELS1 <- c(L_LABELS1, getOddsLabels(.level))
                LABS_ESTIM <- c(LABS_ESTIM, getLabEstim())
                
                L_CASES <- c(L_CASES, "", .T[2,2], .T[1,2]);
                TOTAL <-  .T[2,2] + .T[1,2];
                NB_TOTAL = NB_TOTAL + TOTAL;
                EXPOSED_PC <- sprintf("%3.0f", (.T[2,2] / TOTAL) * 100);
                L_CASES <- c(L_CASES, TOTAL, EXPOSED_PC,"");

                # CONTROLS
                # ------------------------------------------------------------
                L_CONTROLS <- c(L_CONTROLS, "", .T[2,1], .T[1,1]);
                TOTAL <-  .T[2,1] + .T[1,1];
                NB_TOTAL = NB_TOTAL + TOTAL;
                EXPOSED_PC <- sprintf("%3.0f", (.T[2,1] / TOTAL) * 100);
                L_CONTROLS <- c(L_CONTROLS, TOTAL, EXPOSED_PC,"");
                
                # ODDS RATIO
                # ------------------------------------------------------------
                R = or(.T); 
                V_OR  = R[1]; # ODDS ratio
                V_CIL = R[2]; # Confidence interval low
                V_CIH = R[3]; # Confidence interval hight
                OR = sprintf("%3.2f", V_OR);
                CIL = sprintf("%3.2f", V_CIL);
                CIH = sprintf("%3.2f", V_CIH);
                L_STATS <- c(L_STATS, OR);
                L_CIL = c(L_CIL, CIL);
                L_CIH = c(L_CIH, CIH);
                
                R <- CC_AR(.T);
                V_AR  = R[1]; # Attrib.risk.exp
                V_CIL = R[2]; # Confidence interval low
                V_CIH = R[3]; # Confidence interval hight
                AR = sprintf("%3.2f", V_AR);
                CIL = sprintf("%3.2f", V_CIL);
                CIH = sprintf("%3.2f", V_CIH);
                L_STATS <- c(L_STATS, AR);
                L_CIL = c(L_CIL, CIL, "", "", "", "");
                L_CIH = c(L_CIH, CIH, "", "", "", "");
                
                R <- CC_PAR(.T);
                AFP = sprintf("%3.2f", R);
                L_STATS <- c(L_STATS, AFP, "", "", "");

              }
              
              .T <- table(GDS[,x], GDS[,exposure], GDS[,by]);
              R <- CC_STATS(.T);

              # Number of obs
              # ------------------------------------------------------------
              L_CASES = c(L_CASES, NB_TOTAL);
              
              # MISSING
              # ------------------------------------------------------------
              MIS_TO = nrow(GDS) - NB_TOTAL;
              MIS_PC = sprintf("%3.2f%s", (MIS_TO / nrow(GDS))*100, '%');
              L_CASES = c(L_CASES, MIS_TO);
              
              # MH test of Homogeneity pvalue
              # ------------------------------------------------------------
              STAT = sprintf("%3.3f", R$OR.homog[3]);
              L_STATS <- c(L_STATS, "", "", STAT);
              #print(sprintf("MH test of Homogeneity pvalue : %s", STAT));
              
              # Crude OR for exposure
              # ------------------------------------------------------------
              STAT = sprintf("%3.2f", R$OR.crude[1]);
              CIL = sprintf("%3.2f", R$OR.crude[3]);
              CIH = sprintf("%3.2f", R$OR.crude[4]);
              L_STATS <- c(L_STATS, STAT);
              L_CIL = c(L_CIL, "", "", "", CIL);
              L_CIH = c(L_CIH, "", "", "", CIH);
              
              # MH OR for exposure adjusted for by
              # ------------------------------------------------------------
              STAT = sprintf("%3.2f", R$OR.mh[1]);
              CIL = sprintf("%3.2f", R$OR.mh[3]);
              CIH = sprintf("%3.2f", R$OR.mh[4]);
              L_STATS <- c(L_STATS, STAT);
              L_CIL = c(L_CIL, CIL);
              L_CIH = c(L_CIH, CIH);
              
              # Adjusted/crude relative change
              # ------------------------------------------------------------
              RC = 100 * ((R$OR.mh[1]-R$OR.crude[1])/R$OR.crude[1]);
              STAT = sprintf("%3.2f", RC);
              L_STATS <- c(L_STATS, STAT);
              
              
              COL2 = c(L_CASES, "", "", "", "");
              COL3 = c(L_CONTROLS, "", "", "", "", "", "");
              COL4 = c(LABS_ESTIM, "", "", "", "", "", "")
              COL5 = c(L_STATS);
              COL6 = c(L_CIL, "");
              COL7 = c(L_CIH, "");
              
              C1Labels <- c(L_LABELS1, getMHLabels())
              DF <- data.frame(cbind(C1Labels));
              DF = cbind(DF, COL2, COL3, COL4, COL5, COL6, COL7)
              names(DF) <- getColnames()
              row.names(DF) <- NULL
              DF
            }
            
            .Object@output <- getOddsStats()
            .Object;
          });

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"ecr.ccinter" ,
          function(object){
            align  =  c("l","r","c","c","r","r","r","r");
            ec.xtable(object@output, align=align);
          }
)

# -----------------------------------------------------------------------------
# function: ecr.ccinter (call real constructor)
# Return: an object of type ecr.ccinter
# -----------------------------------------------------------------------------
ecr.ccinter <- function(df, x, exposure="", by="")
{
  return(new("ecr.ccinter", df=df, x=x, exposure=exposure, by=by));
}
