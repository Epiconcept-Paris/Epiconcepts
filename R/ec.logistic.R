ec.logistic <- function(M, Yname="Y") {
  computeODDS <- function(M) {
    R <- exp ( coef ( M ))
    df = as.data.frame(R)
    df
#     C_ODDSNAMES <-rownames(df)
#     C_ODDSVALUES = df$R
#     c(c(C_ODDSNAMES), c(C_ODDSVALUES))
  }
  
  LogLikelihood <- function(M) {
    R <- logLik(M)
    sprintf("%5.4f", R[1])
  }
  
  getStats <- function(M) {
    df1 = as.data.frame(M$stats)
    V_OBS = sprintf("%d", df1["Obs",])
    LR_K2 = sprintf("%3.2f", df1["Model L.R.",])
    PR_K2 = sprintf("%3.4f", df1["P",])
    V_R2  = sprintf("%3.4f", df1["R2",])
    c(V_OBS, LR_K2, PR_K2, V_R2)
  }
  
  getCI <- function(M) {
    G <- glm(M, family = "binomial")
    as.data.frame(exp(confint(G)))
  }
  
  getCoefs <- function(M) {
    G <- glm(M, family = "binomial")
    S <- summary(G)
    S$coefficients
  }
  
  STLABELS = c("Number of obs", "LR chi2", "Prob > chi2", "R2", "Log likelihood")
  ODDS <- computeODDS(M)
  COEFS <- getCoefs(M)
  
  LL = LogLikelihood(M)
  ST = getStats(M)
  CS1 = c(ST, LL)
  E = c("","","","","")
  df1 =data.frame(cbind(STLABELS, CS1), stringsAsFactors = F)
  colnames(df1) <- c("Régression Logistique","")
  ec.xtable(df1)

  df <- getCI(M)
  X1 <- rownames(df)
  X2 <- round(ODDS$R, 6)
  X6 <- round(df[,1], 4)
  X7 <- round(df[,2], 4)
  X3 <- round((X7 - X6) / 3.92, 4)
  X4 <- sprintf("%5.3f",COEFS[,3])
  X5 <- sprintf("%5.4f",COEFS[,4])
  df2 =data.frame(cbind(X1,X2,X3,X4,X5,X6, X7), stringsAsFactors = F)
  colnames(df2) <- c(Yname,"Odds Ratio", "Std. Err.", "z", "P>|z|", "95% CI-L", "95% CI-H")
  ec.xtable(df2)
}