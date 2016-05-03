# return labels or indices of table names
# -------------------------------------------------------------------
p.getTableNames <- function(xfreq, nolabel)
{
  if (nolabel == FALSE) {
    return(names(xfreq));
  }
  return(seq(0, length(names(xfreq))-1, by=1));
}

# return stats for tablate functions ec/ecr.tabulate
# -------------------------------------------------------------------
p.frequencies <- function(tf, nbObs, nolabel, namevar)
{
  .freq = tf;
  .rel.freq <- as.numeric(sprintf("%5.3f", (.freq / nbObs) * 100));
  .cum.rel.freq <- as.numeric(sprintf("%5.3f", cumsum(.rel.freq)));
  L0 <- c(namevar, "Freq.", "Percent", "Cumul.");
  C1 <- c(p.getTableNames(.freq, nolabel), "Total");
  C2 <- as.vector(.freq);
  C2 <- c(C2, sum(C2));
  C3 <- as.vector(.rel.freq);
  C3 <- c(C3, sum(C3));
  C4 <- as.vector(.cum.rel.freq);
  C4 <- c(C4, NA);
  DF = data.frame(cbind(C1, C2, C3, C4));
  names(DF) <- L0;
  DF;
}


# return quantiles of a numeric variable
# -------------------------------------------------------------------
p.Quantile <- function(x)
{
  C = class(x);
  if (C == "Date" | C == "Factor") {
    return(c("", "", "", ""));
  }
  
  Q <- quantile(x, na.rm=TRUE);
  return(as.vector(Q));
}

p.kurtosis <- function(x)
{
  if (class(x) == "Date") {
    return(NA);
  }
  
  kurtosis(x, method="moment", na.rm=TRUE);
}

p.skewness <- function(x)
{
  if (class(x) == "Date") {
    return("");
  }
  skewness(x, na.rm=TRUE, method="moment");
}


# return a simple summary of a variable within a data.frame
# -------------------------------------------------------------------
p.simpleSummary <- function(x, namevar)
{
  L0 = c("Variable", "Obs", "Mean", "Std. Dev.", "Min", "Max");
  .MIS = sum(is.na(x));
  .OBS = c(length(x) - .MIS);
  .MIN = c(min(x, na.rm=TRUE));
  .MAX = c(max(x, na.rm=TRUE));
  .MEA = c(mean(x, na.rm=TRUE));
  .STD = c(sd(x, na.rm=TRUE));
  L1 = c(namevar, .OBS, .MEA, .STD, .MIN, .MAX);
  DF <- data.frame(cbind(c(namevar)));
  DF <- cbind(DF, .OBS, .MEA, .STD, .MIN, .MAX);
  names(DF) <- L0;
  return(DF)
}

p.detailSummary <- function(x, namevar)
{
  L0 = c("Summary", namevar);
  L1 = c("Observations", "Missing", "Mean");
  L2 = c("Min.", "Q0.25", "Median", "Q0.75", "Max.");
  L3 = c("S.D", "Variance", "Skewless", "Kurtosis")
  Q = p.Quantile(x);
  .MIS = sum(is.na(x));
  .OBS = length(x) - .MIS;
  .MIN = min(x, na.rm=TRUE);
  .MAX = max(x, na.rm=TRUE);
  .MEA = mean(x, na.rm=TRUE);
  .MED = median(x, na.rm=TRUE);
  .Q25 = Q[2];
  .Q75 = Q[4];
  .STD = sd(x, na.rm=TRUE);
  .VAR = var(x, na.rm=TRUE);
  .SKW = p.skewness(x);
  .KUR = p.kurtosis(x);
  
  C1 = c(L1, L2, L3);
  C2 = c(.OBS, .MIS, .MEA, .MIN, .Q25, .MED, .Q75, .MAX, .STD, .VAR, .SKW, .KUR);
  print(C2)
  DF = data.frame(cbind(C1,C2));
  str(DF)
  names(DF) <- L0;
  return(DF);
}

p.factorSummary <- function(x, namevar)
{
  L0 = c("Factor", "Factor min.", "Factor max.");
  .min = min(levels(x));
  .max = max(levels(x));
  DF = df = as.data.frame(rbind(c(namevar, .min, .max)));
  names(DF) <- L0;
  return(DF);
}

p.proportion <- function(x, label)
{
  set.seed(123)

  myprop<-function(x,i){
    sum(x[i]==0)/length(x)
  }

  lab <- label
  N_ = length(x)
  
  B <- boot(x, myprop, 100);
  SD_ = sprintf("%5.6f", sd(B$t));
  T = table(x);
  NAMES_ = names(T)
  M1_ = T[1];
  M2_ = T[2];
  NDF = c(lab, "Obs.","Proportion", "Std. Err.", "95% CI.Low", "95% CI.Low");
  
  P = prop.test(M1_, N_, conf.level=0.95);
  PE1_ = P$estimate;
  CI1_LOW = P$conf.int[1];
  CI1_HIG = P$conf.int[2];
  
  P = prop.test(M2_, N_, conf.level=0.95);
  PE2_ = P$estimate;
  CI2_LOW = P$conf.int[1];
  CI2_HIG = P$conf.int[2];
  
  R1 = c(NAMES_[1], M1_, PE1_, SD_, CI1_LOW, CI1_HIG);
  R2 = c(NAMES_[2], M2_, PE2_, SD_, CI2_LOW, CI2_HIG);
  
  #  DF = data.frame(rbind("1"=R1, "2"=R2));
  DF = data.frame(cbind(NAMES_));
  DF = cbind(DF, as.numeric(c(M1_,M2_)));
  DF = cbind(DF, as.numeric(c(PE1_,PE2_)));
  DF = cbind(DF, as.numeric(c(SD_,SD_)));
  DF = cbind(DF, as.numeric(c(CI1_LOW,CI2_LOW)));
  DF = cbind(DF, as.numeric(c(CI1_HIG,CI2_HIG)));
  names(DF) <- NDF;
  DF
#   digits =  c(0,0,0,4,4,5,5);
#   align  =  c("l","c","c","r","r","r","r");
#   ec.xtable(DF, digits=digits, align=align)  
}

p.percent <- function(R) {
  P = prop.table(R) * 100
  d <- as.data.frame(P);
  d$Freq
}

