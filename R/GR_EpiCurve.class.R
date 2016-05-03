library(methods)

setClass("ec.epiCurve",
  # ==== Inheritance
  contain = "EpiPlot",
  # ==== Properties
  representation (
    vardate      = "character",
    vartype      = "character",
    varcut       = "character",
    EPC.MAXY     = "numeric"
  )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ec.epiCurve",
  function(.Object, vardate, type, cut="day") {
    .Object@vardate = vardate;
    .Object@vartype = type;
    .Object@varcut = cut;

    # --------------------------------------------------------------------------
    computeXScale <- function(O_, DR)
    {
      period = O_@varcut;
      if (O_@vartype == "date") {
        DTFMT = "%Y-%m-%d";
        if (period == "day") {
          S <- seq(ISOdate(DR[1], DR[2], DR[3]),
                   ISOdate(DR[4], DR[5], DR[6]), by = "day")
          L <- paste(
          sprintf("%s",format(S, format="%m")),
          sprintf("%s",format(S, format="%d")),
          sep='\n')
          return(as.factor(L));
        }
      }
    }
    # --------------------------------------------------------------------------

    computeFactors <- function(O_)
    {
      period = O_@varcut;
      DTFMT = "%Y-%m-%d %H:%M:%S";
      V <- na.omit(GDS[, O_@vardate]);
      if (period == "day") {
        S <-  as.POSIXlt(as.POSIXct(V, DTFMT));
        L <- paste(
          sprintf("%s",format(S, format="%m")),
          sprintf("%s",format(S, format="%d")),
          sep="\n")
        return(as.factor(L));
      }
    }
    

    if (type == "date") {
      D <- as.POSIXct(strptime(GDS[,vardate], "%Y-%m-%d"));

      DATEMIN <- min(D, na.rm=TRUE);
      DATEMAX <- max(D, na.rm=TRUE);
      
      YEARMIN <- as.POSIXlt(DATEMIN)$year+1900;
      MONTHMIN <- as.POSIXlt(DATEMIN)$mon+1;
      DAYMIN <- as.POSIXlt(DATEMIN)$mday;
      
      YEARMAX <- as.POSIXlt(DATEMAX)$year+1900;
      MONTHMAX <- as.POSIXlt(DATEMAX)$mon+1;
      DAYMAX <- as.POSIXlt(DATEMAX)$mday;
      DATE_RANGE = c(YEARMIN, MONTHMIN, DAYMIN, YEARMAX, MONTHMAX, DAYMAX);
    }
    
    V <- computeFactors(.Object);
    DF1 <- data.frame(table(V));
    V <- computeXScale(.Object, DATE_RANGE);
    DF2 <- data.frame(V)
    .Object@WDS <- merge(DF2, DF1, by = "V", all = TRUE)
    .Object@WDS[is.na( .Object@WDS)] <- 0
    .Object@EPC.MAXY <- max(.Object@WDS$Freq);
    .Object;
    # ---------------------------------------------------------------
  }
)


# -----------------------------------------------------------------------------
# function: ec.epiCurve (call real constructor)
# Return: an object of type AgePyramide
# -----------------------------------------------------------------------------
ec.epiCurve <- function(vardate, type="date", cut="day")
{
  return(new("ec.epiCurve", vardate=vardate, type=type, cut=cut));
}

setMethod ("ec.plot" , signature="ec.epiCurve",
  function(this,
    fillcolor = "",
    title     = "",
    xlabel    = "",
    ylabel    = "",
    footer="",
    bgcolor="",
    ...)
  {
    O_ = this;
    #GBASE <- ggplot(data = this@WDS, aes(x=X, y=Freq));
    this@G_TITLE = ifelse(title != "",  sprintf("\n%s\n",title), "");
    this@G_FOOTER = ifelse(footer != "", sprintf(" \n%s\n ",footer), "");
    this@G_LABELY = ifelse(ylabel != "", ylabel, this@G_LABELY);
    this@G_LABELX = ifelse(xlabel != "", xlabel, this@G_LABELX);
    this@T_BGCOLOR = ifelse(bgcolor == "", "white", bgcolor);
    this@G_FILLCOLOR = ifelse(fillcolor == "", this@G_FILLCOLOR, fillcolor);
    
    Draw <- function(this) {
      #deviceOn("PYRamide", "svg", width=700)
      THEME =  theme(panel.background = element_rect(fill = "white", colour="white"))
      P_ <- ggplot(this@WDS, aes(x=V , y = Freq )); 
      P_ <- P_ +  geom_bar(stat='identity', fill= this@G_FILLCOLOR, width=1);
        P_ <- P_ +  geom_hline(yintercept=seq(1, this@EPC.MAXY, by=1), colour="white", size=1);
        P_ <- P_ +  geom_vline(xintercept = seq(1.5, nrow(this@WDS), by=1), colour="white", size=1);
        P_ <- P_ +  geom_vline(xintercept = 0, colour="black", size=1);
        P_ <- P_ +  geom_hline(yintercept = 0, colour="black", size=1);
        P_ <- P_ +  coord_fixed(ratio=1);
        P_ <- P_ +  ggtitle(this@G_TITLE);
        P_ <- P_ +  xlab(this@G_LABELX);
        P_ <- P_ +  ylab(this@G_LABELY);
        P_ <- P_ +  THEME
        P_
      #dev.off()
      
    }
    
    Draw(this);
  }
);

