library(methods)

setClass("ecr.freq",
         # ==== Inheritance
         contain = "EpiPlot",
         # ==== Properties
         representation (
           vardate      = "character",
           vartype      = "character",
           varcut       = "character",
           by           = "logical",
           df           = "data.frame",
           dft          = "data.frame",
           percent      = "numeric",
           EPC.MAXY     = "numeric"
         )           
);

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ecr.freq",
  function(.Object, x, by=NULL, ...) {
    
    percent <- function(R) {
      P = prop.table(R) * 100
      d <- as.data.frame(P);
      d$Freq
    }
    
    if (!is.null(by)) {
    .Object@by <- T;
      #if (by != "") {
      R = table(x, by);
      .Object@percent <- p.percent(R)
      df <- as.data.frame.matrix(R);
      cn <- colnames(df);
      df <- cbind(rownames(df), df)
      rownames(df) <- NULL;
      #colnames(df) <- c(x, cn);
      .Object@dft <- df;
      df <- data.frame(R);
      df <- cbind(df, .Object@percent);
      names(df) <- c("x", "by", "Freq", "%");
      .Object@df <- df;
      return(.Object);
    }

    # Cas une seule variable
    # ------------------------------------------------
    Tab <- table(x);
    df  <- data.frame(Tab);
    .Object@percent <- p.percent(Tab)
    df <- cbind(df, .Object@percent)
    names(df) <- c("x", "Freq", "%");
    .Object@df <- df;
    return(.Object);
    
#     R = data.frame(table(x));
#     names(R) <- c("x", "Freq");
#     .Object@df <- R;
#     return(.Object);
  }
);
# =============================================================================

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" , signature="ecr.freq" ,
  function(object) {
    if (!is.null(object@by)) {
      ec.xtable(object@df)
    } else {
      ec.xtable(object@dft)
    }
  }
);

setMethod ("ec.plot" , signature="ecr.freq",
           function(this,
                    title    ="",
                    fillcolor="",
                    ylabel   ="",
                    xlabel   ="",
                    bgcolor  =""
           ) {
             DF <- this@df;
             varname = names(DF)[1];
             varpart = ifelse(ncol(DF) == 3, names(DF)[2], "") ;
             G_TITLE = ifelse(title != "", sprintf("%s\n",title), "");
             G_LABELY = ifelse(ylabel != "", ylabel, "Frequency");
             G_LABELX = ifelse(xlabel != "", xlabel, varname);
             G_FILLCOLOR = ifelse(fillcolor != "", fillcolor, "lightblue");
             T_BGCOLOR = ifelse(bgcolor == "", "white", bgcolor);
             THEME <- theme(panel.background = element_rect(fill = T_BGCOLOR)) +
               theme(axis.ticks.margin=unit(c(0.25,0.25),'line')) +
               theme(plot.margin = unit(c(0.5,0.5,0,0), "cm")) +
               theme(panel.margin = unit(c(0,0,0,0), "mm"))
             
             # -------------------------------------------------------------------------
             # Effective drawing
             # -------------------------------------------------------------------------    
             Draw <- function(O) {
               P_ <- ggplot(DF, aes_string(x = varname, y="Freq", ymax=50))
               P_ <- P_ + geom_bar(stat="identity", colour="black", fill=G_FILLCOLOR, na.action=na.exclude);
               if (G_TITLE != "_AUTO_") P_ <- P_ + ggtitle(G_TITLE);
               P_ <- P_ + xlab(G_LABELX);
               P_ <- P_ + ylab(G_LABELY);
               P_ <- P_ + scale_y_continuous(expand = c(0,0))
               P_ <- P_ + THEME;
               if (varpart != "") {
                 P_ <- P_ + facet_wrap(eval(parse(text = paste('~', varpart, sep=''))), ncol=2);
               }
               plot(P_);
             }
             Draw(this);
           });

# -----------------------------------------------------------------------------
# function: ecr.freq (call real constructor)
# Return: an object of type AgePyramide
# -----------------------------------------------------------------------------
ecr.freq <- function(x, by=NULL, caption)
{
  return(new("ecr.freq", x=x, by=by, caption=caption));
}


         