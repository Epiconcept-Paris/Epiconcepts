library(methods)

setClass("ec.freq",
         # ==== Inheritance
         contain = "EpiPlot",
         # ==== Properties
         representation (
           vardate      = "character",
           vartype      = "character",
           varcut       = "character",
           by           = "character",
           df           = "data.frame",
           dft          = "data.frame",
           percent      = "numeric",
           EPC.MAXY     = "numeric"
         )           
);

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ec.freq",
  function(.Object, x, by=NULL, where=NULL, caption) {
    .Object@by <- by;
    if (by != "") {
      if (is.vector(where)) {
        R = table(GDS[where, x], GDS[where, by], dnn=c(by));
      } else {
        R = table(VAL(x), VAL(by), dnn=c(by));
      }
      
      .Object@percent <- p.percent(R)
      
      df <- as.data.frame.matrix(R);
      cn <- colnames(df);
      df <- cbind(rownames(df), df)
      rownames(df) <- NULL;
      colnames(df) <- c(x, cn);
      .Object@dft <- df;
      df <- data.frame(R);
      df <- cbind(df, .Object@percent)
      names(df) <- c(x, by, "Freq","%");
      .Object@df <- df;
      return(.Object);
    }
    
    # Cas une seule variable et une clause where
    # ------------------------------------------------
    if (is.vector(where)) {
      Tab <- table(GDS[where, x]);
      df = data.frame(Tab);
      .Object@percent <- p.percent(Tab)
      df <- cbind(df, .Object@percent)
      names(df) <- c(x, "Freq", "%");
      .Object@df <- df;
      return(.Object);
    }
    
    # Cas une seule variable
    # ------------------------------------------------
    Tab <- table(GDS[, x]);
    df  <- data.frame(Tab);
    .Object@percent <- p.percent(Tab)
    df <- cbind(df, .Object@percent)
    names(df) <- c(x, "Freq", "%");
    .Object@df <- df;
    return(.Object);
  }
);
# =============================================================================

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" , signature="ec.freq" ,
  function(object) {
    if (object@by == "") {
      ec.xtable(object@df)
    } else {
      ec.xtable(object@df)
    }
  }
);

setMethod ("ec.plot" , signature="ec.freq",
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
# function: ec.freq (call real constructor)
# Return: an object of type AgePyramide
# -----------------------------------------------------------------------------
ec.freq <- function(x, by="", where=NULL, caption)
{
  return(new("ec.freq", x=x, by=by, where=where, caption=caption));
}


         