library(methods)

setClass("ecr.histogram",
         # ==== Inheritance
         contain = "EpiPlot",
         # ==== Properties
         representation (
           varname  = "character",
           varpart  = "character",
           discrete = "logical",
           start    = "numeric",
           binwidth = "numeric",
           df = "data.frame"
         )           
)

# ------------------------------------------------------------------------------
# Constructor
# ------------------------------------------------------------------------------
setMethod("initialize", 
          "ecr.histogram",
          function(.Object, df, x, by="", discrete=FALSE, start=Inf, binwidth=0) {
            .Object@df = df;
            .Object@varname = x;
            .Object@varpart = by;
            .Object@discrete = discrete;
            .Object@start = start;
            .Object@binwidth = binwidth;
            .Object
          })


setMethod ("ec.plot" , signature="ecr.histogram",
           function(this,
                    title    ="",
                    fillcolor="",
                    ylabel   ="",
                    xlabel   ="",
                    bgcolor  =""
           ) {
             
             this@G_TITLE = ifelse(title != "", sprintf("%s\n",title), this@G_TITLE);
             this@G_LABELY = ifelse(ylabel != "", ylabel, "Frequency");
             this@G_LABELX = ifelse(xlabel != "", xlabel, this@varname);
             this@G_FILLCOLOR = ifelse(fillcolor != "", fillcolor, this@G_FILLCOLOR);
             this@T_BGCOLOR = ifelse(bgcolor == "", "white", bgcolor);
             THEME <- theme(panel.background = element_rect(fill = this@T_BGCOLOR)) +
                      theme(axis.ticks.margin = unit(c(0.25,0.25),'line')) +
                      theme(plot.margin = unit(c(1,0.5,0,0), "cm")) +
                      theme(panel.margin = unit(c(2,2,0,0), "lines"))
             # -------------------------------------------------------------------------
             # Effective drawing
             # -------------------------------------------------------------------------    
             Draw <- function(O) {
               df <- this@df;
               P_ <- ggplot(df, aes_string(x = this@varname));
               if (is.numeric(this@varname)) {
                 if (this@discrete == TRUE) {
                   P_ <- P_ + geom_histogram(binwidth=1, colour="black", fill=this@G_FILLCOLOR,na.action=na.exclude);
                   min = ifelse (this@start != Inf, this@start, ec.min(this@varname));
                   to = ec.max(this@varname);
                   by = 2^floor(log10(to - min));
                   P_ <- P_ + scale_x_discrete(limits=seq(from=min, to=to, by=by));
                 } else {
                   P_ <- P_ + geom_histogram(binwidth=this@binwidth, colour="black", fill=this@G_FILLCOLOR,na.action=na.exclude);
                 }
               }
               else {
                 P_ <- P_ + geom_histogram(binwidth=1, colour="black", fill=this@G_FILLCOLOR,na.action=na.exclude);
               }
               if (this@G_TITLE != "_AUTO_") P_ <- P_ + ggtitle(this@G_TITLE);
               P_ <- P_ + xlab(this@G_LABELX);
               P_ <- P_ + ylab(this@G_LABELY);
               P_ <- P_ + THEME;
               vp = O@varpart;
               if (vp != "") {
                 P_ <- P_ + facet_wrap(eval(parse(text = paste('~', vp, sep=''))), ncol=2);
               }
               P_ <- P_ + scale_y_continuous(expand = c(0,0))
               plot(P_)
             }
             Draw(this);
           });

# ------------------------------------------------------------------------------
# Table() method
# ------------------------------------------------------------------------------
setGeneric("Table", function(this, ...) {
  return(standardGeneric("Table"))
})

setMethod ("Table" , signature="ecr.histogram",
           function(this, fname="F_NONE") {
             print(this@wtable);
             if (fname != "F_NONE") {
               write.csv(this@WDS, file=fname, row.names=FALSE);
             }
           });

# -----------------------------------------------------------------------------
# function: constructor
# Return: an object of type ec.histogram
# -----------------------------------------------------------------------------
ecr.histogram <- function(df, x, by="", discrete=FALSE, start=Inf, binwidth=0) {
  return(new("ecr.histogram", df=df, x=x, by=by, discrete=discrete, start=start, binwidth=binwidth))
}
