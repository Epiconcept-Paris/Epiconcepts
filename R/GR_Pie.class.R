library(methods)

setClass("ec.pie",
  # ==== Inheritance
  contain = "EpiPlot",
  # ==== Properties
  representation (
    varname = "character",
    varpart = "character",
    wtable = "table",
    varx = "vector",
    field  = "vector"
  )           
)

# ------------------------------------------------------------------------------
# Constructor
# ------------------------------------------------------------------------------
setMethod("initialize", 
  "ec.pie",
  function(.Object, x, y="") {
    .Object@varname = x;
    .Object@varpart = y;
    
    #GDS <- get("GDS", envir=Eec.pienv);
    
    V1 = eval(parse(text = paste('GDS', x, sep='$')));
    if (y == "") {
      .Object@wtable = table(V1);
      .Object@WDS = data.frame(.Object@wtable);
      colnames(.Object@WDS)[1] <- x;
    } else {
      #.Object@varpart = y;
      V2 = eval(parse(text = paste('GDS', y, sep='$')));
      .Object@WDS = ddply(GDS, .(V2), function(d) {
        V1 = eval(parse(text = paste('d', x, sep='$')));
        data.frame(table(V1)/length(V1)*100)
      })
      colnames(.Object@WDS)[1] <- y;
      colnames(.Object@WDS)[2] <- x;
    }
    .Object
})

setMethod ("ec.plot" , signature="ec.pie",
  function(this,
           title="",
           legendtitle="",
           gradient="",
           footer="",
           bgcolor=""
           ) {
    this@G_TITLE = ifelse(title != "", title, this@G_TITLE);
    this@G_LABELY = ifelse(footer != "", footer, this@G_LABELY);
    this@L_GRADIENT = ifelse(gradient != "", gradient, this@L_GRADIENT);
    this@T_BGCOLOR = ifelse(bgcolor == "", "white", bgcolor);
    this@L_TITLE  = ifelse(legendtitle != "", legendtitle, this@varname);
    THEME <- theme(axis.ticks = element_blank()) +
             theme(axis.text  = element_blank()) +
             theme(panel.grid.major = element_blank()) + 
             theme(panel.grid.minor = element_blank()) +
             theme(panel.background = element_rect(fill = this@T_BGCOLOR))
    # -------------------------------------------------------------------------
    # Effective drawing
    # -------------------------------------------------------------------------    
    Draw <- function(O) {
      Factor <- this@varname;
      # A ec.pie chart is a stacked bar chart + polar coordinates
      P_ <- ggplot(this@WDS, aes_string(x = factor(1), y = "Freq", fill = Factor))
      P_ <- P_ + geom_bar(width = 1, stat = "identity", colour="black");
      P_ <- P_ + coord_polar(theta = "y");
      if (this@G_TITLE != "_AUTO_") P_ <- P_ + ggtitle(this@G_TITLE);
      P_ <- P_ + xlab(NULL);
      if (this@G_LABELY != "_AUTO_") P_ <- P_ + ylab(this@G_LABELY);
      if (this@L_GRADIENT != "_AUTO_") P_ <- P_ + scale_fill_brewer(name=this@L_TITLE, palette=this@L_GRADIENT);
      P_ <- P_ + THEME;
      vp = O@varpart;
      if (vp != "") {
        P_ <- P_ + facet_wrap(eval(parse(text = paste('~', vp, sep=''))), ncol=2);
      }
      P_
    }
    Draw(this);
  });

# ------------------------------------------------------------------------------
# Table() method
# ------------------------------------------------------------------------------
setGeneric("Table", function(this, ...) {
  return(standardGeneric("Table"))
})

setMethod ("Table" , signature="ec.pie",
           function(this, fname="F_NONE") {
             print(this@wtable);
             if (fname != "F_NONE") {
               write.csv(this@WDS, file=fname, row.names=FALSE);
             }
           });

# -----------------------------------------------------------------------------
# function: constructor
# Return: an object of type ec.pie
# -----------------------------------------------------------------------------
ec.pie <- function(x, y="") {
  return(new("ec.pie", x=x, y=y))
}

          