library(methods)
library(graphics)

#source("R/EpiPlot.class.R");

setClass("ec.agePyramid",
  # ==== Inheritance
  contain = "EpiPlot",
  # ==== Properties
  representation (
    varage = "character",
    vary   = "character",
    agecut = "numeric",
    agemin = "numeric",
    agemax = "numeric",
    wtable = "table",
    PYR.AGES = "vector",
    PYR.LEFTLEVEL = "character",
    PYR.RIGHTLEVEL = "character",
    PYR.YMAX = "numeric"
  )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ec.agePyramid",
  function(.Object, varage, vary, agecut, agemin, agemax) {
    .Object@varage = varage;
    .Object@vary = vary;
    .Object@agecut = agecut;
    .Object@agemin = agemin;
    .Object@agemax = agemax;
    
    df <- get("GDS", envir=.GlobalEnv)
    
    # ---------------------------------------------------------------
    computeAgeScale <- function()
    {
      age    = .Object@varage;
      agecut = .Object@agecut;
      agemin = .Object@agemin;
      agemax = .Object@agemax;
      
      if (agecut == 0) {
        agecut = 1;
        agemax = ec.max(varage);
        agemin = ec.min(varage);
      }
      AS <- createAgeScale(df$age, lower=agemin, upper=agemax, by=agecut);
      return(AS);
    }
    
    # ---------------------------------------------------------------
    VX = eval(parse(text = paste('df$', .Object@varage, sep='')));
    VY = eval(parse(text = paste('df$', .Object@vary, sep='')));
    PART <- as.factor(VY);
    L  = levels(PART);
    .Object@PYR.LEFTLEVEL = L[1];
    .Object@PYR.RIGHTLEVEL = L[2];
    .Object@PYR.AGES <- computeAgeScale();
    DLEFT <- subset(VX, PART == .Object@PYR.LEFTLEVEL);
    DRIGHT <- subset(VX, PART == .Object@PYR.RIGHTLEVEL);
    TAB <<- table(.Object@PYR.AGES, VY);
    .Object@PYR.YMAX = max(max(TAB[,1]), max(TAB[,2]));
    #print(TAB);
    WDS <- data.frame(TAB);
    #colnames(WDS) <- c(.Object@varage, .Object@vary, "Freq");
    colnames(WDS) <- c("X", "Y" , "Freq");
    .Object@WDS = WDS;
    .Object;
  }
)


# -----------------------------------------------------------------------------
# function: ec.agePyramid (call real constructor)
# Return: an object of type ec.agePyramide
# -----------------------------------------------------------------------------
ec.agePyramid <- function( varage, vary, agecut=0, agemin=0, agemax=100)
{
  library(gridExtra);
  return(new("ec.agePyramid", varage, vary, agecut, agemin, agemax));
}


setMethod ("Plot", signature(this="ec.agePyramid"),
  function(this,
    title="",
    footer="",
    bgcolor="",
    ...)
  {
    #this = OPYR;
    GBASE <- ggplot(data = this@WDS, aes(x=X, y=Freq));
    this@G_TITLE = ifelse(title != "",  sprintf("\n%s\n",title), "");
    this@G_FOOTER = ifelse(footer != "", sprintf(" \n%s\n ",footer), "");
    
    Draw <- function(O_) {
      GLEFT <- drawPyramideLeft(O_);
      GRIGHT <- drawPyramideRight(O_);
      
      gg1 <- ggplot_gtable(ggplot_build(GLEFT))
      gg2 <- ggplot_gtable(ggplot_build(GRIGHT))
      G_FOOTER = "\nRepartition par tranche d'ages\n"  

      #deviceOn("PYRamide", "svg", width=700)
      grid.arrange(gg1,gg2,ncol=2,widths=c(0.54,0.46), clip=TRUE,
                   main=textGrob(O_@G_TITLE, gp=gpar(font=2)),
                   sub=textGrob(O_@G_FOOTER, gp=gpar(font=1)))
      #dev.off()
      
    }
    # ----------------------------------------------------------------------------------
    drawPyramideLeft <- function(O_) {
      LEFTTHEM = theme(plot.margin=unit(c(1,-0.25,1,1),"lines")) +
        theme(axis.text.x = element_text(face="bold", colour="black", size=rel(1.1)))+
        theme(axis.text.y = element_text(face="bold", colour="black", size=rel(1.1)))
      
      LG <- GBASE + 
        geom_bar(subset = .(Y == O_@PYR.LEFTLEVEL),stat="identity", position = "identity",
                 fill=O_@G_COLORLEFT, colour="black", width=1) +
        xlim(levels(O_@PYR.AGES)) +
        scale_y_continuous(trans = 'reverse', limits=c(O_@PYR.YMAX, 0), expand=c(0,0)) +
        ylab(NULL) + xlab(NULL) +
        ggtitle(paste(O_@vary, O_@PYR.LEFTLEVEL, sep = " = ")) +
        LEFTTHEM +
        coord_flip();
      return(LG);
    }
    
    # ----------------------------------------------------------------------------------
    drawPyramideRight <- function(O_) {
      RIGHTTHEM = theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
        theme(plot.margin=unit(c(1,1,1,-0.25),"lines")) +
        theme(axis.text.x = element_text(face="bold", colour="black", size=rel(1.1)))
      
      RG <- GBASE + 
        geom_bar( subset = .(Y == O_@PYR.RIGHTLEVEL), stat="identity", position = "identity",
                  fill=O_@G_COLORRIGHT, colour="black", width=1) +
        xlim(levels(O_@PYR.AGES)) +
        scale_y_continuous(limits=c(0,O_@PYR.YMAX), expand=c(0,0)) +
        ggtitle(paste(O_@vary, O_@PYR.RIGHTLEVEL, sep = " = ")) +
        xlab(NULL) + ylab(NULL) +
        RIGHTTHEM +
        coord_flip()
      return(RG);
    }
    
    Draw(this);
  }
);

