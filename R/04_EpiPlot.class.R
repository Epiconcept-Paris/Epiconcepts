require(ggplot2)
library(methods)
setClass("EpiPlot",
  # ==== Properties
  representation (
    WDS         = "data.frame",
    # -- Text
    G_TITLE     = "character",
    G_TITLELEFT = "character",
    G_TITLERIGHT= "character",
    G_LABELX    = "character",
    G_LABELY    = "character",
    G_FOOTER    = "character",
    L_TITLE     = "character",
    # -- Colors
    G_FILLCOLOR = "character",
    G_COLORLEFT = "character",
    G_COLORRIGHT= "character",
    L_GRADIENT  = "character",
    T_FGCOLOR   = "character",
    T_BGCOLOR   = "character",
    # -- Image
    I_FORMAT    = "character",
    I_WIDTH     = "numeric",
    I_HEIGHT    = "numeric"
  ),
  #-- Default values
  prototype(
    # -- Text
    G_TITLE     = "_AUTO_",
    G_TITLELEFT = "_AUTO_",
    G_TITLERIGHT= "_AUTO_",
    G_LABELX    = "_AUTO_",
    G_LABELY    = "_AUTO_",
    G_FOOTER    = "_AUTO_",
    L_TITLE     = "_AUTO_",
    # -- Colors
    G_FILLCOLOR = "lightblue",
    G_COLORLEFT = "lightblue",
    G_COLORRIGHT= "pink",
    L_GRADIENT  = "_AUTO_",
    T_FGCOLOR   = "_AUTO_",
    T_BGCOLOR   = "_AUTO_",
    # -- Image
    I_FORMAT    = "png",
    I_WIDTH     = 700,
    I_HEIGHT    = 400
  )
)

# ------------------------------------------------------------------------------
# Constructor
# ------------------------------------------------------------------------------
setMethod("initialize",
          "EpiPlot",
          function(.Object) {
            .Object
})
