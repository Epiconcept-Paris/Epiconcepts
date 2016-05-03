library(methods)

setClass("ec.Log",
         # ==== Inheritance
         # ==== Properties
         representation (
           JSON       = "list",
           script     = "character",
           outputdir  = "character",
           workdir    = "character",
           mode       = "character",
           manifest   = "data.frame",
           outputId   = 'numeric'
         )
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ec.Log",
  function(.Object)
  {
    
    setOutputDir <- function(od) {
      if (!file.exists(od)) {
        dir.create(od, recursive = TRUE);
      }
      return(od);
    }

    args <- commandArgs(TRUE);
#    print(sprintf("Current name: %s", args[0]));
    
    if (length(args) < 1) {
      .Object@mode <- "STANDARD";
      return(.Object);
    }

    .Object@JSON <- fromJSON(args[1]);
    .Object@workdir <- .Object@JSON$workdir;
    .Object@script <- .Object@JSON$script;
    .Object@outputId <- 100;
    
    setwd( .Object@workdir);
    
    
  # create and initialize manifest data.frame
  # --------------------------------------------------------------
  L = c("manifest.json", "JSON", "_", "_");
  df <- data.frame(rbind(L), row.names = NULL, stringsAsFactors = FALSE);
  names(df) <- c("file", "type", "title", "data");
  .Object@manifest <- df;
    
  output_dir <- sprintf("%s/%s", 'output', .Object@script);
  .Object@outputdir <- setOutputDir(output_dir);
  .Object@mode <- "REMOTE";

  .Object;
  }
);


# -----------------------------------------------------------------------------
# method Close
# -----------------------------------------------------------------------------
setMethod("Close" ,"ec.Log" ,
  function(this){
    # ------ First, create manifest.json
    cat("---------------------\n");
    JSON <- toJSON(this@manifest, pretty=TRUE, dataframe='rows');
    output = sprintf("%s/manifest.json", this@outputdir);
    sink(output);
    cat(JSON);
    sink();
    # ----- ZIP des Fichiers
    output = sprintf("%s/output.zip", this@outputdir);
    directory = sprintf("%s/", this@outputdir);
    zip(output, directory);
    
  }
)


# -----------------------------------------------------------------------------
# function: ec.Log (call real constructor)
# Return: an object of type ec.Log
# -----------------------------------------------------------------------------
ec.Log <- function()
{
  return(new("ec.Log"));
}
