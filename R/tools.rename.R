# ---- Rename a column of a GDS
# --------------------------------------------------------
ec.rename <- function(colname, newname)
{
  DF <- get("GDS", envir=.GlobalEnv);
  names(DF) <- sub(colname, newname, names(DF))
  assign('GDS', DF, envir=.GlobalEnv);
}

# ---- Rename a column of a dataframe
# --------------------------------------------------------
ecr.rename <- function(df, colname, newname)
{
  DF <- df
  names(DF) <- sub(colname, newname, names(DF))
  DF
}

