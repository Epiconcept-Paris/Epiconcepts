ec.codebook <- function(detail=FALSE)
{
  for (x in colnames(GDS)) {
    print(ec.summary(x, detail=detail));
  }  
}

ec.outputDF <- function(df, type="html") {
  if (exists("OUTPUT_FORMAT")) {
    print(xtable(df), type=type, include.rownames = F) ;
  } else {
    print(df);
  }
}

ec.preview <- function(nrows=5, ncols=-1, df=NULL)
{
  if (is.null(df)) {
    df = GDS;
  }
  if (ncols == -1) {
    ncols = ncol(df);
    if (ncols > 20) {
      ncols = 20;
    }
  }
  
  NBCOLS = ncol(df);
  if (ncols > NBCOLS) {
    ncols = NBCOLS;
  }
  NMOD = NBCOLS %% ncols;
  NITER = NBCOLS %/% ncols;
  
  DF1 = head(df, nrows);
  DF2 = tail(df, nrows);
  
  DF = rbind(DF1, DF2)
  
  from = 1;
  to = from + ncols -1;
  
  for(i in 1:NITER) {
    #T <- xtable(DF[,from:to]);
    ec.outputDF(DF[,from:to]);
    #print(T, type="html");
    from <- from + ncols;
    to = from + ncols -1;
  }
  
  if (NMOD > 0) {
    if (NMOD > 1) {
      to = from + NMOD - 1;
      ec.outputDF(DF[,from:to]);
      #T <- xtable(DF[,from:to]);
      #print(T, type="html");
    }
    else {
      cn = colnames(df)[from];
      DF2 <- as.data.frame(DF[,from]);
      colnames(DF2) <- c(cn);
      ec.outputDF(DF2);
      #T <- xtable(DF2);
      #print(T, type="html");
    }
  }
}

# gdsSort <- function(by , decrease=TRUE)
# {
#   if (decrease == TRUE) {
#     GDS <<- GDS[rev(order(GDS[, by])),]
#   }
#   else {
#     GDS <<- GDS[order(GDS[, by]),]
#   }
# }

ec.removeMissing <- function()
{
  GDS <<- na.omit(GDS);
}

# ===================================================================
# Function: ec.toDate
# Description : Convertit une variable en date
# -------------------------------------------------------------------
ec.toDate <- function(x, format="%Y-%m-%d") {
  if (length(c(x)) > 1) {
    return (as.Date(as.character(x), format=format))
  }
  DF <- get("GDS", envir=.GlobalEnv);
  DF[, x] <- as.Date(as.character(DF[, x]), format=format);
  assign("GDS", DF, envir=.GlobalEnv);
}

ec.recode <- function(x, where, by, gen="")
{
  DF = get("GDS", envir=.GlobalEnv);
  L <- length(where);
  vn = x;
  
  # Creating a new column if necessary
  if (gen != "") {
    DF <- cbind(DF, XNEWCOL=DF[,x]);
    names(DF)[names(DF)=="XNEWCOL"] <- gen;
    vn = gen;
  }
  
  # for each conditionnal statement
  for (i in 1:L) {
    L <- where[[i]];
    L <- replace(L, is.na(L), FALSE);
    DF[L, vn] <- by[i];
  }
  assign("GDS", DF, envir=.GlobalEnv);
}


ec.labels <- function(x, to="", labels=list())
{
  DF <- get("GDS", envir=.GlobalEnv);
  # x is a list of column
  if (is.list(x)) {
    for (i in x) {
      ec.labels(i, labels=labels);
    }
  }
  else if (to != "") {
    # x is the start column and to the end column
    CN = names(DF);
    START_ = match(x, CN);
    END_ = match(to, CN);
    for (i in START_:END_) {
      ec.labels(CN[i], labels=labels);
    }
  } else {
    # there is only one column
    DF[, x] <- as.factor(DF[, x]);
    DF[, x] <- revalue(DF[, x], labels);
    assign('GDS', DF, envir=.GlobalEnv)
  }
}

gdsLabelToLevel <- function(x)
{
  if(is.factor(GDS[,x])) {
    GDS[, x] <<-  as.integer(GDS[, x]) - 1;
  }
}

# ---- Move a column of GDS or other data.frame
# --------------------------------------------------------
ec.move <- function(tomove, where = "last", target = NULL, df=FALSE) {
  data = df
  if (is.logical(df)) {
    data = get("GDS", envir=.GlobalEnv);
  }
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      #if (is.null(ba)) stop("must specify ba column")
      data[append(temp, values = tomove, after = (match(target, temp)-1))]
    },
    after = {
      #if (is.null(ba)) stop("must specify ba column")
      data[append(temp, values = tomove, after = (match(target, temp)))]
    })
  if (is.logical(df)) {
    assign('GDS', x, envir=.GlobalEnv)
  }
  x
}


ec.save <- function(name)
{
  FName = sprintf("%s.rds", name);
  saveRDS(GDS, FName);
}

updateManifest <-   function(filename, type, title="", data="") {
  L <- get("Log", envir=.GlobalEnv);
  line = c(filename, type, title, data);
  L@manifest = rbind(Log@manifest, line);
  assign('Log', L, envir=.GlobalEnv);
}

saveToCSV <- function(DF, name) {
  L <- get("Log", envir=.GlobalEnv);
  L@outputId <- L@outputId + 1;
  FName = sprintf("%s-%s.csv", L@outputId, name);
  output = sprintf("%s/%s", L@outputdir, FName);
  write.csv(GDS, file = output, row.names = FALSE);
  updateManifest(FName, "CSV", title="", data="");
}
