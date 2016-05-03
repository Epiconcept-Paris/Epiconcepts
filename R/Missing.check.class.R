library(methods)

setClass("ec.Missing.check",
         # ==== Inheritance
         # ==== Properties
         representation (
           varname   = "character",
           detail    = "logical",
           missdesc  = "data.frame",
           missbyrow = "data.frame"
         )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ec.Missing.check",
          function(.Object, vars, sort)
          {
            Names = names(GDS)
            if (length(vars) > 0) {
              Names = vars;
            }
            
            # Simple description of missing values
            # ------------------------------------
            missdesc <- function(Names)
            {
              Colnames = c("Variable", "Missing", "% Missing");
              Effectif = nrow(GDS);
              Missing  = vector();
              PMissing = vector();
              
              for (N in Names) {
                Miss = sum(is.na(VAL(N)));
                PMiss = sprintf("%5.2f", (Miss / Effectif) * 100);
                Missing = c(Missing, Miss);
                PMissing = c(PMissing, PMiss);
              }
  
              DF = data.frame(cbind(Names, as.numeric(Missing), PMissing));
              names(DF) <- Colnames;
              DF[,2] <- as.numeric(as.character(DF[,2]));
              DF[,3] <- as.numeric(as.character(DF[,3]));
              if (sort == TRUE) {
                DF <- DF[rev(order(DF[, "Missing"])),]
              }
              return(DF);
            }

            
            missbyrow <- function(Names, Effectif)
            {
              Colnames = c("Miss/row", "Frequency", "Percent", "Cumul");
              counts = apply(GDS,1,function(x) sum(is.na(x[Names])));
              d <- as.data.frame(counts);
              t <- table(counts);
              vals = names(t);
              t <- cbind(t, round(prop.table(t)*100,2));
              DF1 <- as.data.frame(t);
              Cum <- cumsum(DF1[,2]);
              DF2 <- data.frame(cbind(vals, DF1$t, DF1$V2, Cum));
              names(DF2) <- Colnames;
              return(DF2);
            }
            
            .Object@missdesc <- missdesc(Names);
            .Object@missbyrow <- missbyrow(Names, Effectif);
            
            .Object;
          });

# -----------------------------------------------------------------------------
# function: ec.Missing.check (call real constructor)
# Return: an object of type ec.Missing.check
# -----------------------------------------------------------------------------
ec.Missing.check <- function(vars=vector(), sort=sort)
{
  return(new("ec.Missing.check", vars=vars, sort=sort));
}

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"ec.Missing.check" ,
    function(object){
      cat("\n-----------------------------------\n");
      print(object@missdesc);
      cat("-----------------------------------\n");
      cat("\n-----------------------------------\n");
      print(object@missbyrow);
      cat("-----------------------------------\n");
    }
)

# library(Epiconcepts);
# data(Tiramitsu)
# ec.Use(Tiramitsu);
# #P <- ec.Missing.check(vars=c("tira","wmousse","dmousse","beer"), sort=TRUE)
# P <- ec.Missing.check(sort=TRUE)
# print(P)
