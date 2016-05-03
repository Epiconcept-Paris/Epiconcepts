ecr.proportion <- function(x, label="Proportion")
{
  
  DF <- p.proportion(x, label)

  digits =  c(0,0,0,4,4,5,5);
  align  =  c("l","c","c","r","r","r","r");
  ec.xtable(DF, digits=digits, align=align)
}
