ec.xtable <- function(df, align=NULL, digits=2, caption="") {
  if (exists("OUTPUT_FORMAT")) {
    require(knitr);
    require(xtable);
    output <- xtable(df, align=align, digits=digits, caption=caption);
    print(output, type = "html", include.rownames = F, comment = F);
#     print(kable(df, align=align, digits=digits, caption=caption));
} else {
    print(df);
  }
}
