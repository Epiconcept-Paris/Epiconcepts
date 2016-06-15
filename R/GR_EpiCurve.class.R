ec.epiCurve <- function(x,
                        date = NULL,
                        freq=NULL,
                        cutvar=NULL,
                        period = NULL,
                        cutorder = NULL,
                        color = NULL,
                        title = NULL,
                        xlabel = NULL,
                        ylabel=NULL) {
  
  
  DF <- x
  
  if (!is.null(date)) {
    names(DF)[names(DF)==date] <- "Date"
    DF$Date <- as.Date(DF$Date)
  }
  
  if (!is.null(freq)) {
    names(DF)[names(DF)==freq] <- "Freq"
  }
  
  if (!is.null(cutvar)) {
    names(DF)[names(DF)==cutvar] <- "Cut"
  }
  else {
    DF$Cut <- rep("1 cas", length.out = nrow(DF))
  }
  
  if (!is.null(cutorder)) {
    DF$Cut <- factor(DF$Cut, levels=cutorder, ordered=TRUE)
  }
  
  # Calcul du max après agrégation
  if (!is.null(cutvar)) {
    TMP <- DF %>%
      dplyr::group_by(Date) %>%
      dplyr::summarize(total=sum(Freq)) %>%
      as.data.frame()
    MaxValue = max(TMP$total)
  } else {
    MaxValue = max(DF$Freq)
  }
  
  
  # Calcul du nombre de ticks 'lisibles'
  TMP <- dplyr::distinct(DF, Date)
  N = nrow(DF)
  n_ticks = max(as.integer(log10(N)*10)-10, 1)
  
  # Labels de l'axe x
  d_labels = "%d"
  d_breaks = sprintf("%d days", n_ticks)
  if (period == "week") {
    DW = data_frame(Date = seq(min(DF$Date), max(DF$Date), by="week"))
    DF <- dplyr::left_join(x = DW, y = DF, by = "Date") %>%
      as.data.frame()
    DF$Freq[is.na(DF$Freq)] <- 0
    DF <- mutate(DF, Week = format(Date, "%V")) %>%
      mutate(Date = NULL) %>%
      mutate(Date = Week)
    
    d_labels = "%W"
    d_breaks = sprintf("%d weeks", n_ticks)
  }
  
  
  
  P_ <- ggplot(arrange(DF, Cut), aes(x=Date, y=Freq, fill=Cut)) 
  if (period == "day") {
    P_ <- P_ + geom_bar(stat='identity') +
      scale_x_date(date_breaks=d_breaks, date_labels = d_labels, expand = c(0,0))
  } else {
    P_ <- P_ +  geom_bar(stat='identity', width=1);
  }
  
  P_ <- P_ + scale_fill_manual(values = color, guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(expand = c(0,0)) +
    
    geom_hline(yintercept=seq(1, MaxValue, by=1), colour="white", size=0.3) +
    geom_vline(xintercept = seq(1.5, nrow(DF), by=1), colour="white", size=0.3) +
    xlab(xlabel) + 
    ylab(ylabel) +
    labs(title = title, fill = "") +
    coord_fixed(ratio=1) +
    theme_bw() +
    theme(panel.border = element_blank()) +
    theme(axis.line.x = element_line(color="black", size = 0.25),
          axis.line.y = element_line(color="black", size = 0.25))
  
  P_
}

