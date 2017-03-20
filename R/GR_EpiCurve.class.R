ec.epiCurve <- function(x,
                        date = NULL,
                        freq=NULL,
                        cutvar=NULL,
                        period = NULL,
                        cutorder = NULL,
                        colors = NULL,
                        title = NULL,
                        xlabel = NULL,
                        ylabel=NULL,
                        note = NULL) {
  
  
  DF <- x
  .cutorder <- cutorder
  .color = colors
  
  
  if (is.null(.color)) {
    .color <- rev(brewer.pal(9, "Spectral"))
  }
  
  
  if (!is.null(date)) {
    names(DF)[names(DF)==date] <- "Date"
    if (period=="week") {
      DF$Date <- ISOweek2date(paste(DF$Date, "-1", sep=""))
    }
    else if(period == "month") {
      DF$Date <- as.Date(paste(DF$Date,"-01",sep=""))
    }
    else {
      DF$Date <- as.Date(DF$Date)
    }
  }
  
  if (!is.null(freq)) {
    names(DF)[names(DF)==freq] <- "Freq"
  }
  
  if (!is.null(cutvar)) {
    names(DF)[names(DF)==cutvar] <- "Cut"
    if (!is.null(.cutorder)) {
      DF$Cut <- factor(DF$Cut, levels=cutorder, ordered=TRUE)
    } 
    else {
      DF$Cut <- as.factor(DF$Cut)
      .cutorder <- levels(DF$Cut)
    }
  }
  else {
    DF$Cut <- rep("1 cas", length.out = nrow(DF))
  }
  
  
  # Calcul du max après agrégation
  # -----------------------------------------------------------------
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
  
  if (period == "day") {
    if (max(DF$Date) - min(DF$Date) > 365) {
      print("Error: Too much days, must be < 366")
      return(FALSE)
    }
    DW = data_frame(Date = seq(min(DF$Date), max(DF$Date), by="day"))
    DF <- dplyr::left_join(x = DW, y = DF, by = "Date") %>%
      as.data.frame()
    DF$Freq[is.na(DF$Freq)] <- 0
    DF <- mutate(DF, Day = format(Date, "%Y-%m-%d")) %>%
      mutate(Date = NULL) %>%
      mutate(Date = Day)
  }
  
  if (period == "week") {
    DW = data_frame(Date = seq(min(DF$Date), max(DF$Date), by="week"))
    DW$Date <- ISOweek(DW$Date)
    DF$Date <- ISOweek(DF$Date)
    DF <- dplyr::left_join(x = DW, y = DF, by = "Date") %>%
      as.data.frame()
    DF$Freq[is.na(DF$Freq)] <- 0

}

  if (period == "month") {
    DM = data_frame(Date = seq(min(DF$Date), max(DF$Date), by="month"))
    DF <- dplyr::left_join(x = DM, y = DF, by = "Date") %>%
      as.data.frame()
    DF$Freq[is.na(DF$Freq)] <- 0
    DF <- mutate(DF, Mois = format(Date, "%Y-%m")) %>%
      mutate(Date = NULL) %>%
      mutate(Date = Mois)
  }
  
  P_ <- ggplot(arrange(DF, Cut), aes(x=Date, y=Freq, fill=factor(Cut))) 
  P_ <- P_ +  geom_bar(stat='identity', width=1);
  
  P_ <- P_ + scale_fill_manual(values = .color, labels=.cutorder,
                               breaks=levels(DF$Cut), limits=levels(DF$Cut),
                               guide = guide_legend(reverse = TRUE))
  # Compute y ticks
  # -----------------------------------------------------------------
  if (MaxValue > 2) {
    P_ <- P_ + scale_y_continuous(expand = c(0,0))
  } else {
    P_ <- P_ + scale_y_continuous(expand = c(0,0), breaks = c(0,seq(1,MaxValue)))
  }
  
  # Draw hlines and vlines
  # -----------------------------------------------------------------
  P_ <- P_ + geom_hline(yintercept=seq(1, MaxValue, by=1), colour="white", size=0.3)
  
  if (nrow(DF) > 1) {
    P_ <- P_ + geom_vline(xintercept = seq(1.5, nrow(DF), by=1), colour="white", size=0.3)
  }
  
  note <- gsub('(.{1,90})(\\s|$)', '\\1\n', note)
  P_ <- P_ + xlab(paste(xlabel, note, sep="\n\n")) + 
    ylab(ylabel) +
    labs(title = title, fill = "") +
    coord_fixed(ratio=1) +
    theme_bw() +
    theme(panel.border = element_blank()) +
    theme(axis.text.x = element_text(angle=90))+
    theme(axis.line.x = element_line(colour="black", linetype="solid", size = 0.5),
          axis.line.y = element_line(colour="black", linetype="solid", size = 0.5))
    P_
}

