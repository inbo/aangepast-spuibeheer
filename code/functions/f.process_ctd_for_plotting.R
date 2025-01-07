#process_ctd_for_plotting
process_ctd_for_plotting<-function(date.min,date.max){
  ctd.long <- ctd %>% 
    dplyr::select(-druk) %>% 
    rename("debiet (m³/s)"="debiet", "temperatuur (°C)"="temperatuur", "neerslag (mm)" = "neerslag", "saliniteit (psu)" = "saliniteit", "spec. geleidbaarheid (mS/cm)" = "spgeleidbaarheid","act. geleidbaarheid (mS/cm)" = "geleidbaarheid") %>% 
    pivot_longer(cols=where(is.numeric),names_to="parameter",values_to="value") %>%
    dplyr::filter(datum.ctd>=date.min-lubridate::days(50) & datum.ctd<=date.max+lubridate::days(90))
  ctd.long$parameter<-factor(ctd.long$parameter, levels = c("act. geleidbaarheid (mS/cm)",
                                                            "spec. geleidbaarheid (mS/cm)",
                                                            "saliniteit (psu)",
                                                            "debiet (m³/s)",
                                                            "temperatuur (°C)",
                                                            "neerslag (mm)"))
  k=0
  p.list<-list()
  for (i in unique(ctd$loc.ctd)){
    k=k+1
    ctd.long.plot <- ctd.long %>% dplyr::filter(loc.ctd==i)
    os.plot<-os[which(os$site==unique(ctd.long.plot$site) & os$jaar==unique(lubridate::year(ctd.long.plot$datum.ctd))),]
    p.list[[k]]<-ggplot(ctd.long.plot, aes(x = datum.ctd, y = value)) + 
      geom_rect(data=os.plot, inherit.aes=FALSE, aes(xmin=open, xmax=dicht, ymin=-Inf, ymax=Inf), color="green", alpha=0.3, size=0.2) +
      geom_line(aes(group=group_plot)) +
      annotate("rect", xmin = date.min, xmax = date.max, ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.3) +
      xlab("Datum") + ylab("Waarde") +
      facet_wrap(~ parameter,scales="free_y",ncol=1) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15), axis.text.y = element_text(size=15), axis.title = element_text(size = 20), title = element_text(size = 30), strip.text = element_text(size=20)) +
      scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 month") + ggtitle(i)
  }
  names(p.list)=unique(ctd$loc.ctd)
  return(p.list)
}