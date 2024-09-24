#process_ctd_for_plotting
process_ctd_for_plotting<-function(date.min,date.max){
  ctd.long <- ctd %>% 
    pivot_longer(cols=where(is.numeric),names_to="parameter",values_to="value") %>%
    dplyr::filter(is.na(value)==FALSE & datum.ctd>=date.min-lubridate::days(30) & datum.ctd<=date.max+lubridate::days(30))
  k=0
  p.list<-list()
  for (i in unique(ctd$loc.ctd)){
    k=k+1
    p.list[[k]]<-ggplot(ctd.long %>% dplyr::filter(loc.ctd==i), aes(x = datum.ctd, y = value)) + 
      geom_line() + geom_smooth() +
      annotate("rect", xmin = date.min, xmax = date.max, ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.3) +
      xlab("Datum") + ylab("Waarde") +
      facet_wrap(~ parameter,scales="free_y") + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
      scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 month") + ggtitle(i)
  }
  names(p.list)=unique(ctd$loc.ctd)
  return(p.list)
}