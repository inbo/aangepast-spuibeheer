#process_ctd_series_for_plotting
process_ctd_series_for_plotting<-function(date.min,date.max){
  ctd <- ctd %>% 
    rename("debiet (m³/s)"="debiet", "geleidbaarheid (mS/cm)"="geleidbaarheid", "neerslag (mm)" = "neerslag") %>% 
    dplyr::filter(datum.ctd>=date.min-lubridate::days(50) & datum.ctd<=date.max+lubridate::days(90))
  k=0
  p.list<-list()
  for (i in unique(ctd$site)){
    k=k+1
    ctd.plot <- ctd %>% dplyr::filter(site==i) %>% mutate(loc.ctd=paste(distance,loc.ctd))
    os.plot<-os[which(os$site==unique(ctd.plot$site) & os$jaar==unique(lubridate::year(ctd.plot$datum.ctd))),]
    p.list[[k]]<-ggplot(ctd.plot, aes(x = datum.ctd, y = `geleidbaarheid (mS/cm)`)) + 
      geom_rect(data=os.plot, inherit.aes=FALSE, aes(xmin=open, xmax=dicht, ymin=-Inf, ymax=Inf), color="green", alpha=0.3, size=0.2) +
      geom_line(aes(group=group_plot)) + geom_smooth(aes(group=group_plot),colour="black",method = loess,alpha = 0.3,size=0.5) +
      annotate("rect", xmin = date.min, xmax = date.max, ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.3) +
      xlab("Datum") + ylab("Waarde") +
      facet_wrap(~ loc.ctd, ncol=2) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15), axis.text.y = element_text(size=15), axis.title = element_text(size = 20), title = element_text(size = 30), strip.text = element_text(size=15)) +
      scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 month") + ggtitle(i) + 
      geom_hline(data = data.frame(yint=0.87,parameter="geleidbaarheid (mS/cm)"), aes(yintercept = yint), linetype = "dotted", colour="red")
  }
  names(p.list)=unique(ctd$site)
  return(p.list)
}