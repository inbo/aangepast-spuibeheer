#process_conductiviteit_for_plotting
process_conductiviteit_series_for_plotting<-function(date.min,date.max){
  conductiviteit <- conductiviteit %>% 
    rename("debiet (m³/s)"="debiet", "geleidbaarheid (mS/cm)"="conductiviteit", "neerslag (mm)" = "neerslag") %>% 
    dplyr::filter(datum.conductiviteit>=date.min-lubridate::days(50) & datum.conductiviteit<=date.max+lubridate::days(90))
  k=0
  p.list<-list()
  
  # Group the data to calculate Y-limits PER SITE/FACET
  grouped_data <- conductiviteit %>% 
    group_by(site.conductiviteit) %>% 
    summarise(y_min = min(`geleidbaarheid (mS/cm)`, na.rm = TRUE),
              y_max = max(`geleidbaarheid (mS/cm)`, na.rm = TRUE))
  
  for (i in unique(conductiviteit$site.conductiviteit)){
    k=k+1
    conductiviteit.plot <- conductiviteit %>% dplyr::filter(site.conductiviteit==i) %>% mutate(loc.conductiviteit=paste(distance,loc.conductiviteit))
    
    # Get the site-specific Y-limits
    y_limits <- grouped_data %>% dplyr::filter(site.conductiviteit == i)
    y_min <- y_limits$y_min
    y_max <- y_limits$y_max
    
    conductiviteit.plot.daily <- conductiviteit.plot %>% mutate(date=as.POSIXct(paste(as.Date(datum.conductiviteit),"12:00:00"),tz="GMT")) %>% group_by(date,loc.conductiviteit) %>% 
      summarise(across(where(is.numeric), mean, na.rm=TRUE))
    
    os.plot<-os[which(os$site==unique(conductiviteit.plot$site.conductiviteit) & os$jaar==unique(lubridate::year(conductiviteit.plot$datum.conductiviteit))),] 
    os.plot$y_min_rect <- y_min
    os.plot$y_max_rect <- y_max
    
    p.list[[k]]<-ggplot(conductiviteit.plot, aes(x = datum.conductiviteit, y = `geleidbaarheid (mS/cm)`)) + 
      geom_rect(data=os.plot, inherit.aes=FALSE, aes(xmin=open, xmax=dicht, ymin=y_min_rect, ymax=y_max_rect), color="green", alpha=0.3, size=0.2) +
      geom_line(aes(group=group_plot)) + geom_point(data=conductiviteit.plot.daily,aes(x=date,y=`geleidbaarheid (mS/cm)`),color="black",shape=1,size=0.8) +
      annotate("rect", xmin = date.min, xmax = date.max, ymin = y_min, ymax = y_max, fill = "orange", alpha = 0.3) +
      xlab("Datum") + ylab("Waarde") +
      facet_wrap(~ loc.conductiviteit, ncol=2) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15), axis.text.y = element_text(size=15), axis.title = element_text(size = 20), title = element_text(size = 20), strip.text = element_text(size=15)) +
      scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 month") + ggtitle(i)
  }
  names(p.list)=unique(conductiviteit$site.conductiviteit)
  return(p.list)
}