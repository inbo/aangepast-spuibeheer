#process_ctd_series_for_plotting
process_ctd_series_for_plotting<-function(date.min,date.max){
  ctd <- ctd %>% 
    rename("debiet (m³/s)"="debiet", "geleidbaarheid (mS/cm)"="geleidbaarheid", "neerslag (mm)" = "neerslag", "saliniteit (psu)" = "saliniteit", "spec. geleidbaarheid (mS/cm)" = "spgeleidbaarheid") %>% 
    dplyr::filter(datum.ctd>=date.min-lubridate::days(50) & datum.ctd<=date.max+lubridate::days(90))
  k=0
  p.list<-list()
  
  # Group the data to calculate Y-limits PER SITE/FACET
  grouped_data <- ctd %>% 
    group_by(site) %>% 
    summarise(y_min = min(`spec. geleidbaarheid (mS/cm)`, na.rm = TRUE),
              y_max = max(`spec. geleidbaarheid (mS/cm)`, na.rm = TRUE))
  
  for (i in unique(ctd$site)){
    k=k+1
    ctd.plot <- ctd %>% dplyr::filter(site==i) %>% mutate(loc.ctd=paste(distance,loc.ctd))
    
    # Get the site-specific Y-limits
    y_limits <- grouped_data %>% dplyr::filter(site == i)
    y_min <- y_limits$y_min
    y_max <- y_limits$y_max
    
    ctd.plot.daily <- ctd.plot %>% mutate(date=as.POSIXct(paste(as.Date(datum.ctd),"12:00:00"),tz="GMT")) %>% group_by(date,loc.ctd) %>% 
      summarise(across(where(is.numeric), mean, na.rm=TRUE))
    
    os.plot<-os[which(os$site==unique(ctd.plot$site) & os$jaar==unique(lubridate::year(ctd.plot$datum.ctd))),]
    # **CRITICAL CHANGE: Ensure os.plot has the correct y-values for mapping**
    # Since y_min and y_max are vectors of length 1, they are recycled for all rows in os.plot.
    
    p <- ggplot(ctd.plot, aes(x = datum.ctd, y = `spec. geleidbaarheid (mS/cm)`)) 
    
    # Only add the green geom_rect layer if data exists in os.plot
    if (nrow(os.plot) > 0) {
      # Define y-limits for the rectangle explicitly in the local data frame
      # This works ONLY if os.plot has 1 or more rows
      os.plot$y_min_rect <- y_min
      os.plot$y_max_rect <- y_max
      
      p <- p + geom_rect(data=os.plot, 
                         inherit.aes=FALSE, 
                         # Map the Y-limits to the explicit columns in os.plot
                         aes(xmin=open, xmax=dicht, ymin=y_min_rect, ymax=y_max_rect), 
                         color="green",            # Use FILL, not color, for the area
                         alpha=0.3, 
                         size=0.2)
    }
    
    # ----------------------------------------------------
    # Continue building the plot 'p'
    # ----------------------------------------------------
    p.list[[k]] <- p + 
      geom_line(aes(group=group_plot)) + 
      geom_point(data=ctd.plot.daily,aes(x=date,y=`spec. geleidbaarheid (mS/cm)`),color="black",shape=1,size=0.8) +
      
      # annotate() works because it uses static limits
      annotate("rect", xmin = date.min, xmax = date.max, ymin = y_min, ymax = y_max, fill = "orange", alpha = 0.3) +
      
      xlab("Datum") + ylab("spec. geleidbaarheid (mS/cm)") +
      facet_wrap(~ loc.ctd, ncol=2) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15), axis.text.y = element_text(size=15), axis.title = element_text(size = 20), title = element_text(size = 30), strip.text = element_text(size=15)) +
      scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 month") + 
      ggtitle(i)
  }
  names(p.list)=unique(ctd$site)
  return(p.list)
}