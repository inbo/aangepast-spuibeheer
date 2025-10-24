plot_compare_inbo_waterinfo<-function(loc.inbo,loc.waterinfo,date.min,date.max,canal,output){
  
  inbo <- ctd %>% dplyr::filter(loc.ctd == loc.inbo & lubridate::year(datum.ctd)==2025) %>% dplyr::select(datum.ctd,spgeleidbaarheid,group_plot) %>% set_colnames(c("date","value","group_plot")) %>% mutate(sensor="INBO")
  waterinfo <- conductiviteit %>% dplyr::filter(loc.conductiviteit == loc.waterinfo & lubridate::year(datum.conductiviteit)==2025) %>% dplyr::select(datum.conductiviteit,conductiviteit,group_plot) %>% set_colnames(c("date","value","group_plot")) %>% mutate(sensor="Waterinfo")
  
  data=rbind(inbo,waterinfo) %>% dplyr::filter(date>=date.min & date<=date.max)
  
  os.plot<-os[which(os$site=="KGO" & os$jaar==2025),]
  
  y_min <- min(data$value, na.rm = TRUE)
  y_max <- max(data$value, na.rm = TRUE)
  
  p<-ggplot(data , aes(x = date, y = value)) + 
    geom_rect(data=os.plot, inherit.aes=FALSE, aes(xmin=open, xmax=dicht, ymin=y_min, ymax=y_max), color="green", alpha=0.3, size=0.2) +
    geom_line(aes(group=group_plot, colour=sensor), alpha=0.6) +
    #annotate("rect", xmin = date.min, xmax = date.max, ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.3) +
    xlab("Datum") + ylab("spec. geleidbaarheid (mS/cm)") +
    #facet_wrap(~ parameter,scales="free_y",ncol=1) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15), axis.text.y = element_text(size=15), axis.title = element_text(size = 20), title = element_text(size = 30), strip.text = element_text(size=20), legend.position = "bottom") +
    scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 week")
  if (output=="html"){ggplotly(p)} else{p}
}