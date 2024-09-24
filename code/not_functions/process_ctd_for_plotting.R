#process_ctd_for_plotting
ctd.long <- ctd %>% pivot_longer(cols=where(is.numeric),names_to="parameter",values_to="value")
k=0
p.list<-list()
for (i in unique(ctd$loc.ctd)){
  k=k+1
  p.list[[k]]<-ggplot(ctd.long[which(ctd.long$loc.ctd==i),], aes(x = datum.ctd, y = value)) + 
    geom_line() + geom_smooth() +
    annotate("rect", xmin = as.POSIXct("2023-03-01 00:00:00"), xmax = as.POSIXct("2023-05-26 00:00:00"), ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.3) +
    xlab("Datum") + ylab("Waarde") +
    facet_wrap(~ parameter,scales="free_y") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
    scale_x_datetime(limits=c(as.POSIXct("2023-02-01 00:00:00"), as.POSIXct("2023-07-01 00:00:00")),date_labels = "%d-%m-%Y", date_breaks = "1 month") + ggtitle(i)
}
names(p.list)=unique(ctd$loc.ctd)