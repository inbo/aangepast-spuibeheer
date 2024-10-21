single_wavelet_analysis<-function(data,response,date.min,date.max){
  #data<-ctd%>%dplyr::filter(loc.ctd=="sas slijkens")
  rownames(data)=data$datum.ctd
  data <- data %>% dplyr::filter(datum.ctd>=date.min-lubridate::days(30) & datum.ctd<=date.max+lubridate::days(30))
  data[,response]<-imputeTS::na_locf(data[,response])
  
  wave.test.single<-analyze.wavelet(data,my.series=response,loess=0.75,dt=1)#loess=0 means no detrending
  wave.test.single$Power.avg
  wave.test.single$Period
  wave.test.single$P
  x=cbind(wave.test.single$Power.avg,wave.test.single$Period)
  
  ## Plot of wavelet power spectrum (with equidistant color breakpoints):
  wt.image(wave.test.single, color.key = "interval", main = "wavelet power spectrum",legend.params = list(lab = "wavelet power levels"),periodlab = "period (hours)")
  ## Plot of average wavelet power:
  wt.avg(wave.test.single, siglvl = 0.05, sigcol = "red",periodlab = "period (hours)")
  ## Default image of phases:
  wt.phase.image(wave.test.single,main = "image of phases",periodlab = "period (hours)")
  ## With time elapsed in days
  ## (starting from 0 and proceeding in steps of 50 days)
  ## instead of the (default) time index:
  index.ticks <- seq(1, nrow(data), by = 12*24)
  index.labels <- (index.ticks-1)/24
  wt.phase.image(wave.test.single,main = "image of phases",periodlab = "period (hours)",timelab = "time elapsed (hours)",spec.time.axis = list(at = index.ticks, labels = index.labels))
  ## The same plot, but with (automatically produced) calendar axis:
  wt.phase.image(wave.test.single,main = "image of phases", periodlab = "period (hours)",show.date = TRUE)
  ## For further axis plotting options:
  ## Please see the examples in our guide booklet,
  ## URL http://www.hs-stat.com/projects/WaveletComp/WaveletComp_guided_tour.pdf.
  ## Image plot of phases with numerals as labels of the color legend bar:
  wt.phase.image(wave.test.single,legend.params=list(pi.style = FALSE, label.digits = 2))
  return(wave.test.single)
}