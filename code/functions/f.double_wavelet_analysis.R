double_wavelet_analysis<-function(data,response,date.min,date.max){
  #data<-ctd%>%dplyr::filter(loc.ctd=="sas slijkens")
  rownames(data)=data$datum.ctd
  data <- data %>% dplyr::filter(datum.ctd>=date.min-lubridate::days(30) & datum.ctd<=date.max+lubridate::days(30))
  data[,response]<-imputeTS::na_locf(data[,response])
  
  wave.test.coherence<-analyze.coherency(data,my.pair=response)
  ## Plot of cross-wavelet power
  ## (with color breakpoints according to quantiles):
  wc.image(wave.test.coherence, main = "cross-wavelet power spectrum, x over y",legend.params = list(lab = "cross-wavelet power levels"), periodlab = "period (hours)")
  ## The same plot, now with calendar axis
  ## (according to date format stored in 'my.wc'):
  wc.image(wave.test.coherence, main = "cross-wavelet power spectrum, x over y",legend.params = list(lab = "cross-wavelet power levels"),periodlab = "period (hours)", show.date = TRUE)
  ## Plot of average cross-wavelet power:
  wc.avg(wave.test.coherence, siglvl = 0.05, sigcol = 'red',periodlab = "period (hours)")
  ## Plot of wavelet coherence
  ## (with color breakpoints according to quantiles):
  wc.image(wave.test.coherence, which.image = "wc", main = "wavelet coherence, x over y",legend.params = list(lab = "wavelet coherence levels",lab.line = 3.5, label.digits = 3),periodlab = "period (hours)")
  ## plot of average coherence:
  wc.avg(wave.test.coherence, which.avg = "wc",siglvl = 0.05, sigcol = 'red',legend.coords = "topleft",periodlab = "period (hours)")
  ## Default plot of phase differences
  ## (with contour lines referring to cross-wavelet power)
  wc.phasediff.image(wave.test.coherence, which.contour = "wp",main = "image of phase differences, x over y",periodlab = "period (hours)")
  ## The same plot, but with (automatically produced) calendar axis:
  wc.phasediff.image(wave.test.coherence, which.contour = "wp",main = "image of phase differences, x over y",periodlab = "period (hours)",show.date = TRUE)
  ## Select period 64 and compare plots of corresponding phases, including
  ## the phase differences (angles) in their non-smoothed (default) version:
  wc.sel.phases(wave.test.coherence, sel.period = 300, show.Angle = TRUE, show.date = TRUE)
  ## With time elapsed in days
  ## (starting from 0 and proceeding in steps of 50 days)
  ## instead of the (default) time index:
  index.ticks <- seq(1, nrow(data), by = 12*24)
  index.labels <- (index.ticks-1)/24
  wc.sel.phases(wave.test.coherence, sel.period = 300, show.Angle = TRUE,timelab = "time elapsed (hours)",spec.time.axis = list(at = index.ticks, labels = index.labels))
  ## In the following, no periods are selected.
  ## In this case, instead of individual phases, the plot shows
  ## average phases for each series:
  phase.difference=wc.sel.phases(wave.test.coherence,show.date = TRUE)
  data$Phase.difference<-phase.difference$Angle
  data$ftidal.phase=as.factor(data$tidal.phase)
  data$fcurrent.direction=as.factor(data$current.direction)
  plot(data$fcurrent.direction,data$Phase.difference)
  mean(data$Phase.difference)
  return(data)
}