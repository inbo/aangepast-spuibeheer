clean_ijzer_and_va<-function(file,skip,datum,tijd,opmerking,site){
  data<-f.read_excel_allsheets(file,skip=skip)
  data<-rbindlist(data)
  variable.select<-c(datum,tijd,opmerking)
  data<-data[,..variable.select]
  data<-data[rowSums(is.na(data)) != ncol(data), ]
  colnames(data)<-c("datum","tijd","opmerking")
  data$datum<-zoo::na.locf(data$datum)
  data$tijd <- format(data$tijd,"%H:%M:%S")
  data$datum <- format(data$datum,"%y-%m-%d")
  data$tijd <- parse_date_time(paste(data$datum,data$tijd), c("ymd HMS"), tz="Europe/Brussels")
  attr(data$tijd, "tzone") <- "GMT"
  data$datum<-NULL
  data$opmerking<-tolower((data$opmerking))
  data$opmerking[which(data$opmerking %in% c("omgekeerd spuien",
                                               "omgekeerd spuibeheer",
                                               "omgekeerd spuibeheer opgestart in opdracht van tim wolff",
                                               "en start omgekeerd spuibeheer",
                                               "begin omgekeerd spuibeheer"))]="open"
  data$opmerking[which(data$opmerking %in% c("einde omgekeerd spuien",
                                               "einde os iov dré maes",
                                               "einde omgekeerd spuibeheer",
                                               "einde omgekeerde spuibeheer"))]="dicht"
  data$opmerking[(which(!data$opmerking %in% c("open","dicht")))]=NA
  #data <- data[-which((format(data$tijd,"%H:%M:%S") %in% c("08:00:00","18:00:00")) & (is.na(data$opmerking)==TRUE)),]

  data$opmerking.lag<-dplyr::lag(data$opmerking)
  data$opmerking.lead<-lead(data$opmerking)
  data$opmerking.lag<-zoo::na.locf(data$opmerking.lag,na.rm=FALSE,fromLast = TRUE)
  data$opmerking.lead<-zoo::na.locf(data$opmerking.lead,na.rm=FALSE,fromLast = TRUE)
  data$opmerking[which(data$opmerking=="open" & data$opmerking.lag=="open" & data$opmerking.lead=="open")+1]="dicht"
  
  data$opmerking.lag<-dplyr::lag(data$opmerking)
  data$opmerking.lead<-lead(data$opmerking)
  data$opmerking.lag<-zoo::na.locf(data$opmerking.lag,na.rm=FALSE,fromLast = FALSE)
  data$opmerking.lead<-zoo::na.locf(data$opmerking.lead,na.rm=FALSE,fromLast = FALSE)
  data$opmerking[which(data$opmerking=="dicht" & data$opmerking.lag=="dicht" & data$opmerking.lead=="dicht")-1]="open"
  
  data<-data[-which(is.na(data$opmerking)==TRUE),c("tijd","opmerking")]
  data <- data %>%
    group_by(opmerking) %>%
    mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = opmerking, values_from = tijd) %>%
    dplyr::select(-row)
  data$site=site
  return(data)
}