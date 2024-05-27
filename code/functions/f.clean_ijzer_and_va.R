clean_ijzer_and_va<-function(file,skip,datum,tijd,opmerking,site){
  Ijzer<-f.read_excel_allsheets(file,skip=skip)
  Ijzer<-rbindlist(Ijzer)
  variable.select<-c(datum,tijd,opmerking)
  Ijzer<-Ijzer[,..variable.select]
  Ijzer<-Ijzer[rowSums(is.na(Ijzer)) != ncol(Ijzer), ]
  colnames(Ijzer)<-c("datum","tijd","opmerking")
  Ijzer$datum<-zoo::na.locf(Ijzer$datum)
  Ijzer$tijd <- format(Ijzer$tijd,"%H:%M:%S")
  Ijzer$datum <- format(Ijzer$datum,"%y-%m-%d")
  Ijzer$tijd <- parse_date_time(paste(Ijzer$datum,Ijzer$tijd), c("ymd HMS"))
  Ijzer$datum<-NULL
  Ijzer$opmerking<-tolower((Ijzer$opmerking))
  Ijzer$opmerking[which(Ijzer$opmerking %in% c("omgekeerd spuien",
                                               "omgekeerd spuibeheer",
                                               "omgekeerd spuibeheer opgestart in opdracht van tim wolff",
                                               "en start omgekeerd spuibeheer"))]="open"
  Ijzer$opmerking[which(Ijzer$opmerking %in% c("einde omgekeerd spuien",
                                               "einde os iov dré maes",
                                               "einde omgekeerd spuibeheer",
                                               "einde omgekeerde spuibeheer"))]="dicht"
  Ijzer$opmerking[(which(!Ijzer$opmerking %in% c("open","dicht")))]=NA
  #Ijzer <- Ijzer[-which((format(Ijzer$tijd,"%H:%M:%S") %in% c("08:00:00","18:00:00")) & (is.na(Ijzer$opmerking)==TRUE)),]
  
  Ijzer$opmerking.lag<-dplyr::lag(Ijzer$opmerking)
  Ijzer$opmerking.lead<-lead(Ijzer$opmerking)
  Ijzer$opmerking.lag<-zoo::na.locf(Ijzer$opmerking.lag,na.rm=FALSE,fromLast = TRUE)
  Ijzer$opmerking.lead<-zoo::na.locf(Ijzer$opmerking.lead,na.rm=FALSE,fromLast = TRUE)
  Ijzer$opmerking[which(Ijzer$opmerking=="open" & Ijzer$opmerking.lag=="open" & Ijzer$opmerking.lead=="open")+1]="dicht"
  
  Ijzer$opmerking.lag<-dplyr::lag(Ijzer$opmerking)
  Ijzer$opmerking.lead<-lead(Ijzer$opmerking)
  Ijzer$opmerking.lag<-zoo::na.locf(Ijzer$opmerking.lag,na.rm=FALSE,fromLast = FALSE)
  Ijzer$opmerking.lead<-zoo::na.locf(Ijzer$opmerking.lead,na.rm=FALSE,fromLast = FALSE)
  Ijzer$opmerking[which(Ijzer$opmerking=="dicht" & Ijzer$opmerking.lag=="dicht" & Ijzer$opmerking.lead=="dicht")-1]="open"
  
  Ijzer<-Ijzer[-which(is.na(Ijzer$opmerking)==TRUE),c("tijd","opmerking")]
  Ijzer <- Ijzer %>%
    group_by(opmerking) %>%
    mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = opmerking, values_from = tijd) %>%
    dplyr::select(-row)
  Ijzer$site=site
  return(Ijzer)
}