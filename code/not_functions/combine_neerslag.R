#combine_neerslag
files <- list.files(path="./data/neerslag/",pattern = ".csv")
for (i in 1:length(files)){
  path<-paste0("./data/neerslag/",files[i])
  file_name<-gsub(".csv","",files[i])
  temp<-read.csv(path,sep=';',skip=7)[,1:2]
  colnames(temp)<-c("date","value")
  temp$loc.neerslag<-str_split_1(file_name,"_")[1]
  temp$site.neerslag<-str_split_1(file_name,"_")[2]
  temp$neerslag<-as.numeric(sub(",", ".", temp$value, fixed = TRUE))
  if (i==1){neerslag=temp} else {neerslag=rbind(neerslag,temp)}
}

neerslag$date=substr(gsub("T", " ", as.character(neerslag$date)),1,19)
neerslag$date_brussels = as.POSIXct(neerslag$date,format='%Y-%m-%d %H:%M:%S',tz="Europe/Brussels")
neerslag$datum.neerslag = neerslag$date_brussels; attr(neerslag$datum.neerslag, "tzone") <- "GMT"
neerslag <- neerslag %>% dplyr::select(-date, -date_brussels, -value, -site.neerslag)
neerslag<-neerslag[!duplicated(neerslag[,c("loc.neerslag","datum.neerslag")]), ]

remove(files,path,file_name,temp)