#combine_debiet
files <- list.files(path="./data/debiet/",pattern = ".csv")
for (i in 1:length(files)){
  path<-paste0("./data/debiet/",files[i])
  file_name<-gsub(".csv","",files[i])
  temp<-read.csv(path,sep=';',skip=8)[,1:2]
  colnames(temp)<-c("date","value")
  temp$loc.debiet<-str_split_1(file_name,"_")[1]
  temp$site.debiet<-str_split_1(file_name,"_")[2]
  temp$debiet<-as.numeric(sub(",", ".", temp$value, fixed = TRUE))
  if (i==1){debiet=temp} else {debiet=rbind(debiet,temp)}
}

debiet$date=substr(gsub("T", " ", as.character(debiet$date)),1,19)
debiet$date_brussels = as.POSIXct(debiet$date,format='%Y-%m-%d %H:%M:%S',tz="Europe/Brussels")
debiet$datum.debiet = debiet$date_brussels; #attr(debiet$datum.debiet, "tzone") <- "GMT"
debiet <- debiet %>% dplyr::select(-date, -date_brussels, -value)

remove(files,path,file_name,temp)