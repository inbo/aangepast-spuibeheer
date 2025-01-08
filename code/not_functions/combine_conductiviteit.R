#combine_conductiviteit
dir0="./data/conductiviteit/"
sites<-list.files(path=dir0)
counter=0
for (h in sites){
  dir1=paste0(dir0,h,"/")
  files <- list.files(path=dir1,pattern = ".csv")
  for (i in 1:length(files)){
    path<-paste0(dir1,files[i])
    file_name<-gsub(".csv","",files[i])
    temp<-read.csv(path,sep=';',skip=7)[,1:2]
    colnames(temp)<-c("date","value")
    temp$loc.conductiviteit<-str_split_1(file_name,"_")[1]
    temp$site.conductiviteit<-h
    temp$conductiviteit<-as.numeric(sub(",", ".", temp$value, fixed = TRUE))/1000
    if (i==1 & counter==0){conductiviteit=temp} else {conductiviteit=rbind(conductiviteit,temp)}
  }
  counter=counter+1
}

conductiviteit$date=substr(gsub("T", " ", as.character(conductiviteit$date)),1,19)
conductiviteit$date_brussels = as.POSIXct(conductiviteit$date,format='%Y-%m-%d %H:%M:%S',tz="Europe/Brussels")
conductiviteit$datum.conductiviteit = conductiviteit$date_brussels; attr(conductiviteit$datum.conductiviteit, "tzone") <- "GMT"
conductiviteit <- conductiviteit %>% dplyr::select(-date, -date_brussels, -value)
conductiviteit<-conductiviteit[!duplicated(conductiviteit[,c("loc.conductiviteit","datum.conductiviteit")]), ]

source("./code/functions/f.seq_date.R")
source("./code/functions/f.grouping_breaking_line.R")
conductiviteit$datum.conductiviteit<-round_date(conductiviteit$datum.conductiviteit, unit="1 day")
x <- do.call("rbind", by(conductiviteit, conductiviteit$loc.conductiviteit, with, data.frame(loc.conductiviteit = loc.conductiviteit[1], datum.conductiviteit = seq_date(datum.conductiviteit,"1 day"))))
conductiviteit <- left_join(x,conductiviteit,by=c("loc.conductiviteit","datum.conductiviteit"))
conductiviteit$group_plot <- as.character(grouping_breaking_line(conductiviteit$conductiviteit))
conductiviteit <- conductiviteit %>% group_by(loc.conductiviteit) %>% fill(site.conductiviteit)

link_conductiviteit_debiet_neerslag<-read.csv("./data/metadata/link_conductiviteit_debiet_neerslag.csv",sep=";")
conductiviteit<-left_join(conductiviteit,link_conductiviteit_debiet_neerslag,by="loc.conductiviteit")
conductiviteit <- conductiviteit %>% left_join(debiet, join_by(loc.debiet, closest(datum.conductiviteit >= datum.debiet)))
conductiviteit <- conductiviteit %>% left_join(neerslag, join_by(loc.neerslag, closest(datum.conductiviteit >= datum.neerslag)))

remove(files,path,file_name,temp,counter)