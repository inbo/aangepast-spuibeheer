#combine_ctd
path="./data/ctd/"
jaren<-list.files(path=path)
counter=0
for (i in jaren){
  dir.name1<-paste0(path,i,"/")
  gebied<-list.files(path=dir.name1)
  for (j in gebied){
    dir.name2<-paste0(dir.name1,j,"/")
    dagen<-list.files(path=dir.name2)
    for (k in dagen){
      dir.name3<-paste0(dir.name2,k,"/")
      files<-list.files(path=dir.name3)
      for (l in files){
        dir.name4<-paste0(dir.name3,l)
        counter=counter+1
        if("try-error" %in% class(try(read.csv(dir.name4)))){
          ctd.temp<-read.csv(dir.name4,skip=1)[,c(2:4)]
          if (grepl("mS", colnames(ctd.temp)[2], fixed = TRUE) == FALSE){
            ctd.temp[,2]<-ctd.temp[,2]/1000
          }
          colnames(ctd.temp)<-c("datum.ctd","geleidbaarheid","temperatuur")
          ctd.temp$datum.ctd<-parse_date_time(ctd.temp$datum.ctd, c("mdy HMS p"), tz="Europe/Brussels")
          ctd.temp$sensor<-"small header"
        } else{
          ctd.temp<-read.csv(dir.name4,skip=65,sep=";",header=FALSE)
          ctd.temp<-ctd.temp[-nrow(ctd.temp),]
          colnames(ctd.temp)<-c("datum.ctd","druk","temperatuur","geleidbaarheid")
          ctd.temp<-as.data.frame(lapply(ctd.temp, function(y) gsub(",", ".", y)))
          ctd.temp$datum.ctd<-parse_date_time(ctd.temp$datum.ctd, c("ymd HMS"), tz="Europe/Brussels")
          ctd.temp$sensor<-"large header"
        }
        ctd.temp$site<-j
        ctd.temp$retrieval<-k
        ctd.temp$filename<-tolower(l)
        if (counter==1){ctd=ctd.temp} else{ctd=smartbind(ctd,ctd.temp)}
      }
    }
  }
}
remove(ctd.temp)

ctd$datum.ctd<-as.POSIXct(ctd$datum.ctd,format='%Y-%m-%d %H:%M:%S',tz="Europe/Brussels")
attr(ctd$datum.ctd, "tzone") <- "GMT"

ctd[c("druk","temperatuur","geleidbaarheid")] <- sapply(ctd[c("druk","temperatuur","geleidbaarheid")],as.numeric)
ctd <- ctd %>% distinct()
ctd <- ctd %>% mutate(loc.ctd= case_when(str_detect(filename, 'akl ramskapelle|akl rampskapelle') ~ 'akl ramskapelle',
                                         str_detect(filename, 'lk ramskapelle|lk rampskapelle') ~ 'lk ramskapelle',
                                         str_detect(filename, 'akl moerkerke') ~ 'akl moerkerke',
                                         str_detect(filename, 'sk zeebrugge|schipdonkkanaal_zeebrugge') ~ 'sk zeebrugge',
                                         str_detect(filename, 'blinker moerkerke') ~ 'blinker moerkerke',
                                         str_detect(filename, 'diksmuide') ~ 'diksmuide',
                                         str_detect(filename, 'schoorbakkebrug') ~ 'schoorbakkebrug',
                                         str_detect(filename, 'tervate') ~ 'tervate',
                                         str_detect(filename, 'uniebrug') ~ 'uniebrug',
                                         str_detect(filename, 'yserstar') ~ 'yserstar',
                                         str_detect(filename, 'nieuwpoort-plassendale|nieuwpoort_plassendal') ~ 'nieuwpoort-plassendale',
                                         str_detect(filename, 'brugge') ~ 'brugge',
                                         str_detect(filename, 'plassendale') ~ 'plassendale',
                                         str_detect(filename, 'sas slijkens') ~ 'sas slijkens',
                                         str_detect(filename, 'oude_veurne_vaart') ~ 'oude veurne vaart',
                                         str_detect(filename, 'blauwe sluis') ~ 'blauwe sluis',
                                         str_detect(filename, 'maertensas') ~ 'maertensas',
                                         str_detect(filename, 'clemensheule') ~ 'clemensheule'
))

source("./code/functions/f.seq_date.R")
source("./code/functions/f.grouping_breaking_line.R")
ctd$datum.ctd<-round_date(ctd$datum.ctd, unit="30 mins")
x <- do.call("rbind", by(ctd, ctd$loc.ctd, with, data.frame(loc.ctd = loc.ctd[1], datum.ctd = seq_date(datum.ctd,"30 mins"))))
ctd <- left_join(x,ctd,by=c("loc.ctd","datum.ctd"))
ctd$group_plot <- as.character(grouping_breaking_line(ctd$filename))
ctd <- ctd %>% group_by(loc.ctd) %>% fill(site)
ctd$saliniteit <- ec2pss(ctd$geleidbaarheid,ctd$temperatuur)

link_ctd_debiet_neerslag<-read.csv("./data/link_ctd_debiet_neerslag.csv",sep=";")
ctd<-left_join(ctd,link_ctd_debiet_neerslag,by="loc.ctd")
ctd <- ctd %>% left_join(debiet, join_by(loc.debiet, closest(datum.ctd >= datum.debiet)))
ctd <- ctd %>% left_join(neerslag, join_by(loc.neerslag, closest(datum.ctd >= datum.neerslag)))