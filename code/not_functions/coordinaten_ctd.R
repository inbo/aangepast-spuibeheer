#coordinaten_ctd
source('./code/functions/f.map.R')
file<-read.csv("./data/metadata/coordinaten_ctd.csv",sep=";")
file[c('X', 'Y')] <- as.numeric(str_split_fixed(file$X..Y, ',', 2))
file$X..Y<-NULL
map<-f.map(file,"Y","X",crs="+init=epsg:4326","site","loc")
f.map(file,"Y","X",crs="+init=epsg:4326","site","loc",plot=TRUE)
