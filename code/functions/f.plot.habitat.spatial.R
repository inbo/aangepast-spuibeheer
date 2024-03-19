f.plot.habitat.spatial<-function(var){
  k=0
  for (j in unique(habitat$sectie)){
    range <- c(max(habitat$X_range),max(habitat$Y_range))
    k=k+1
    habitat.kriging<-habitat[which(is.na(habitat$X)==FALSE & habitat$sectie==j),]
    habitat.kriging <- habitat.kriging %>% mutate(across(where(is.numeric), ~ replace_na(., 0)))
    for (l in unique(habitat.kriging$traject)){
      s=habitat.kriging[which(habitat.kriging$traject==l),]
      print(ggplot(s, aes_string(x="X", y="Y", colour= var)) +
              geom_point(alpha=0.7,size=2) + 
              coord_fixed(ratio = 1) + 
              facet_wrap(~jaar) + 
              ggtitle(paste(j,l)) + 
              scale_colour_gradientn(colours=terrain.colors(10))
            ) 
    }
  }
}