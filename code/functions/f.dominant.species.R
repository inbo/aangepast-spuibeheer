f.dominant.species <- function(data,treshold){
  data <- data %>% 
    group_by(sectie,soort,jaar) %>% 
    summarize(aantal=sum(aantal),gewicht=sum(gewicht)) %>%
    ungroup() %>%
    group_by(sectie,jaar) %>%
    mutate(sum_aantal=sum(aantal,na.rm=TRUE),sum_gewicht=sum(gewicht,na.rm=TRUE))
  data$percent_aantal <- data$aantal/data$sum_aantal
  data$percent_gewicht <- data$gewicht/data$sum_gewicht
  
  specieslist.temp <- unique(data$soort[which(data$percent_gewicht>treshold)])
  data$soort[which(!data$soort %in% specieslist.temp)]="rest"
  
  data <- data %>%
    dplyr::group_by(soort,sectie,jaar) %>%
    dplyr::summarise_at(vars(-group_cols(),-sum_aantal,-sum_gewicht),sum,na.rm=TRUE)
  return(data)
}