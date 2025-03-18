#Aangepast spuibeheer verschillende resoluties 

start = "01-01"
stop = "12-31"

daily_os <- os %>%
  mutate(month_day = format(dicht, "%m-%d"),
         datum=as.Date(dicht)) %>%
  dplyr::filter((month_day >= start & month_day <= stop)) %>%
  dplyr::select(site, duration, jaar, datum) %>%
  group_by(site, jaar, datum) %>%
  dplyr::summarize(events = n(),
                   duration=as.numeric(sum(duration))) %>%
  ungroup() 

missing_start_and_stop<-anti_join(unique(os[,c("jaar","site")]) %>%
                                    mutate(start=start,
                                           stop=stop) %>%
                                    pivot_longer(!c(jaar,site),names_to = "type",values_to = "datum") %>%
                                    dplyr::select(-type) %>%
                                    mutate(datum = as.Date(paste0(jaar,"-",datum)),
                                           jaar=as.numeric(jaar),
                                           site=as.character(site)), 
                                  daily_os %>% dplyr::select(site,jaar,datum)) %>%
  mutate(events = 0,
         duration = 0)

daily_os <- rbind(daily_os,missing_start_and_stop) %>%
  group_by(site, jaar) %>%
  complete(datum = seq.Date(min(datum), max(datum), by = "day")) %>%
  replace(is.na(.), 0) %>%
  ungroup()

weekly_os <- daily_os %>%
  group_by(site, jaar) %>%
  mutate(week = lubridate::week(datum)) %>%
  group_by(site, jaar, week) %>%
  summarize(events = sum(events),
            duration = sum(duration))

monthly_os <- daily_os %>%
  group_by(site, jaar) %>%
  mutate(maand = lubridate::month(datum)) %>%
  group_by(site, jaar, maand) %>%
  summarize(events = sum(events),
            duration = sum(duration))

yearly_os <- daily_os %>%
  group_by(site, jaar) %>%
  summarize(events = sum(events),
            duration = sum(duration))

remove(missing_start_and_stop)

# CTD verschillende resoluties

ctd.daily <- ctd %>%
  rename(datum = datum.ctd) %>%
  dplyr::select(site, loc.ctd, datum, spgeleidbaarheid, distance, debiet, neerslag) %>%
  mutate(dag = lubridate::day(datum),
         week = lubridate::week(datum),
         maand = lubridate::month(datum),
         jaar = lubridate::year(datum),
         distance = as.numeric(gsub("km", "", distance)),
         month_day = format(datum, "%m-%d"),
         datum=as.Date(datum)) %>%
  dplyr::filter((month_day >= start & month_day <= stop)) %>%
  group_by(site, loc.ctd, jaar, maand, week, dag, datum)  %>%
  summarise_at(vars(spgeleidbaarheid, distance, debiet, neerslag), 
               funs(ifelse(sum(!is.na(.))/n() >= 0.9, mean(., na.rm = TRUE), NA))) %>%
  ungroup()

ctd.weekly <- ctd.daily %>%
  group_by(site, loc.ctd, jaar, maand, week)  %>%
  summarise_at(vars(spgeleidbaarheid, distance, debiet, neerslag), 
               funs(ifelse(sum(!is.na(.))/n() >= 0.9, mean(., na.rm = TRUE), NA)))

ctd.monthly <- ctd.daily %>%
  group_by(site, loc.ctd, jaar, maand)  %>%
  summarise_at(vars(spgeleidbaarheid, distance, debiet, neerslag), 
               funs(ifelse(sum(!is.na(.))/n() >= 0.9, mean(., na.rm = TRUE), NA)))

ctd.yearly <- ctd.daily %>%
  group_by(site, loc.ctd, jaar)  %>%
  summarise_at(vars(spgeleidbaarheid, distance, debiet, neerslag), 
               funs(ifelse(sum(!is.na(.))/n() >= 0.9, mean(., na.rm = TRUE), NA)))

# Conductiviteit verschillende resoluties

cond.daily <- conductiviteit %>%
  rename(site = site.conductiviteit, loc.ctd = loc.conductiviteit, datum = datum.conductiviteit, spgeleidbaarheid = conductiviteit) %>%
  dplyr::select(site, loc.ctd, datum, spgeleidbaarheid, distance, debiet, neerslag) %>%
  mutate(dag = lubridate::day(datum),
         week = lubridate::week(datum),
         maand = lubridate::month(datum),
         jaar = lubridate::year(datum),
         distance = as.numeric(gsub("km", "", distance)),
         month_day = format(datum, "%m-%d"),
         datum=as.Date(datum)) %>%
  dplyr::filter((month_day >= start & month_day <= stop)) %>%
  group_by(site, loc.ctd, jaar, maand, week, dag, datum)  %>%
  summarise_at(vars(spgeleidbaarheid, distance, debiet, neerslag), 
               funs(ifelse(sum(!is.na(.))/n() >= 0.9, mean(., na.rm = TRUE), NA))) %>%
  ungroup()

cond.weekly <- cond.daily %>%
  group_by(site, loc.ctd, jaar, maand, week)  %>%
  summarise_at(vars(spgeleidbaarheid, distance, debiet, neerslag), 
               funs(ifelse(sum(!is.na(.))/n() >= 0.9, mean(., na.rm = TRUE), NA)))

cond.monthly <- cond.daily %>%
  group_by(site, loc.ctd, jaar, maand)  %>%
  summarise_at(vars(spgeleidbaarheid, distance, debiet, neerslag), 
               funs(ifelse(sum(!is.na(.))/n() >= 0.9, mean(., na.rm = TRUE), NA)))

cond.yearly <- cond.daily %>%
  group_by(site, loc.ctd, jaar)  %>%
  summarise_at(vars(spgeleidbaarheid, distance, debiet, neerslag), 
               funs(ifelse(sum(!is.na(.))/n() >= 0.9, mean(., na.rm = TRUE), NA)))

# Eventueel combineren ctd en conductiviteit

omgeving.daily<-rbind(ctd.daily)

omgeving.weekly<-rbind(ctd.weekly)

omgeving.monthly<-rbind(ctd.monthly)

omgeving.yearly<-rbind(ctd.yearly)

remove(ctd.daily,ctd.weekly,ctd.monthly,ctd.yearly,cond.daily,cond.weekly,cond.monthly,cond.yearly)

# Combineren ctd en as

omgeving.as.daily<-left_join(omgeving.daily,daily_os,by=c("site","jaar","datum")) #%>% mutate_at(vars(-site,-loc.ctd,-jaar,-maand,-week,-dag,-datum), scale)

omgeving.as.weekly<-left_join(omgeving.weekly,weekly_os,by=c("site","jaar","week")) #%>% mutate_at(vars(-site,-loc.ctd,-jaar,-maand,-week), scale)

omgeving.as.monthly<-left_join(omgeving.monthly,monthly_os,by=c("site","jaar","maand")) #%>% mutate_at(vars(-site,-loc.ctd,-jaar,-maand), scale)

omgeving.as.yearly<-left_join(omgeving.yearly,yearly_os,by=c("site","jaar")) #%>% mutate_at(vars(-site,-loc.ctd,-jaar), scale)

remove(omgeving.daily,omgeving.weekly,omgeving.monthly,omgeving.yearly,daily_os,weekly_os,monthly_os,yearly_os)