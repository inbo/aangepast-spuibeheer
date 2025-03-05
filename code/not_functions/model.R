# model

# lineaire regressie voor verschillende resoluties

model.daily<-stepAIC(lm(spgeleidbaarheid ~ events + duration + distance + debiet + neerslag, data=omgeving.as.daily),trace=FALSE)
summary(model.daily)

model.weekly<-stepAIC(lm(spgeleidbaarheid ~ events + duration + distance + debiet + neerslag, data=omgeving.as.weekly),trace=FALSE)
summary(model.weekly)

model.monthly<-stepAIC(lm(spgeleidbaarheid ~ events + duration + distance + debiet + neerslag, data=omgeving.as.monthly),trace=FALSE)
summary(model.monthly)

model.yearly<-stepAIC(lm(spgeleidbaarheid ~ events + duration + distance + debiet + neerslag, data=omgeving.as.yearly),trace=FALSE)
summary(model.yearly)

# GAMM voor verschillende resoluties

# gamm_model <- gamm(spgeleidbaarheid ~ 
#                      events + 
#                      duration + 
#                      distance + 
#                      debiet +
#                      neerslag +
#                      s(week, bs = "cc", k = 3) +  # Cyclic smooth for seasonality
#                      s(jaar, bs = "tp", k = 3),
#                    random = list(site = ~1),
#                    correlation = corAR1(form = ~ week | site),  # AR(1) structure per site
#                    data = omgeving.as.weekly, method = "REML")


