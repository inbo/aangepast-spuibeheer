omgeving.as.daily.train <- omgeving.as.daily %>%
  dplyr::filter(loc.ctd == "yserstar" & jaar >= 2020 & jaar < 2024)

omgeving.as.daily.ts.train <- ts(omgeving.as.daily.train$spgeleidbaarheid, start = c(2020, 1), frequency = 365)

xreg.train <- omgeving.as.daily %>%
  dplyr::filter(loc.ctd == "yserstar" & jaar >= 2020 & jaar < 2024) %>%
  dplyr::select(-datum,-spgeleidbaarheid,-site,-loc.ctd,-jaar,-maand,-week,-dag,-distance,-neerslag) %>%
  as.matrix()

omgeving.as.daily.test <- omgeving.as.daily %>%
  dplyr::filter(loc.ctd == "yserstar" & jaar >= 2024) 

omgeving.as.daily.ts.test <- ts(omgeving.as.daily.test$spgeleidbaarheid, start = c(2024, 1), frequency = 365)

xreg.test <- omgeving.as.daily %>%
  dplyr::filter(loc.ctd == "yserstar" & jaar >= 2024) %>%
  dplyr::select(-datum,-spgeleidbaarheid,-site,-loc.ctd,-jaar,-maand,-week,-dag,-distance,-neerslag) %>%
  as.matrix()

fit <- ARml(y=omgeving.as.daily.ts.train, xreg=xreg.train, max_lag = 5, caret_method = "cubist")

fc <- forecast(fit, h = length(omgeving.as.daily.ts.test), level = c(80, 95, 99), xreg = xreg.test)

get_var_imp(fc)

autoplot(fc)

accuracy(fc, omgeving.as.daily.ts.test)