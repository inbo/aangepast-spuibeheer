clean_KGO <- function(file){
  data <- read_excel(file)
  data$open[which(grepl("[0-9]", data$open) == FALSE)] = NA
  data$dicht[which(grepl("[0-9]", data$dicht) == FALSE)] = NA
  data <- data[-which(is.na(data$open) == TRUE | is.na(data$dicht) == TRUE),]
  data$open <- parse_date_time(paste(data$datum,data$open), c("ymd HM", "ymd H"))
  data$dicht <- parse_date_time(paste(data$datum,data$dicht), c("ymd HM", "ymd H"))
  data$datum <- NULL
  data$site = "KGO"
  data$dicht[which(data$dicht - data$open < 0)] = data$dicht[which(data$dicht - data$open < 0)] + lubridate::days(1)
  return(data)
}