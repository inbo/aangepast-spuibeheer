clean_AKLLK <- function(file){
  AKLLK <- f.read_excel_allsheets(file, skip = 5)
  AKLLK <- AKLLK[[1]]
  AKLLK <- AKLLK[, colSums(is.na(AKLLK)) < nrow(AKLLK)]
  AKL <- AKLLK[, c(1:3)]
  colnames(AKL) = c("datum","open","dicht")
  AKL$site = "AKL"
  LK <- AKLLK[,c(4:6)]
  colnames(LK) = c("datum","open","dicht")
  LK$site = "LK"
  AKLLK <- rbind(AKL,LK)
  AKLLK$open[which(grepl("[0-9]" , AKLLK$open) == FALSE)] = NA
  AKLLK$dicht[which(grepl("[0-9]" , AKLLK$dicht) == FALSE)] = NA
  AKLLK <- AKLLK[-which(is.na(AKLLK$open) == TRUE | is.na(AKLLK$dicht) == TRUE),]
  AKLLK$open <- str_replace(AKLLK$open,"u",":")
  AKLLK$dicht <- str_replace(AKLLK$dicht,"u",":")
  AKLLK$open <- parse_date_time(paste(AKLLK$datum,AKLLK$open), c("ymd HM", "ymd H"), tz="Europe/Brussels")
  attr(AKLLK$open, "tzone") <- "GMT"
  AKLLK$dicht <- parse_date_time(paste(AKLLK$datum,AKLLK$dicht), c("ymd HM", "ymd H"), tz="Europe/Brussels")
  attr(AKLLK$dicht, "tzone") <- "GMT"
  AKLLK$datum <- NULL
  return(AKLLK)
}