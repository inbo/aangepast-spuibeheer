#combine_as_data

AKLLK2023 <- clean_AKLLK("./data/spuibeheer/extern/verwerkt_in_excel/os_AKL&LK_2023.xlsx")
AKLLK2024 <- clean_AKLLK("./data/spuibeheer/extern/verwerkt_in_excel/os_AKL&LK_2024.xlsx")

Ijzer2023 <- clean_ijzer_and_va(file = "./data/spuibeheer/extern/verwerkt_in_excel/os_Ijzer_2023.xlsx",
                                skip = 7,
                                datum = 1,
                                tijd = 2,
                                opmerking = 14,
                                site = "Ijzer")
Ijzer2024 <- clean_ijzer_and_va(file = "./data/spuibeheer/extern/verwerkt_in_excel/os_Ijzer_2024.xlsx",
                                skip = 7,
                                datum = 1,
                                tijd = 2,
                                opmerking = 14,
                                site = "Ijzer")
VA2022 <- clean_ijzer_and_va(file = "./data/spuibeheer/extern/verwerkt_in_excel/os_VA_2022.xlsx",
                             skip = 6,
                             datum = 1,
                             tijd = 2,
                             opmerking = 8,
                             site = "VA")
VA2023 <- clean_ijzer_and_va(file = "./data/spuibeheer/extern/verwerkt_in_excel/os_VA_2023.xlsx",
                             skip = 6,
                             datum = 1,
                             tijd = 2,
                             opmerking = 8,
                             site = "VA")
VA2024 <- clean_ijzer_and_va(file = "./data/spuibeheer/extern/verwerkt_in_excel/os_VA_2024.xlsx",
                             skip = 6,
                             datum = 1,
                             tijd = 2,
                             opmerking = 8,
                             site = "VA")

KGO2023 <- clean_KGO("./data/spuibeheer/extern/verwerkt_in_excel/os_KGO_2023.xlsx")
KGO2024 <- clean_KGO("./data/spuibeheer/extern/verwerkt_in_excel/os_KGO_2024.xlsx")

NE <- read.csv("./data/spuibeheer/intern/os_NE_2023_cleaned.csv")[,c(2:4)]

os <- rbind(AKLLK2023, AKLLK2024, Ijzer2023, Ijzer2024, KGO2023, KGO2024, NE, VA2023, VA2024)
remove(AKLLK2023, AKLLK2024, Ijzer2023, Ijzer2024, KGO2023, KGO2024, NE, VA2023, VA2024)

os$dicht[which(os$open > os$dicht)] <- os$dicht[which(os$open > os$dicht)] + lubridate::days(1)
os$duration <- os$dicht - os$open
os$jaar <- lubridate::year(os$open)
os$datum <- as.Date(os$open)
os$glasaal <- NA
os$glasaal[which(os$site == "AKL")] = mean(c(1351, 976, 5093, 935, 82, 1461, 704, 200))
os$glasaal[which(os$site == "LK")] = mean(c(1351, 976, 5093, 935, 82, 1461, 704, 200)) #metingen van AKL gebruikt
os$glasaal[which(os$site == "Ijzer")] = mean(c(1200, 3242)) #tussen min en max
os$glasaal[which(os$site == "NE")] = mean(c(8, 56,31,1,2167,9,26,309,3750,17,3827,21,69,88,2014,40,300,12,39,9,60))
os$glasaal[which(os$site == "KGO")] = 964.2

write.csv(os, "./data/spuibeheer/intern/os_cleaned.csv")
