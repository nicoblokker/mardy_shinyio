library(dplyr)
library(lubridate)

data <- mardyr2:::LRE %>% filter(cdate >= "2015-09-01" & cdate <= "2015-09-07") %>% select(-legacy)
migration_codebook_english <- mardyr2:::migration_codebook_english
data$cdate <- lubridate::ymd(data$cdate)
data$parties[data$parties == "AFD"] <- "AfD"
data$parties <- gsub("Gr.ne", "Green", data$parties)
data$parties[data$parties == "Linke"] <- "Left"
data <- data %>% dplyr::mutate(rowid = paste0("C", 1:nrow(.)))

saveRDS(list(data, migration_codebook_english), "./demo/mardy_data.rds")


